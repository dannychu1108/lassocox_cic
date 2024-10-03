DROP TABLE IF EXISTS #CIC; -- Ensure the table name is consistent

-- Creating the temp table #CIC with the results - case
SELECT COUNT(*) as keyCount, def.PatientDurableKey -- Count the records for each patient
INTO #CIC -- Storing the results in #CIC
FROM CDW_NEW.deid_uf.DiagnosisTerminologyDim AS dtd -- Using an alias for DiagnosisTerminologyDim
LEFT OUTER JOIN CDW_NEW.deid_uf.DiagnosisEventFact AS def -- Using an alias for DiagnosisEventFact
    ON dtd.DiagnosisKey = def.DiagnosisKey -- Joining on DiagnosisKey
WHERE 
    (
        dtd.l2_name LIKE '%noninfective enteritis and colitis%'  OR dtd.l3_name LIKE  '%noninfective enteritis and colitis%'
    ) 
    AND dtd.Type IN ('ICD-9-CM', 'ICD-10-CM')
    AND EXISTS (
        SELECT 1
        FROM CDW_NEW.deid_uf.MedicationDim AS medDtd
        INNER JOIN CDW_NEW.deid_uf.MedicationAdministrationFact AS medAdmin
            ON medDtd.MedicationKey = medAdmin.PrimaryComponentKey
        WHERE 
            (
                medDtd.PharmaceuticalClass LIKE '%CTLA-4%' 
                OR medDtd.PharmaceuticalClass LIKE '%PD-1%' 
                OR medDtd.PharmaceuticalClass LIKE '%PD-L1%'
            )
            AND medAdmin.PatientDurableKey = def.PatientDurableKey -- Correctly referencing the outer query
    )
GROUP BY def.PatientDurableKey; -- Grouping by PatientDurableKey as required

-- SELECT * FROM #CIC;

/* (A) Diagnosis events table */
DROP TABLE IF EXISTS #diagnosisEvents;
SELECT DISTINCT def.DiagnosisEventKey, -- (Diagnosis) surrogate identifying record
    def.PatientDurableKey,    -- (Diagnosis) unique patient key***
    def.DiagnosisName,        -- (Diagnosis) diagnosis of patient
    def.EncounterKey,         -- (Diagnosis) encounter key value for joining other tables***
    def.DepartmentKey,        -- (Diagnosis) department key values
    def.StartDateKey,         -- (Diagnosis) start date 
    def.EndDateKey,           -- (Diagnosis) end date 
    def.StartDateKeyValue,    -- (Diagnosis) start date in DateTime
    def.EndDateKeyValue,      -- (Diagnosis) end date in DateTime
    def.DepartmentSpecialty,  -- (Diagnosis) department specialty
    def.Type,                 -- (Diagnosis) type of diagnosis
    def.Status,               -- (Diagnosis) diagnosis status (active, deleted, resolved)
    def.PresentOnAdmission,   -- (Diagnosis) the present on admission status
    def.HospitalDiagnosis,    -- (Diagnosis) hospital diagnosis or not
    dtd.l1_name,              -- (Diagnosis) including l1, l2 and l3 name easier for further filtering
    dtd.l2_name,
    dtd.l3_name,
    dtd.Type as typedtd       -- coding type
INTO #diagnosisEvents
FROM CDW_NEW.deid_uf.DiagnosisTerminologyDim AS dtd
LEFT OUTER JOIN CDW_NEW.deid_uf.DiagnosisEventFact AS def
    ON dtd.DiagnosisKey = def.DiagnosisKey
WHERE 
    (
        (dtd.l2_name LIKE '%noninfective enteritis and colitis%'  OR dtd.l3_name LIKE '%noninfective enteritis and colitis%') 
        AND dtd.Type IN ('ICD-9-CM', 'ICD-10-CM')
    )
    AND EXISTS (
        SELECT 1
        FROM #CIC AS cic
        WHERE cic.PatientDurableKey = def.PatientDurableKey
    );

-- SELECT * FROM #diagnosisEvents;

/* (B) Medication events table */
-- Drop the temporary table if it exists
DROP TABLE IF EXISTS #MedEvents;

-- Create a new temporary table with medication events
SELECT DISTINCT
    medAdmin.PatientDurableKey,
    medAdmin.MedicationOrderKey,
    medAdmin.EncounterKey,
    medAdmin.AdministrationDateKeyValue AS AdminDate,
    medAdmin.PrimaryComponentName,
    medAdmin.PrimaryComponentPharmaceuticalClass,
    medAdmin.PrimaryComponentStrength,
    medAdmin.SecondComponentName,
    medAdmin.SecondComponentPharmaceuticalClass,
    medAdmin.SecondComponentStrength,
    medAdmin.MedicationName,
    SUM(TRY_CONVERT(decimal(10,2), medAdmin.Dose)) AS TotalDose,
    medAdmin.DoseUnit,
    MIN(medAdmin.AdministrationDateKeyValue) OVER (PARTITION BY medAdmin.PatientDurableKey, medAdmin.PrimaryComponentName, medAdmin.SecondComponentName) AS FirstAdmin,
    MAX(medAdmin.AdministrationDateKeyValue) OVER (PARTITION BY medAdmin.PatientDurableKey, medAdmin.PrimaryComponentName, medAdmin.SecondComponentName) AS LastAdmin
INTO #MedEvents
FROM CDW_NEW.deid_uf.MedicationAdministrationFact AS medAdmin
WHERE 
    (medAdmin.PrimaryComponentPharmaceuticalClass LIKE '%CTLA-4%' 
     OR medAdmin.PrimaryComponentPharmaceuticalClass LIKE '%PD-1%' 
     OR medAdmin.PrimaryComponentPharmaceuticalClass LIKE '%PD-L1%'
     OR medAdmin.SecondComponentPharmaceuticalClass LIKE '%CTLA-4%' 
     OR medAdmin.SecondComponentPharmaceuticalClass LIKE '%PD-1%'
     OR medAdmin.SecondComponentPharmaceuticalClass LIKE '%PD-L1%'
     )
    AND EXISTS (
        SELECT 1
        FROM #CIC AS cic
        WHERE cic.PatientDurableKey = medAdmin.PatientDurableKey
    )
GROUP BY
    medAdmin.PatientDurableKey,
    medAdmin.MedicationOrderKey,
    medAdmin.EncounterKey,
    medAdmin.AdministrationDateKeyValue,
    medAdmin.PrimaryComponentName,
    medAdmin.PrimaryComponentPharmaceuticalClass,
    medAdmin.PrimaryComponentStrength,
    medAdmin.SecondComponentName,
    medAdmin.SecondComponentPharmaceuticalClass,
    medAdmin.SecondComponentStrength,
    medAdmin.MedicationName,
    medAdmin.DoseUnit;


-- Select data from the newly created table
-- SELECT * FROM #MedEvents;

-- Drop existing table if it exists
DROP TABLE IF EXISTS #Casecohort;

-- Create a new table to store the results
SELECT 
    combined.PatientDurableKey,
    combined.DiagnosisEventKey,
    combined.DiagnosisName,
    combined.DiagnosisDate,
    combined.l3_name,
    combined.MedicationOrderKey,
    combined.PrimaryComponentName,
    combined.SecondComponentName,
    combined.TotalDose,
    combined.DoseUnit,
    combined.AdminDate,
    combined.MedAdminEncounterKey, -- EncounterKey identifying the medication administration (all the med hx of the CIC patient)
    combined.CICDiagnosisEncounterKey AS EncounterKey -- EncounterKey identifying the CIC diagnosis
INTO #Casecohort
FROM (
    SELECT 
        med.PatientDurableKey,
        med.MedicationOrderKey,
        med.PrimaryComponentName,
        med.SecondComponentName,
        med.TotalDose,
        med.DoseUnit,
        med.AdminDate,
        med.EncounterKey AS MedAdminEncounterKey,
        diag.DiagnosisEventKey,
        diag.DiagnosisName,
        diag.StartDateKeyValue AS DiagnosisDate,
        diag.l3_name,
        diag.EncounterKey AS CICDiagnosisEncounterKey,
        ROW_NUMBER() OVER (PARTITION BY med.PatientDurableKey ORDER BY diag.StartDateKeyValue) AS rn
    FROM #MedEvents AS med
    INNER JOIN #diagnosisEvents AS diag
        ON diag.PatientDurableKey = med.PatientDurableKey
        AND diag.StartDateKeyValue > med.AdminDate
    WHERE NOT EXISTS (
        SELECT 1
        FROM #diagnosisEvents AS sub_diag
        WHERE sub_diag.PatientDurableKey = diag.PatientDurableKey
            AND sub_diag.l3_name LIKE '%MALIGNANT NEOPLASM OF DIGESTIVE ORGANS AND PERITONEUM%'
            AND sub_diag.StartDateKeyValue < (
                SELECT MIN(med2.AdminDate)
                FROM #MedEvents AS med2
                WHERE med2.PatientDurableKey = sub_diag.PatientDurableKey
            )
    )
) AS combined
WHERE combined.rn = 1;

-- Select data from the newly created table
--SELECT * FROM #Casecohort;
-- See who needs treatment
DROP TABLE IF EXISTS #SevereCaseCohort;
SELECT DISTINCT
    cc.PatientDurableKey,
    cc.DiagnosisEventKey,
    cc.DiagnosisName,
    cc.DiagnosisDate,
    cc.l3_name,
    cc.MedicationOrderKey AS cic_tx_medorder,
    cc.MedAdminEncounterKey AS TxEncounterKey,
    cc.PrimaryComponentName AS cic_treatment,
    cc.SecondComponentName AS cic_treatment_2comp,
    medAdmin.MedicationOrderKey AS medAdminOrderKey,
    medAdmin.PrimaryComponentPharmaceuticalClass AS cic_tx_class,
    medAdmin.PrimaryComponentPharmaceuticalSubclass AS cic_tx_subclass,
    medAdmin.SecondComponentPharmaceuticalClass AS cic_tx_class_2comp,
    medAdmin.SecondComponentPharmaceuticalSubclass AS cic_tx_subclass_2comp,
    medAdmin.MedicationName AS cic_tx_name,
    SUM(TRY_CONVERT(decimal(10,2), medAdmin.Dose)) AS cic_tx_totaldose,
    medAdmin.DoseUnit AS cic_tx_totaldose_unit,
    medAdmin.AdministrationDateKeyValue AS cic_tx_admin_date,
    dtd.l3_name AS tx_indication
INTO #SevereCaseCohort
FROM #CaseCohort AS cc 
LEFT JOIN CDW_NEW.deid_uf.MedicationAdministrationFact AS medAdmin ON cc.PatientDurableKey = medAdmin.PatientDurableKey AND cc.MedAdminEncounterKey = medAdmin.EncounterKey
LEFT JOIN CDW_NEW.deid_uf.EncounterFact AS ef ON medAdmin.EncounterKey = ef.EncounterKey AND medAdmin.PatientDurableKey = ef.PatientDurableKey
LEFT JOIN CDW_NEW.deid_uf.DiagnosisTerminologyDim AS dtd ON ef.PrimaryDiagnosisKey = dtd.DiagnosisKey
WHERE (((medAdmin.PrimaryComponentPharmaceuticalClass LIKE '%GLUCOCORTICOIDS%' AND medAdmin.MedicationTherapeuticClass LIKE '%HORMONES%')
        OR (medAdmin.SecondComponentPharmaceuticalClass LIKE '%GLUCOCORTICOIDS%' AND medAdmin.MedicationTherapeuticClass LIKE '%HORMONES%')
        OR medAdmin.PrimaryComponentPharmaceuticalSubclass LIKE '%Inflammatory Bowel Agent%' 
        OR medAdmin.SecondComponentPharmaceuticalSubclass LIKE '%Inflammatory Bowel Agent%') AND
        medAdmin.AdministrationDateKeyValue > cc.DiagnosisDate AND (dtd.l2_name LIKE '%noninfective enteritis and colitis%'  OR dtd.l3_name LIKE '%noninfective enteritis and colitis%') 
        AND dtd.Type IN ('ICD-9-CM', 'ICD-10-CM') 
        )
GROUP BY
    cc.PatientDurableKey,
    cc.DiagnosisEventKey,
    cc.DiagnosisName,
    cc.DiagnosisDate,
    cc.l3_name,
    cc.MedicationOrderKey,
    cc.MedAdminEncounterKey,
    cc.PrimaryComponentName,
    cc.SecondComponentName,
    medAdmin.MedicationOrderKey,
    medAdmin.PrimaryComponentPharmaceuticalClass,
    medAdmin.PrimaryComponentPharmaceuticalSubclass,
    medAdmin.SecondComponentPharmaceuticalClass,
    medAdmin.SecondComponentPharmaceuticalSubclass,
    medAdmin.MedicationName,
    medAdmin.DoseUnit,
    medAdmin.AdministrationDateKeyValue,
    dtd.l3_name;


-- See who needs treatment
DROP TABLE IF EXISTS #BiologicsCaseCohort;
SELECT 
    cc.PatientDurableKey,
    medAdmin.EncounterKey AS BiologicsEncounterKey
INTO #BiologicsCaseCohort
FROM #CaseCohort AS cc 
LEFT JOIN CDW_NEW.deid_uf.MedicationAdministrationFact AS medAdmin ON cc.PatientDurableKey = medAdmin.PatientDurableKey
LEFT JOIN [CDW_NEW].[deid_uf].[EncounterFact] AS ef ON medAdmin.EncounterKey = ef.EncounterKey AND medAdmin.PatientDurableKey = ef.PatientDurableKey
LEFT JOIN CDW_NEW.deid_uf.DiagnosisTerminologyDim AS dtd ON ef.PrimaryDiagnosisKey = dtd.DiagnosisKey
WHERE ((
        medAdmin.PrimaryComponentPharmaceuticalSubclass LIKE '%Inflammatory Bowel Agent%') AND
        medAdmin.AdministrationDateKeyValue > cc.DiagnosisDate AND (dtd.l2_name LIKE '%noninfective enteritis and colitis%'  OR dtd.l3_name LIKE '%noninfective enteritis and colitis%') 
        AND dtd.Type IN ('ICD-9-CM', 'ICD-10-CM') 
        );

/* Create a temporary table to hold the earliest administration dates for patients */
DROP TABLE IF EXISTS #EarliestAdmin;
SELECT 
    PatientDurableKey,
    MIN(AdministrationDateKeyValue) AS FirstAdminDate
INTO #EarliestAdmin
FROM CDW_NEW.deid_uf.MedicationAdministrationFact
WHERE (PrimaryComponentPharmaceuticalClass LIKE '%CTLA-4%' 
        OR PrimaryComponentPharmaceuticalClass LIKE '%PD-1%' 
        OR PrimaryComponentPharmaceuticalClass LIKE '%PD-L1%')
GROUP BY PatientDurableKey;

-- First, calculate the maximum AdministrationDateKeyValue for each patient if needed
DROP TABLE IF EXISTS #MaxAdminDate;
SELECT 
    PatientDurableKey, 
    MAX(AdministrationDateKeyValue) AS MaxAdminDateKeyValue
INTO #MaxAdminDate
FROM CDW_NEW.deid_uf.MedicationAdministrationFact
WHERE (PrimaryComponentPharmaceuticalClass LIKE '%CTLA-4%' 
        OR PrimaryComponentPharmaceuticalClass LIKE '%PD-1%' 
        OR PrimaryComponentPharmaceuticalClass LIKE '%PD-L1%')
GROUP BY PatientDurableKey;

-- Drop existing temp table if exists
DROP TABLE IF EXISTS #DisqualifiedPatients;

-- Create temporary table to hold disqualified patients
SELECT DISTINCT
    def.PatientDurableKey
INTO #DisqualifiedPatients
FROM CDW_NEW.deid_uf.DiagnosisEventFact AS def
JOIN CDW_NEW.deid_uf.DiagnosisTerminologyDim AS dtd ON def.DiagnosisKey = dtd.DiagnosisKey
LEFT JOIN #EarliestAdmin AS ea ON def.PatientDurableKey = ea.PatientDurableKey
LEFT JOIN #MaxAdminDate AS mad ON def.PatientDurableKey = mad.PatientDurableKey
LEFT JOIN [CDW_NEW].[deid_uf].[PregnancyFact] AS pf ON def.PatientDurableKey = pf.PatientDurableKey
WHERE
    (
        (dtd.l3_name LIKE '%MALIGNANT NEOPLASM OF DIGESTIVE ORGANS AND PERITONEUM%'
        OR dtd.l3_name LIKE '%noninfective enteritis and colitis%')
        AND def.StartDateKeyValue < ea.FirstAdminDate
    )
    OR
    (
        ((pf.PregnancyKey IS NOT NULL) OR dtd.l2_name LIKE '%pregnan%')
        AND (
            pf.EpisodeStartDateKeyValue BETWEEN ea.FirstAdminDate AND mad.MaxAdminDateKeyValue
            OR pf.EpisodeEndDateKeyValue BETWEEN ea.FirstAdminDate AND mad.MaxAdminDateKeyValue
            OR def.StartDateKeyValue BETWEEN ea.FirstAdminDate AND mad.MaxAdminDateKeyValue
            OR def.EndDateKeyValue BETWEEN ea.FirstAdminDate AND mad.MaxAdminDateKeyValue
        )
    );

-- Index the disqualified patients for faster access
CREATE INDEX IDX_DisqualifiedPatients ON #DisqualifiedPatients (PatientDurableKey);

-- Drop existing temporary table if exists
DROP TABLE IF EXISTS #Earliest_n_Latest_Admin;

-- Create a new temporary table for earliest administration dates using window function
SELECT DISTINCT
    medAdmin.PatientDurableKey,
    medAdmin.MedicationOrderKey AS medAdminOrderKey,
    medAdmin.EncounterKey,
    medAdmin.MedicationName,
    medAdmin.PrimaryComponentName,
    medAdmin.PrimaryComponentPharmaceuticalClass,
    medAdmin.PrimaryComponentStrength,
    medAdmin.PrimaryComponentPharmaceuticalSubclass,
    medAdmin.SecondComponentName,
    medAdmin.SecondComponentPharmaceuticalClass,
    medAdmin.SecondComponentStrength,
    medAdmin.AdministrationDateKeyValue,
    medAdmin.MedicationGenericName,
    MIN(medAdmin.AdministrationDateKeyValue) OVER (
        PARTITION BY medAdmin.PatientDurableKey,    
        medAdmin.PrimaryComponentName,
        medAdmin.SecondComponentName
    ) AS FirstAdminDate,
    MAX(medAdmin.AdministrationDateKeyValue) OVER (
        PARTITION BY medAdmin.PatientDurableKey,    
        medAdmin.PrimaryComponentName,
        medAdmin.SecondComponentName
    ) AS LastAdminDate,
    SUM(TRY_CONVERT(decimal(10,2), medAdmin.Dose)) AS medAdminTotalDose,
    medAdmin.DoseUnit AS medAdminTotalDoseUnit
INTO #Earliest_n_Latest_Admin
FROM CDW_NEW.deid_uf.MedicationAdministrationFact AS medAdmin
WHERE EXISTS (
    SELECT 1
    FROM CDW_NEW.deid_uf.MedicationAdministrationFact AS subMed
    WHERE subMed.PatientDurableKey = medAdmin.PatientDurableKey
    AND (subMed.PrimaryComponentPharmaceuticalClass LIKE '%CTLA-4%' 
        OR subMed.PrimaryComponentPharmaceuticalClass LIKE '%PD-1%' 
        OR subMed.PrimaryComponentPharmaceuticalClass LIKE '%PD-L1%'
        OR subMed.SecondComponentPharmaceuticalClass LIKE '%CTLA-4%'
        OR subMed.SecondComponentPharmaceuticalClass LIKE '%PD-1%'
        OR subMed.SecondComponentPharmaceuticalClass LIKE '%PD-L1%'
        )
) AND (medAdmin.PrimaryComponentPharmaceuticalClass LIKE '%CTLA-4%'
OR medAdmin.PrimaryComponentPharmaceuticalClass LIKE '%PD-1%'
OR medAdmin.PrimaryComponentPharmaceuticalClass LIKE '%PD-L1%'
OR medAdmin.PrimaryComponentPharmaceuticalClass LIKE '%PROTON PUMP INHIBITORS%'
OR medAdmin.PrimaryComponentPharmaceuticalClass LIKE '%NSAID%'
OR medAdmin.PrimaryComponentPharmaceuticalClass LIKE '%GLUCOCORTICOIDS%'
OR medAdmin.PrimaryComponentPharmaceuticalSubclass LIKE '%inflammatory bowel agent%'
OR medAdmin.PrimaryComponentTherapeuticClass LIKE '%Antineoplastics'
)
GROUP BY
    medAdmin.PatientDurableKey,
    medAdmin.MedicationOrderKey,
    medAdmin.EncounterKey,
    medAdmin.MedicationName,
    medAdmin.PrimaryComponentName,
    medAdmin.PrimaryComponentPharmaceuticalClass,
    medAdmin.PrimaryComponentStrength,
    medAdmin.PrimaryComponentPharmaceuticalSubclass,
    medAdmin.SecondComponentName,
    medAdmin.SecondComponentPharmaceuticalClass,
    medAdmin.SecondComponentStrength,
    medAdmin.MedicationGenericName,
    medAdmin.DoseUnit,
    medAdmin.AdministrationDateKeyValue;

-- View the results
-- SELECT * FROM #Earliest_n_Latest_Admin;

DROP TABLE IF EXISTS #ltf;

SELECT DISTINCT
    lab.PatientDurableKey,
    lab.EncounterKey,
    lab.ProcedureName AS TestName,
    lab.CollectedDateKeyValue AS TestDate,
    lab.IsAbnormal AS IsAbNormalTestResult
INTO #ltf
FROM [CDW_NEW].[deid_uf].[LabTestFact] lab
WHERE (lab.ProcedureName LIKE '%vitamin D%')
AND lab.PatientDurableKey IN (SELECT PatientDurableKey FROM #Earliest_n_Latest_Admin);

-- Drop existing cohort table if exists
DROP TABLE IF EXISTS #FilteredData;

-- Define CTEs for pre-filtering data
WITH FilteredVisitFact AS (
    SELECT *
    FROM [CDW_NEW].[deid_uf].[VisitFact]
    WHERE PatientDurableKey IN (SELECT PatientDurableKey FROM #Earliest_n_Latest_Admin)
),
FilteredEncounterFact AS (
    SELECT *
    FROM [CDW_NEW].[deid_uf].[EncounterFact]
    WHERE PatientDurableKey IN (SELECT PatientDurableKey FROM #Earliest_n_Latest_Admin)
)

-- SELECT * FROM FilteredEncounterFact
-- Create cohort table excluding disqualified patients
-- Creating intermediate #FilteredData table
SELECT DISTINCT
       pdd.PatientDurableKey,
    pdd.Sex,
    pdd.BirthDate,
    ef.DateKeyValue AS EncounterDate,
    vf.CheckInDateKeyValue,
    vf.CheckOutDateKeyValue,
    vf.EncounterDateKeyValue,
    vf.AgeKeyValue,
    vf.BodyMassIndex,
    vf.HeightInInches,
    vf.WeightInOunces,
    vf.VisitType,
    pdd.StartDate,
    pdd.EndDate,
    pdd.PreferredLanguage,
    pdd.Ethnicity,
    pdd.FirstRace,
    pdd.SecondRace,
    pdd.MultiRacial,
    pdd.DeathDate,
    pdd.[Status],
    pdd.StateOrProvince,
    pdd.Country,
    pdd.PostalCode,
    pdd.MaritalStatus,
    pdd.Religion,
    pdd.SmokingStatus,
    pdd.PrimaryFinancialClass,
    pdd.HighestLevelOfEducation,
    pdd.DeathInstant,
    pdd.PreliminaryCauseOfDeathDiagnosisKey,
    pdd.SexAssignedAtBirth,
    pdd.GenderIdentity,
    pdd.SexualOrientation,
    pdd.UCSFDerivedRaceEthnicity_X,
    ef.[PrimaryDiagnosisKey] AS EncounterPriDiagKey,
    ef.[PrimaryDiagnosisName] AS EncounterPriDiagName,
    ela.medAdminOrderKey,
    ef.EncounterKey,
    ela.MedicationName,
    ela.medAdminTotalDose,
    ela.medAdminTotalDoseUnit,
    ela.PrimaryComponentName,
    ela.PrimaryComponentPharmaceuticalClass,
    ela.PrimaryComponentPharmaceuticalSubClass,
    ela.PrimaryComponentStrength,
    ela.SecondComponentName,
    ela.SecondComponentPharmaceuticalClass,
    ela.SecondComponentStrength,
    ela.AdministrationDateKeyValue,
    ela.MedicationGenericName,
    ela.FirstAdminDate,
    ela.LastAdminDate,
    dtd.[Type] AS ICD_type_encounter_asso, -- dtd here can help identify the indication of the drug
    dtd.[Value] AS ICD_code_encounter_asso,
    dtd.l1_name AS encounter_asso_l1,
    dtd.l2_name AS encounter_asso_l2,
    dtd.l3_name AS encounter_asso_l3
INTO #FilteredData
FROM FilteredVisitFact vf
FULL JOIN FilteredEncounterFact ef ON vf.EncounterKey = ef.EncounterKey AND vf.PatientDurableKey = ef.PatientDurableKey
LEFT JOIN #Earliest_n_Latest_Admin ela ON vf.PatientDurableKey = ela.PatientDurableKey AND vf.EncounterKey = ela.EncounterKey
LEFT JOIN [CDW_NEW].[deid_uf].[PatDurableDim] pdd ON vf.PatientDurableKey = pdd.PatientDurableKey
LEFT JOIN [CDW_NEW].[deid_uf].[DiagnosisTerminologyDim] dtd ON ef.PrimaryDiagnosisKey = dtd.DiagnosisKey
WHERE vf.PatientDurableKey NOT IN (SELECT PatientDurableKey FROM #DisqualifiedPatients) AND dtd.[Type] IN ('ICD-9-CM', 'ICD-10-CM');


DROP TABLE IF EXISTS #TotalCohort;
-- Joining other details and applying conditions
SELECT DISTINCT
    fd.*,
    cc.DiagnosisName AS cic_diag,
    cc.DiagnosisDate AS cic_diag_date,
    cc.l3_name AS cic_l3_name,
    scc.cic_treatment,
    scc.cic_tx_class,
    scc.cic_tx_subclass,
    scc.cic_tx_admin_date,
    scc.tx_indication,
    bcc.BiologicsEncounterKey,
    def.DiagnosisName AS DiagnosisNameInEncounterEvent, -- diagnosis here indicates whether the encounter is a diagnosis, and see through what diagnosis is that
    def.DiagnosisKey AS DiagnosisKeyInEncounterEvent,
    def.StartDateKeyValue AS DiagnosisDateInEncounterEvent,
    dtd2.l1_name AS DiagnosisEncounterL1Name,
    dtd2.l2_name AS DiagnosisEncounterL2Name,
    dtd2.l3_name AS DiagnosisEncounterL3Name,
    dtd2.[Value] AS ICD_diagencounter,
    ltf.TestName,
    ltf.TestDate,
    ltf.IsAbNormalTestResult
INTO #TotalCohort
FROM #FilteredData AS fd
LEFT JOIN [CDW_NEW].[deid_uf].DiagnosisEventFact def ON fd.EncounterKey = def.EncounterKey
LEFT JOIN #ltf AS ltf ON fd.EncounterKey = ltf.EncounterKey
LEFT JOIN [CDW_NEW].[deid_uf].DiagnosisTerminologyDim AS dtd2 ON def.DiagnosisKey = dtd2.DiagnosisKey
LEFT JOIN #CaseCohort AS cc ON (fd.EncounterKey = cc.[EncounterKey])
LEFT JOIN #SevereCaseCohort AS scc ON (fd.EncounterKey = scc.[TxEncounterKey]) AND fd.PatientDurableKey = scc.PatientDurableKey AND fd.medAdminOrderKey = scc.cic_tx_medorder
LEFT JOIN #BiologicsCaseCohort AS bcc ON (fd.EncounterKey = bcc.[BiologicsEncounterKey])
WHERE dtd2.[Type] IN ('ICD-9-CM', 'ICD-10-CM');

-- Select data from the newly created table
--SELECT DISTINCT * FROM #TotalCohort;

DROP TABLE IF EXISTS #TotalCohort2;

-- Create a CTE to handle procedure data filtering separately
WITH ProcedureData AS (
    SELECT
        PatientDurableKey,
        ProcedureName,
        ProcedureStartDateKeyValue,
        EarliestDocumentedDateKeyValue,
        ROW_NUMBER() OVER (PARTITION BY PatientDurableKey ORDER BY EarliestDocumentedDateKeyValue) AS Rank
    FROM [CDW_NEW].[deid_uf].[ProcedureEventFact]
    WHERE ProcedureName LIKE '%appendectomy%'
),
FilteredProcedures AS (
    SELECT
        PatientDurableKey,
        ProcedureName,
        ProcedureStartDateKeyValue,
        EarliestDocumentedDateKeyValue
    FROM ProcedureData
    WHERE Rank = 1 -- Only select the earliest record for each PatientDurableKey
)

SELECT DISTINCT
    tc.*,
    csf.[BodySiteOrCancerType],
    csf.[StageGroup],
    csf.[StageGroupGeneral],
    csf.[AjccStageGroup],
    csf.[StageDateKeyValue],
    fp.ProcedureName,
    fp.ProcedureStartDateKeyValue,
    fp.EarliestDocumentedDateKeyValue,
    CASE 
        WHEN (tc.DiagnosisNameInEncounterEvent LIKE '%rash%' OR
             tc.DiagnosisNameInEncounterEvent LIKE '%hypothyrodism%' OR
             tc.DiagnosisNameInEncounterEvent LIKE '%pneumonitis%' OR
             tc.DiagnosisNameInEncounterEvent LIKE '%nephritis%' OR
             tc.DiagnosisNameInEncounterEvent LIKE '%hypophysitis%' OR
             tc.DiagnosisNameInEncounterEvent LIKE '%Adrenal Insufficiency%' OR
             tc.DiagnosisNameInEncounterEvent LIKE '%Hypophysitis%' OR
             tc.DiagnosisNameInEncounterEvent LIKE '%type 1 diabetes%' OR
             tc.DiagnosisNameInEncounterEvent LIKE '%Myalgia%' OR 
             tc.DiagnosisNameInEncounterEvent LIKE '%encephalopathy%'
            ) AND tc.DiagnosisDateInEncounterEvent >= tc.LastAdminDate THEN 1
        ELSE 0
    END AS NonGIirAE_stopped
INTO #TotalCohort2
FROM #TotalCohort AS tc
LEFT JOIN [CDW_NEW].[deid_uf].[CancerStagingFact] AS csf 
    ON tc.EncounterKey = csf.EncounterKey AND tc.PatientDurableKey = csf.PatientDurableKey
LEFT JOIN FilteredProcedures AS fp 
    ON tc.PatientDurableKey = fp.PatientDurableKey;

-- Now you can select from #TotalCohort2 to see your final results
-- SELECT * FROM #TotalCohort2;


DROP TABLE IF EXISTS #TotalCohort3;
SELECT DISTINCT *
INTO #TotalCohort3
FROM #TotalCohort2
WHERE 
    TestName IS NOT NULL OR
    medAdminOrderKey IS NOT NULL OR
    (DiagnosisKeyInEncounterEvent IS NOT NULL AND
     (
        DiagnosisNameInEncounterEvent LIKE '%Primary sclerosing cholangitis%' OR
        DiagnosisNameInEncounterEvent LIKE '%Pyoderma gangrenosum%' OR
        DiagnosisNameInEncounterEvent LIKE '%Autoimmune hepatitis%' OR
        DiagnosisNameInEncounterEvent LIKE '%Celiac disease%' OR
        DiagnosisNameInEncounterEvent LIKE '%Ankylosing spondylitis%' OR
        DiagnosisNameInEncounterEvent LIKE '%Churg Strauss syndrome%' OR
        DiagnosisNameInEncounterEvent LIKE '%Primary biliary cholangitis%' OR
        DiagnosisNameInEncounterEvent LIKE '%Episcleritis%' OR
        DiagnosisNameInEncounterEvent LIKE '%Iridocyclitis%' OR
        DiagnosisNameInEncounterEvent LIKE '%Atrophic gastritis%' OR
        DiagnosisNameInEncounterEvent LIKE '%Psoriasis%' OR
        DiagnosisNameInEncounterEvent LIKE '%Polyarteritis nodosa%' OR
        DiagnosisNameInEncounterEvent LIKE '%Rheumatoid arthritis%' OR
        DiagnosisNameInEncounterEvent LIKE '%Type 1 diabetes%' OR
        DiagnosisNameInEncounterEvent LIKE '%Sarcoidosis%' OR
        DiagnosisNameInEncounterEvent LIKE '%Asthma%' OR
        DiagnosisNameInEncounterEvent LIKE '%Giant cell arteritis%' OR
        DiagnosisNameInEncounterEvent LIKE '%Psoriatic arthritis%' OR
        DiagnosisNameInEncounterEvent LIKE '%Grave%' OR
        DiagnosisNameInEncounterEvent LIKE '%Polymyalgia rheumatica%' OR
        DiagnosisNameInEncounterEvent LIKE '%intestine%' OR
        DiagnosisNameInEncounterEvent LIKE '%digestive system%' OR
        DiagnosisNameInEncounterEvent LIKE '%malignancy%' OR
        DiagnosisNameInEncounterEvent LIKE '%neoplasm%' OR
        DiagnosisNameInEncounterEvent LIKE '%cancer%' OR
        DiagnosisNameInEncounterEvent LIKE '%tumor%' OR
        DiagnosisNameInEncounterEvent LIKE '%stool%' OR
        DiagnosisNameInEncounterEvent LIKE '%colitis%' OR
        (
        (
        DiagnosisNameInEncounterEvent LIKE '%rash%' OR
        DiagnosisNameInEncounterEvent LIKE '%hypothyrodism%' OR
        DiagnosisNameInEncounterEvent LIKE '%pneumonitis%' OR
        DiagnosisNameInEncounterEvent LIKE '%nephritis%' OR
        DiagnosisNameInEncounterEvent LIKE '%hypophysitis%' OR
        DiagnosisNameInEncounterEvent LIKE '%Adrenal Insufficiency%' OR
        DiagnosisNameInEncounterEvent LIKE '%Hypophysitis%' OR
        DiagnosisNameInEncounterEvent LIKE '%type 1 diabetes%' OR
        DiagnosisNameInEncounterEvent LIKE '%Myalgia%' OR 
        DiagnosisNameInEncounterEvent LIKE '%encephalopathy%'
        ) AND DiagnosisDateInEncounterEvent >= LastAdminDate
        ) OR
        (
        DiagnosisEncounterL1Name LIKE '%Primary sclerosing cholangitis%' OR
        DiagnosisEncounterL1Name LIKE '%Pyoderma gangrenosum%' OR
        DiagnosisEncounterL1Name LIKE '%Autoimmune hepatitis%' OR
        DiagnosisEncounterL1Name LIKE '%Celiac disease%' OR
        DiagnosisEncounterL1Name LIKE '%Ankylosing spondylitis%' OR
        DiagnosisEncounterL1Name LIKE '%Churg Strauss syndrome%' OR
        DiagnosisEncounterL1Name LIKE '%Primary biliary cholangitis%' OR
        DiagnosisEncounterL1Name LIKE '%Episcleritis%' OR
        DiagnosisEncounterL1Name LIKE '%Iridocyclitis%' OR
        DiagnosisEncounterL1Name LIKE '%Atrophic gastritis%' OR
        DiagnosisEncounterL1Name LIKE '%Psoriasis%' OR
        DiagnosisEncounterL1Name LIKE '%Polyarteritis nodosa%' OR
        DiagnosisEncounterL1Name LIKE '%Rheumatoid arthritis%' OR
        DiagnosisEncounterL1Name LIKE '%Type 1 diabetes%' OR
        DiagnosisEncounterL1Name LIKE '%Sarcoidosis%' OR
        DiagnosisEncounterL1Name LIKE '%Asthma%' OR
        DiagnosisEncounterL1Name LIKE '%Giant cell arteritis%' OR
        DiagnosisEncounterL1Name LIKE '%Psoriatic arthritis%' OR
        DiagnosisEncounterL1Name LIKE '%Grave%' OR
        DiagnosisEncounterL1Name LIKE '%Polymyalgia rheumatica%' OR
        DiagnosisEncounterL1Name LIKE '%intestine%' OR
        DiagnosisEncounterL1Name LIKE '%digestive system%' OR
        DiagnosisEncounterL1Name LIKE '%malignancy%' OR
        DiagnosisEncounterL1Name LIKE '%neoplasm%' OR
        DiagnosisEncounterL1Name LIKE '%cancer%' OR
        DiagnosisEncounterL1Name LIKE '%tumor%' OR
        DiagnosisEncounterL1Name LIKE '%stool%' OR
        DiagnosisEncounterL1Name LIKE '%colitis%' OR 
                (
        (
        DiagnosisEncounterL1Name LIKE '%rash%' OR
        DiagnosisEncounterL1Name LIKE '%hypothyrodism%' OR
        DiagnosisEncounterL1Name LIKE '%pneumonitis%' OR
        DiagnosisEncounterL1Name LIKE '%nephritis%' OR
        DiagnosisEncounterL1Name LIKE '%hypophysitis%' OR
        DiagnosisEncounterL1Name LIKE '%Adrenal Insufficiency%' OR
        DiagnosisEncounterL1Name LIKE '%Hypophysitis%' OR
        DiagnosisEncounterL1Name LIKE '%type 1 diabetes%' OR
        DiagnosisEncounterL1Name LIKE '%Myalgia%' OR 
        DiagnosisEncounterL1Name LIKE '%encephalopathy%'
        ) AND DiagnosisDateInEncounterEvent >= LastAdminDate
        )
        )
        OR
        (
        DiagnosisEncounterL2Name LIKE '%Primary sclerosing cholangitis%' OR
        DiagnosisEncounterL2Name LIKE '%Pyoderma gangrenosum%' OR
        DiagnosisEncounterL2Name LIKE '%Autoimmune hepatitis%' OR
        DiagnosisEncounterL2Name LIKE '%Celiac disease%' OR
        DiagnosisEncounterL2Name LIKE '%Ankylosing spondylitis%' OR
        DiagnosisEncounterL2Name LIKE '%Churg Strauss syndrome%' OR
        DiagnosisEncounterL2Name LIKE '%Primary biliary cholangitis%' OR
        DiagnosisEncounterL2Name LIKE '%Episcleritis%' OR
        DiagnosisEncounterL2Name LIKE '%Iridocyclitis%' OR
        DiagnosisEncounterL2Name LIKE '%Atrophic gastritis%' OR
        DiagnosisEncounterL2Name LIKE '%Psoriasis%' OR
        DiagnosisEncounterL2Name LIKE '%Polyarteritis nodosa%' OR
        DiagnosisEncounterL2Name LIKE '%Rheumatoid arthritis%' OR
        DiagnosisEncounterL2Name LIKE '%Type 1 diabetes%' OR
        DiagnosisEncounterL2Name LIKE '%Sarcoidosis%' OR
        DiagnosisEncounterL2Name LIKE '%Asthma%' OR
        DiagnosisEncounterL2Name LIKE '%Giant cell arteritis%' OR
        DiagnosisEncounterL2Name LIKE '%Psoriatic arthritis%' OR
        DiagnosisEncounterL2Name LIKE '%Graves disease%' OR
        DiagnosisEncounterL2Name LIKE '%Polymyalgia rheumatica%' OR
        DiagnosisEncounterL2Name LIKE '%intestine%' OR
        DiagnosisEncounterL2Name LIKE '%digestive system%' OR
        DiagnosisEncounterL2Name LIKE '%malignancy%' OR
        DiagnosisEncounterL2Name LIKE '%neoplasm%' OR
        DiagnosisEncounterL2Name LIKE '%cancer%' OR
        DiagnosisEncounterL2Name LIKE '%tumor%' OR
        DiagnosisEncounterL2Name LIKE '%stool%' OR
        DiagnosisEncounterL2Name LIKE '%colitis%' OR 
        (
                    (
        DiagnosisEncounterL2Name LIKE '%rash%' OR
        DiagnosisEncounterL2Name LIKE '%hypothyrodism%' OR
        DiagnosisEncounterL2Name LIKE '%pneumonitis%' OR
        DiagnosisEncounterL2Name LIKE '%nephritis%' OR
        DiagnosisEncounterL2Name LIKE '%hypophysitis%' OR
        DiagnosisEncounterL2Name LIKE '%Adrenal Insufficiency%' OR
        DiagnosisEncounterL2Name LIKE '%Hypophysitis%' OR
        DiagnosisEncounterL2Name LIKE '%type 1 diabetes%' OR
        DiagnosisEncounterL2Name LIKE '%Myalgia%' OR 
        DiagnosisEncounterL2Name LIKE '%encephalopathy%'
        ) AND DiagnosisDateInEncounterEvent >= LastAdminDate
        )
        )
        OR
        (
        DiagnosisEncounterL3Name LIKE '%Primary sclerosing cholangitis%' OR
        DiagnosisEncounterL3Name LIKE '%Pyoderma gangrenosum%' OR
        DiagnosisEncounterL3Name LIKE '%Autoimmune hepatitis%' OR
        DiagnosisEncounterL3Name LIKE '%Celiac disease%' OR
        DiagnosisEncounterL3Name LIKE '%Ankylosing spondylitis%' OR
        DiagnosisEncounterL3Name LIKE '%Churg Strauss syndrome%' OR
        DiagnosisEncounterL3Name LIKE '%Primary biliary cholangitis%' OR
        DiagnosisEncounterL3Name LIKE '%Episcleritis%' OR
        DiagnosisEncounterL3Name LIKE '%Iridocyclitis%' OR
        DiagnosisEncounterL3Name LIKE '%Atrophic gastritis%' OR
        DiagnosisEncounterL3Name LIKE '%Psoriasis%' OR
        DiagnosisEncounterL3Name LIKE '%Polyarteritis nodosa%' OR
        DiagnosisEncounterL3Name LIKE '%Rheumatoid arthritis%' OR
        DiagnosisEncounterL3Name LIKE '%Type 1 diabetes%' OR
        DiagnosisEncounterL3Name LIKE '%Sarcoidosis%' OR
        DiagnosisEncounterL3Name LIKE '%Asthma%' OR
        DiagnosisEncounterL3Name LIKE '%Giant cell arteritis%' OR
        DiagnosisEncounterL3Name LIKE '%Psoriatic arthritis%' OR
        DiagnosisEncounterL3Name LIKE '%Grave%' OR
        DiagnosisEncounterL3Name LIKE '%Polymyalgia rheumatica%' OR
        DiagnosisEncounterL3Name LIKE '%intestine%' OR
        DiagnosisEncounterL3Name LIKE '%digestive system%' OR
        DiagnosisEncounterL3Name LIKE '%malignancy%' OR
        DiagnosisEncounterL3Name LIKE '%neoplasm%' OR
        DiagnosisEncounterL3Name LIKE '%cancer%' OR
        DiagnosisEncounterL3Name LIKE '%tumor%' OR
        DiagnosisEncounterL3Name LIKE '%stool%' OR
        DiagnosisEncounterL3Name LIKE '%colitis%' OR 
                (
        (
        DiagnosisEncounterL3Name LIKE '%rash%' OR
        DiagnosisEncounterL3Name LIKE '%hypothyrodism%' OR
        DiagnosisEncounterL3Name LIKE '%pneumonitis%' OR
        DiagnosisEncounterL3Name LIKE '%nephritis%' OR
        DiagnosisEncounterL3Name LIKE '%hypophysitis%' OR
        DiagnosisEncounterL3Name LIKE '%Adrenal Insufficiency%' OR
        DiagnosisEncounterL3Name LIKE '%Hypophysitis%' OR
        DiagnosisEncounterL3Name LIKE '%type 1 diabetes%' OR
        DiagnosisEncounterL3Name LIKE '%Myalgia%' OR 
        DiagnosisEncounterL3Name LIKE '%encephalopathy%'
        ) AND DiagnosisDateInEncounterEvent >= LastAdminDate
        )
        )
    )
    ) OR
    StageGroup IS NOT NULL OR
    (BodyMassIndex IS NOT NULL OR HeightInInches IS NOT NULL OR WeightInOunces IS NOT NULL) OR
    ProcedureName LIKE '%Appendectomy%';

-- First, defining a CTE to filter and process initial conditions
WITH FilteredCohort2 AS (
    SELECT *,
        ROW_NUMBER() OVER (PARTITION BY DiagnosisNameInEncounterEvent, DiagnosisEncounterL1Name, DiagnosisEncounterL2Name, DiagnosisEncounterL3Name 
                           ORDER BY DiagnosisDateInEncounterEvent) AS EarliestBeforeAdmin,
        ROW_NUMBER() OVER (PARTITION BY DiagnosisNameInEncounterEvent, DiagnosisEncounterL1Name, DiagnosisEncounterL2Name, DiagnosisEncounterL3Name 
                           ORDER BY DiagnosisDateInEncounterEvent) AS EarliestAfterAdmin
    FROM #TotalCohort3
    WHERE 
        (StageGroup IS NULL AND 
        BodyMassIndex IS NULL AND 
        HeightInInches IS NULL AND 
        WeightInOunces IS NULL AND 
        ProcedureName NOT LIKE '%Appendectomy%' AND
        TestName IS NULL AND
        medAdminOrderKey IS NULL) AND 
        (
            DiagnosisDateInEncounterEvent < FirstAdminDate OR
            DiagnosisDateInEncounterEvent >= FirstAdminDate
        )
)

-- Now, select the distinct entries based on the initial criteria and additional filters
SELECT DISTINCT *
FROM FilteredCohort2
WHERE 
    EarliestBeforeAdmin = 1 OR
    EarliestAfterAdmin = 1 OR
    StageGroup IS NOT NULL OR
    BodyMassIndex IS NOT NULL OR 
    HeightInInches IS NOT NULL OR 
    WeightInOunces IS NOT NULL OR
    ProcedureName LIKE '%Appendectomy%' OR
    TestName IS NOT NULL OR
    medAdminOrderKey IS NOT NULL;


