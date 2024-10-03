setwd("/Users/dchu4/Library/Mobile Documents/com~apple~CloudDocs/Documents/Clinical and Epi Research Year 1/Research - IBD/Table exported")

Medication <- read.csv("Medication.csv")
Diagnosis <- read.csv("Diagnosis and Cancer stage.csv")
TestResult <- read.csv("TestResult.csv")
Appendectomy <- read.csv("Appendectomy.csv")
PsyDiagnosis <- read.csv("PsyDiagnosis.csv")
Demographics <- read.csv("Demographics.csv")
Visits <- read.csv("Visits.csv")

library(dplyr)
library(lubridate)
library(stringr)

#### Diagnosis Table Cleaning ####

# Function to check prefixes for any prefix-based code detection
check_prefix <- function(value, prefixes) {
  any(sapply(prefixes, function(prefix) startsWith(value, prefix)))
}

# Step 1: Filter patients with GI tumors (ICD-10: C15-C26, ICD-9: 150-159)
GI_tumor_codes_icd10_prefixes <- c("C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26")
GI_tumor_codes_icd9_prefixes <- c("150", "151", "152", "153", "154", "155", "156", "157", "158", "159")

# Add a new unified 'EventDate' column to each data frame
Diagnosis <- Diagnosis %>%
  mutate(EventDate = StartDateKeyValue)

filtered_Diagnosis <- Diagnosis %>%
  group_by(DiagnosisEventKey, PatientDurableKey, EncounterKey, DiagnosisKey) %>%
  # Arrange by type and date within each group
  arrange(typedtd != "ICD-10-CM", StartDateKeyValue) %>%
  # Get the first row from each group after arranging
  slice(1) %>%
  ungroup()  # Optionally ungroup the data frame after manipulation

# Identify GI tumor patients
GI_tumor_patients <- filtered_Diagnosis %>%
  filter(
    (grepl(paste0("^", GI_tumor_codes_icd10_prefixes, collapse = "|"), Value) & typedtd == "ICD-10-CM" ) |
      (grepl(paste0("^", GI_tumor_codes_icd9_prefixes, collapse = "|"), Value) & typedtd == "ICD-9-CM" )
  )

# Step 3: Add pregnancy diagnosis column
pregnancy_codes_icd9_prefixes <- as.character(630:679)
pregnancy_codes_icd10_prefixes <- c("Z32", "Z33", "Z34", "Z35", "Z36", "Z37", "Z38", "Z39", "Z3A")

# Update filtered_Diagnosis with the new "immunodisorder" column
filtered_Diagnosis <- filtered_Diagnosis %>%
  mutate(immunodisorder = if_else(
    DiagnosisName %in% c('Primary sclerosing cholangitis', 'Pyoderma gangrenosum', 'Autoimmune hepatitis', 'Celiac disease', 
                         'Ankylosing spondylitis', 'Churg Strauss syndrome', 'Primary biliary cholangitis', 'Episcleritis', 
                         'Iridocyclitis', 'Atrophic gastritis', 'Psoriasis', 'Polyarteritis nodosa', 'Rheumatoid arthritis', 
                         'Type 1 diabetes', 'Sarcoidosis', 'Asthma', 'Giant cell arteritis', 'Psoriatic arthritis', 'Grave', 
                         'Polymyalgia rheumatica') |
      l1_name %in% c('Primary sclerosing cholangitis', 'Pyoderma gangrenosum', 'Autoimmune hepatitis', 'Celiac disease', 
                     'Ankylosing spondylitis', 'Churg Strauss syndrome', 'Primary biliary cholangitis', 'Episcleritis', 
                     'Iridocyclitis', 'Atrophic gastritis', 'Psoriasis', 'Polyarteritis nodosa', 'Rheumatoid arthritis', 
                     'Type 1 diabetes', 'Sarcoidosis', 'Asthma', 'Giant cell arteritis', 'Psoriatic arthritis', 'Grave', 
                     'Polymyalgia rheumatica') |
      l2_name %in% c('Primary sclerosing cholangitis', 'Pyoderma gangrenosum', 'Autoimmune hepatitis', 'Celiac disease', 
                     'Ankylosing spondylitis', 'Churg Strauss syndrome', 'Primary biliary cholangitis', 'Episcleritis', 
                     'Iridocyclitis', 'Atrophic gastritis', 'Psoriasis', 'Polyarteritis nodosa', 'Rheumatoid arthritis', 
                     'Type 1 diabetes', 'Sarcoidosis', 'Asthma', 'Giant cell arteritis', 'Psoriatic arthritis', 'Grave', 
                     'Polymyalgia rheumatica') |
      l3_name %in% c('Primary sclerosing cholangitis', 'Pyoderma gangrenosum', 'Autoimmune hepatitis', 'Celiac disease', 
                     'Ankylosing spondylitis', 'Churg Strauss syndrome', 'Primary biliary cholangitis', 'Episcleritis', 
                     'Iridocyclitis', 'Atrophic gastritis', 'Psoriasis', 'Polyarteritis nodosa', 'Rheumatoid arthritis', 
                     'Type 1 diabetes', 'Sarcoidosis', 'Asthma', 'Giant cell arteritis', 'Psoriatic arthritis', 'Grave', 
                     'Polymyalgia rheumatica'), 
    1, 0),
    pregnancy = if_else(
      (typedtd == "ICD-9-CM" & check_prefix(Value, pregnancy_codes_icd9_prefixes)) |
        (typedtd == "ICD-10-CM" & check_prefix(Value, pregnancy_codes_icd10_prefixes)),
      1, 0)
  )



# Update filtered_Diagnosis with a new column for GI disorder history
filtered_Diagnosis <- filtered_Diagnosis %>%
  mutate(
    gord = if_else(
      (str_starts(Value, "530.1") | str_starts(Value, "530.2") | str_starts(Value, "530.3") | str_starts(Value, "530.8") | str_starts(Value, "K21")),
      1, 0
    ),
    ulcer = if_else(
      (str_starts(Value, "531") | str_starts(Value, "532") | str_starts(Value, "533") | str_starts(Value, "534") |
         str_starts(Value, "K25") | str_starts(Value, "K26") | str_starts(Value, "K27") | str_starts(Value, "K28")),
      1, 0
    ),
    ibs = if_else(
      (str_starts(Value, "564.1") | str_starts(Value, "K58")),
      1, 0
    ),
    other_gi_disorder = if_else(
      # Check for codes starting with 'K', but exclude those for gord or ulcer
      ((str_starts(Value, "K") & !str_starts(Value, "K50") & !str_starts(Value, "K51") & !str_starts(Value, "K52")) |
         # Check for codes starting with '5', excluding relevant codes for gord or ibs
         (str_starts(Value, "5") & !str_starts(Value, "555") & !str_starts(Value, "556") & !str_starts(Value, "557") & !str_starts(Value, "558"))) &
        # Ensure not categorized as gord, ulcer, or ibs
        !(gord == 1 | ulcer == 1 | ibs == 1),
      1, 0
    )
  )


# Include IBD (either drug induced or pre-existing)
filtered_Diagnosis <- filtered_Diagnosis %>%
  mutate(colitis = if_else(
    (str_starts(Value, "K50") | str_starts(Value, "K51") | str_starts(Value, "K52") | str_starts(Value, "555") | str_starts(Value, "556") | str_starts(Value, "557") | str_starts(Value, "558")),
    1, 0
  ))
# Pattern for ICD-10 codes
icd10_pattern_hnn <- "^C(0[0-9]|1[0-3]|30|31|32|33|76[.]0|76[.]1)$" #hnn: head and neck

# Pattern for ICD-9 codes
icd9_pattern_hnn <- "^(140|141|142|143|144|145|146|147|148|160|161|162[.]0|195[.]0|195[.]1)$"

# Include cancer - head and neck cancer, melanoma, NSCLC and RCC
filtered_Diagnosis <- filtered_Diagnosis %>%
  mutate(melanoma = if_else(
    (str_starts(Value, "C43") | str_starts(Value, "172")),
    1, 0
  ),
  other_skin_tumour = if_else(
    (str_starts(Value, "C44") | str_starts(Value, "173")),
    1, 0
  ),
  nsclc = if_else(
    (str_starts(Value, "C34") | str_starts(Value, "162")) & 
      (str_detect(tolower(DiagnosisName), regex('non-small|nsclc', ignore_case = TRUE))),
    1, 0
  ),
  other_lung_tumour = if_else(
    (str_starts(Value, "C34") | str_starts(Value, "162")) & 
      !(str_detect(tolower(DiagnosisName), regex('non-small|nsclc', ignore_case = TRUE))),
    1, 0
  ),
  rcc = if_else(
    (str_starts(Value, "C64") | (str_starts(Value, "189"))),
    1, 0
  ),
  head_n_neck_cancer = if_else(
    str_detect(Value, icd10_pattern_hnn) | str_detect(Value, icd9_pattern_hnn),
    1, 0
  ),
  non_gi_irae_susp = if_else(
    (str_detect(tolower(DiagnosisName), regex('rash|hypothyrodism|penumonitis|nephritis|hypophysitis|adernal insuffi|type 1 diabetes|myalgia|encephalopathy', ignore_case = TRUE))),
    1,0
  )
  )

# Step 1: Add flags for the presence of cancer in any record per patient
filtered_Diagnosis <- filtered_Diagnosis %>%
  group_by(PatientDurableKey) %>%
  mutate(any_target_cancer = if_else(any(melanoma == 1 | other_skin_tumour == 1 | nsclc == 1 | other_lung_tumour == 1 | rcc == 1 | head_n_neck_cancer == 1), 1, 0)) %>%
  ungroup()

# Step 2: Filter to keep all records for patients with any cancer flag
filtered_Diagnosis <- filtered_Diagnosis %>%
  filter(any_target_cancer == 1)

################### HERE TO SEE WHAT CANCER LEFT 

# Filter Diagnosis data to include only records where l1_name or l2_name is "neoplasms"
diagnosis_neoplasms <- Diagnosis %>%
  filter(str_detect(l1_name, regex("neoplasms", ignore_case = TRUE)) |
           str_detect(l2_name, regex("neoplasms", ignore_case = TRUE)))

# Group by DiagnosisName and count unique PatientDurableKey entries
patient_count_per_cancer <- diagnosis_neoplasms %>%
  group_by(DiagnosisName) %>%
  summarise(patient_count = n_distinct(PatientDurableKey)) %>%
  arrange(desc(patient_count))

# Display the result
print("Number of unique patients per cancer type (with neoplasms):")
print(patient_count_per_cancer)



################### 

library(tidyr)

# 'EventDate' is a column representing the date of the event/diagnosis
filtered_Diagnosis <- filtered_Diagnosis %>%
  arrange(PatientDurableKey, EventDate) %>% # Ensure the data is ordered by patient and date
  group_by(PatientDurableKey) %>%
  mutate(
    gord = if_else(gord == 1, 1, 0),
    ulcer = if_else(ulcer == 1, 1, 0),
    ibs = if_else(ibs == 1, 1, 0),
    other_gi_disorder = if_else(other_gi_disorder == 1, 1, 0),
    colitis = if_else(colitis == 1, 1, 0),
    melanoma = if_else(melanoma == 1, 1, 0),
    other_skin_tumour = if_else(other_skin_tumour == 1, 1, 0),
    nsclc = if_else(nsclc == 1, 1, 0),
    other_lung_tumour = if_else(other_lung_tumour == 1, 1, 0),
    rcc = if_else(rcc == 1, 1, 0),
    head_n_neck_cancer = if_else(head_n_neck_cancer == 1, 1, 0),
    non_gi_irae_susp = if_else(non_gi_irae_susp == 1, 1, 0)
  ) %>%
  # Apply cumulative max to carry forward any '1' once it appears
  mutate(
    gord = cummax(gord),
    ulcer = cummax(ulcer),
    ibs = cummax(ibs),
    other_gi_disorder = cummax(other_gi_disorder),
    colitis = cummax(colitis),
    melanoma = cummax(melanoma),
    other_skin_tumour = cummax(other_skin_tumour),
    nsclc = cummax(nsclc),
    other_lung_tumour = cummax(other_lung_tumour),
    rcc = cummax(rcc),
    head_n_neck_cancer = cummax(head_n_neck_cancer),
    non_gi_irae_susp = cummax(non_gi_irae_susp)
  ) %>%
  ungroup()

filtered_Diagnosis  <- filtered_Diagnosis  %>%
  filter(!PatientDurableKey %in% GI_tumor_patients$PatientDurableKey)

# Count unique PatientDurableKey entries
unique_patient_count <- filtered_Diagnosis %>%
  distinct(PatientDurableKey) %>%
  nrow()

# Print the result
cat("Number of unique PatientDurableKey entries:", unique_patient_count, "\n")

# Convert EventDate to a Date object if not already
filtered_Diagnosis$EventDate <- as.Date(filtered_Diagnosis$EventDate)

diagnosis_to_combine <- filtered_Diagnosis %>%
  dplyr::select(PatientDurableKey, EventDate, immunodisorder, gord, ulcer, ibs, other_gi_disorder, colitis, melanoma, other_skin_tumour, nsclc, other_lung_tumour, rcc, head_n_neck_cancer ,non_gi_irae_susp, pregnancy) %>%
  distinct()
# I don't make it to month here because in case there's some diagnosis happen less than a month before drug administration that indicates it's not because of the drug

#### Medication Table Cleaning ####
Medication <- Medication %>%
  mutate(EventDate = AdminDate)

Medication_filtered <- Medication %>%
  filter(PatientDurableKey %in% diagnosis_to_combine$PatientDurableKey)

Medication_filtered <- Medication_filtered %>%
  mutate(
    PD1 = if_else(
      str_detect(tolower(PrimaryComponentPharmaceuticalClass), "pd-1") |
        str_detect(tolower(SecondComponentPharmaceuticalClass), "pd-1"), 
      1, 0
    ),
    PDL1 = if_else(
      str_detect(tolower(PrimaryComponentPharmaceuticalClass), "pd-l1") |
        str_detect(tolower(SecondComponentPharmaceuticalClass), "pd-l1"), 
      1, 0
    ),
    CTLA4 = if_else(
      str_detect(tolower(PrimaryComponentPharmaceuticalClass), "ctla-4") |
        str_detect(tolower(SecondComponentPharmaceuticalClass), "ctla-4"), 
      1, 0
    )
  )
Medication_filtered <- Medication_filtered %>%
  mutate(
    Durvalumab = if_else(
      str_detect(tolower(PrimaryComponentName), "durvalumab") |
        str_detect(tolower(SecondComponentName), "durvalumab") |
        str_detect(tolower(MedicationName), "durvalumab"),
      1, 0
    ),
    Atezolizumab = if_else(
      str_detect(tolower(PrimaryComponentName), "atezolizumab") |
        str_detect(tolower(SecondComponentName), "atezolizumab") |
        str_detect(tolower(MedicationName), "atezolizumab"),
      1, 0
    ),
    Pembrolizumab = if_else(
      str_detect(tolower(PrimaryComponentName), "pembrolizumab") |
        str_detect(tolower(SecondComponentName), "pembrolizumab") |
        str_detect(tolower(MedicationName), "pembrolizumab"),
      1, 0
    ),
    Nivolumab = if_else(
      str_detect(tolower(PrimaryComponentName), "nivolumab") |
        str_detect(tolower(SecondComponentName), "nivolumab") |
        str_detect(tolower(MedicationName), "nivolumab"),
      1, 0
    ),
    Ipilimumab = if_else(
      str_detect(tolower(PrimaryComponentName), "ipilimumab") |
        str_detect(tolower(SecondComponentName), "ipilimumab") |
        str_detect(tolower(MedicationName), "ipilimumab"),
      1, 0
    ),
    Cemiplimab = if_else(
      str_detect(tolower(PrimaryComponentName), "cemiplimab") |
        str_detect(tolower(SecondComponentName), "cemiplimab") |
        str_detect(tolower(MedicationName), "cemiplimab"),
      1, 0
    ),
    Vibostolimab_Pembrolizumab = if_else(
      str_detect(tolower(PrimaryComponentName), "vibostolimab-pembrolizumab") |
        str_detect(tolower(SecondComponentName), "vibostolimab-pembrolizumab") |
        str_detect(tolower(MedicationName), "vibostolimab-pembrolizumab"),
      1, 0
    )
  )


Medication_filtered <- Medication_filtered %>%
  mutate(
    corticosteroids = if_else(
      (str_detect(tolower(PrimaryComponentPharmaceuticalClass), "glucocorticoids") | 
         str_detect(tolower(SecondComponentPharmaceuticalClass), "glucocorticoids")) &
        str_detect(tolower(MedicationTherapeuticClass), "hormones"),
      1, 0
    ),
    ibd_biologics = if_else(
      str_detect(tolower(PrimaryComponentPharmaceuticalSubclass), "inflammatory bowel agent") |
        str_detect(tolower(SecondComponentPharmaceuticalSubclass), "inflammatory bowel agent"),
      1, 0
    ),
    ppi = if_else(
      str_detect(tolower(PrimaryComponentPharmaceuticalClass), "proton pump inhibitors") |
        str_detect(tolower(SecondComponentPharmaceuticalClass), "proton pump inhibitors"),
      1, 0
    ),
    nsaid = if_else(
      str_detect(tolower(PrimaryComponentPharmaceuticalClass), "nsaid") |
        str_detect(tolower(SecondComponentPharmaceuticalClass), "nsaid"),
      1, 0
    ),
    other_cancer_tx = if_else((
      str_detect(tolower(PrimaryComponentPharmaceuticalClass), "antineoplastics") |
        str_detect(tolower(SecondComponentPharmaceuticalClass), "antineoplastics")) & PD1 == 0 & PDL1 == 0 & CTLA4 == 0,
      1, 0
    )
  )

Medication_filtered <- Medication_filtered %>%
  mutate(
    antibiotics = if_else(
      str_detect(tolower(PrimaryComponentPharmaceuticalClass), "antibiotics") | 
        str_detect(tolower(SecondComponentPharmaceuticalClass), "antibiotics") |
        str_detect(tolower(MedicationTherapeuticClass), "antibiotics"),
      1, 0
    )
  )

# Filter Medication_filtered to keep rows with corticosteroids and relevant medical indications
steroid_need_to_keep <- Medication_filtered %>%
  filter(corticosteroids == 1) %>%
  filter(
    (str_detect(tolower(MedIndicationL3), "digestive|colitis") |
       str_detect(tolower(MedIndicationL2), "digestive|colitis") |
       str_detect(tolower(MedIndicationL1), "digestive|colitis") |
       str_detect(tolower(MedIndicationName), "digestive|colitis")) &
      !str_detect(tolower(MedIndicationL3), "malignant|bleeding from mouth|nausea|oral pain") &
      !str_detect(tolower(MedIndicationL2), "malignant|bleeding from mouth|nausea|oral pain") &
      !str_detect(tolower(MedIndicationL1), "malignant|bleeding from mouth|nausea|oral pain") &
      !str_detect(tolower(MedIndicationName), "malignant|bleeding from mouth|nausea|oral pain")
  )

Medication_filtered <- Medication_filtered %>%
  filter(corticosteroids == 0 | (corticosteroids == 1 & 
                                   ((MedIndicationL3 %in% steroid_need_to_keep$MedIndicationL3 & 
                                       MedIndicationL2 %in% steroid_need_to_keep$MedIndicationL2 & 
                                       MedIndicationL1 %in% steroid_need_to_keep$MedIndicationL1 & 
                                       MedIndicationName %in% steroid_need_to_keep$MedIndicationName) |
                                      (!MedIndicationL3 %in% steroid_need_to_keep$MedIndicationL3 & 
                                         !MedIndicationL2 %in% steroid_need_to_keep$MedIndicationL2 & 
                                         !MedIndicationL1 %in% steroid_need_to_keep$MedIndicationL1 & 
                                         !MedIndicationName %in% steroid_need_to_keep$MedIndicationName)
                                   )))

# Calculate the accumulated number of doses of checkpoint inhibitors based on pharmaceutical classes
Medication_filtered <- Medication_filtered %>%
  mutate(
    is_pd1 = if_else(
      str_detect(tolower(PrimaryComponentPharmaceuticalClass), "pd-1") |
        str_detect(tolower(SecondComponentPharmaceuticalClass), "pd-1"),
      1, 0
    ),
    is_pdl1 = if_else(
      str_detect(tolower(PrimaryComponentPharmaceuticalClass), "pd-l1") |
        str_detect(tolower(SecondComponentPharmaceuticalClass), "pd-l1"),
      1, 0
    ),
    is_ctla4 = if_else(
      str_detect(tolower(PrimaryComponentPharmaceuticalClass), "ctla-4") |
        str_detect(tolower(SecondComponentPharmaceuticalClass), "ctla-4"),
      1, 0
    )
  ) %>%
  group_by(PatientDurableKey) %>%
  arrange(PatientDurableKey, AdminDate) %>%
  mutate(
    accumulated_pd1_doses = cumsum(is_pd1),
    accumulated_pdl1_doses = cumsum(is_pdl1),
    accumulated_ctla4_doses = cumsum(is_ctla4),
    accumulated_durvalumab_doses = cumsum(Durvalumab),
    accumulated_atezolizumab_doses = cumsum(Atezolizumab),
    accumulated_pembrolizumab_doses = cumsum(Pembrolizumab),
    accumulated_nivolumab_doses = cumsum(Nivolumab),
    accumulated_ipilimumab_doses = cumsum(Ipilimumab),
    accumulated_cemiplimab_doses = cumsum(Cemiplimab),
    accumulated_vibostolimab_pembrolizumab_doses = cumsum(Vibostolimab_Pembrolizumab)
  ) %>%
  ungroup() %>%
  dplyr::select(-is_pd1, -is_pdl1, -is_ctla4)
################# FOR CHECKING ####################
# Filter rows with PD-1, PD-L1, or CTLA-4 components and count PatientDurableKey per indication
indication_patient_count <- Medication%>%
  filter(
    str_detect(tolower(PrimaryComponentPharmaceuticalClass), "pd-1|pd-l1|ctla-4") |
      str_detect(tolower(SecondComponentPharmaceuticalClass), "pd-1|pd-l1|ctla-4")
  ) %>%
  group_by(MedIndicationName) %>%
  summarise(patient_count = n_distinct(PatientDurableKey)) %>%
  arrange(desc(patient_count))
################# FOR CHECKING ####################


# Display the number of unique patients per indication
print("Number of unique patients per indication for PD-1, PD-L1, or CTLA-4 components:")
print(indication_patient_count)

medication_to_combine <- Medication_filtered %>%
  dplyr::select(PatientDurableKey, 31:ncol(Medication_filtered))

#### Test Result Table Cleaning #### 
TestResult <- TestResult %>%
  mutate(EventDate = TestRepDateKeyValue)

filtered_TR <- TestResult %>%
  filter(PatientDurableKey %in% diagnosis_to_combine$PatientDurableKey)

filtered_TR <- filtered_TR %>%
  filter(str_detect(tolower(ProcedureName), "vitamin d")) %>%
  mutate(VitD_Value = Value)

extract_limits <- function(ref_values) {
  if (grepl("normal", tolower(ref_values))) {
    # Special case where ReferenceValues is marked as "normal"
    return(c(NA, NA, "normal"))
  } else {
    # Extract numeric values from "Low: xx High: xx"
    low <- as.numeric(stringr::str_extract(ref_values, "(?<=Low: )\\d+"))
    high <- as.numeric(stringr::str_extract(ref_values, "(?<=High: )\\d+"))
    return(c(low, high, NA))
  }
}

# Main data transformation
TR_to_combine <- filtered_TR %>%
  # Extract low, high limits, and check for "normal"
  rowwise() %>%
  mutate(
    # Determine if ReferenceValues explicitly states "normal"
    normal_flag = grepl("normal", tolower(ReferenceValues)),
    # Extract numeric low and high limits using regex patterns
    low = as.numeric(str_extract(ReferenceValues, "(?<=Low: )\\d+")),
    high = as.numeric(str_extract(ReferenceValues, "(?<=High: )\\d+")),
    # Convert VitD_Value to numeric
    VitD_Value = as.numeric(VitD_Value)
  ) %>%
  ungroup() %>%
  # Update Abnormal values according to specified rules
  mutate(
    Abnormal = if_else(
      (is.na(Abnormal) | Abnormal == "NULL") &
        (
          normal_flag == TRUE |
            (!is.na(VitD_Value) & !is.na(low) & !is.na(high) & VitD_Value >= low & VitD_Value <= high)
        ),
      "0",
      Abnormal
    ),
    VitD_Abnormal = as.numeric(Abnormal),  # Convert to numeric for consistency
    # Identify if VitD_Value is too high or too low
    VitD_too_high = if_else(!is.na(high) & VitD_Value > high, 1, 0),
    VitD_too_low = if_else(!is.na(low) & VitD_Value < low, 1, 0),
    VitD_Unit = Unit
  ) %>%
  dplyr::select(PatientDurableKey, ProcedureName, EventDate, VitD_Value, VitD_Unit, VitD_Abnormal, VitD_too_high, VitD_too_low)

#### Appendectomy Table Cleaning ####
Appendectomy <- Appendectomy %>%
  mutate(EventDate = AppendectomyStartDate, appendectomy = 1)

appendectomy_to_combine <- Appendectomy %>% dplyr::select(PatientDurableKey, EventDate, appendectomy)

# Convert the EncounterKey column in each data frame to character
TestResult$EncounterKey <- as.character(TestResult$EncounterKey)
Appendectomy$EncounterKey <- as.character(Appendectomy$EncounterKey)

#### Psy Diagnosis Cleaning ####
filtered_PsyDiagnosis <- PsyDiagnosis %>%
  group_by(DiagnosisEventKey, PatientDurableKey, EncounterKey, DiagnosisKey) %>%
  # Arrange by type and date within each group
  arrange(typedtd != "ICD-10-CM", StartDateKeyValue) %>%
  # Get the first row from each group after arranging
  slice(1) %>%
  ungroup()  # Optionally ungroup the data frame after manipulation


filtered_PsyDiagnosis <- filtered_PsyDiagnosis %>%
  filter(PatientDurableKey %in% diagnosis_to_combine$PatientDurableKey) %>%
  mutate(
    EventDate = StartDateKeyValue,
    # Initialize each condition to 0
    depression = 0,
    bipolar = 0,
    other_mood_disorder = 0,
    anxiety = 0,
    schizophrenia_related_psychosis = 0,
    other_psy = 0
  ) %>%
  mutate(
    # Conditions for depression
    depression = if_else(
      (typedtd == "ICD-9-CM" & grepl("^296[.]2|^296[.]3|^296[.]8", Value)) |
        (typedtd == "ICD-10-CM" & grepl("^F32|^F33", Value)),
      1, depression
    ),
    # Conditions for bipolar
    bipolar = if_else(
      (typedtd == "ICD-9-CM" & grepl("^296[.]5|^296[.]6", Value)) |
        (typedtd == "ICD-10-CM" & grepl("^F31", Value)),
      1, bipolar
    ),
    # Conditions for other mood disorder
    other_mood_disorder = if_else(
      (typedtd == "ICD-9-CM" & grepl("^296", Value) & !grepl("^296[.]2|^296[.]3|^296[.]8|^296[.]5|^296[.]6", Value)) |
        (typedtd == "ICD-10-CM" & grepl("^F3", Value) & !grepl("^F31|^F32|^F33", Value)),
      1, other_mood_disorder
    ),
    # Conditions for anxiety
    anxiety = if_else(
      (typedtd == "ICD-10-CM" & grepl("^F4", Value)) |
        (typedtd == "ICD-9-CM" & grepl("^300", Value)),
      1, anxiety
    ),
    # Conditions for schizophrenia-related psychosis
    schizophrenia_related_psychosis = if_else(
      (typedtd == "ICD-10-CM" & grepl("^F2", Value)) |
        (typedtd == "ICD-9-CM" & grepl("^295|^297|^298", Value)),
      1, schizophrenia_related_psychosis
    )
  ) %>%
  # Identify rows not meeting any of the above conditions as other_psy
  mutate(
    other_psy = if_else(
      depression == 0 & bipolar == 0 & other_mood_disorder == 0 & anxiety == 0 & schizophrenia_related_psychosis == 0,
      1, other_psy
    )
  ) %>%
  # Apply cumulative maximum to propagate diagnosis over time
  group_by(PatientDurableKey) %>%
  arrange(EventDate) %>%
  mutate(
    depression = cummax(depression),
    bipolar = cummax(bipolar),
    other_mood_disorder = cummax(other_mood_disorder),
    anxiety = cummax(anxiety),
    schizophrenia_related_psychosis = cummax(schizophrenia_related_psychosis),
    other_psy = cummax(other_psy)
  ) %>%
  ungroup()

psy_to_combine <- filtered_PsyDiagnosis %>% dplyr::select(PatientDurableKey, EventDate, depression,
                                                   bipolar,
                                                   other_mood_disorder,
                                                   anxiety,
                                                   schizophrenia_related_psychosis,
                                                   other_psy) %>% distinct()

#### Demographics ####
filtered_dem <- Demographics %>% filter(PatientDurableKey %in% diagnosis_to_combine$PatientDurableKey) %>%
  mutate(
    sex = if_else(Sex == "Female", 0, if_else(Sex == "Male", 1, NA)),
    sex_at_birth = if_else(SexAssignedAtBirth == "Female", 0, if_else(SexAssignedAtBirth == "Male", 1, NA)),
    RaceEthnicity = as.factor(UCSFDerivedRaceEthnicity_X),
    PostCode = as.factor(PostalCode),
    Birth_Date = as.Date(BirthDate, format = "%Y-%m-%d"),
    Death_Date = as.Date(DeathDate, format = "%Y-%m-%d"),
    Survival_Status = as.factor(status),
    Smoking_Status = as.factor(if_else(grepl("^[*]", SmokingStatus), NA, SmokingStatus)),
    Highest_ed = as.factor(if_else(grepl("^[*]", HighestLevelOfEducation), NA, HighestLevelOfEducation)),
    Marital_Status = as.factor(if_else(grepl("^[*]", MaritalStatus), NA, MaritalStatus))
  )

dem_to_combine <- filtered_dem %>% dplyr::select(PatientDurableKey,sex, sex_at_birth, RaceEthnicity, PostCode, Birth_Date, Death_Date, Survival_Status, Smoking_Status, Highest_ed, Marital_Status)

#### Visits ####
filtered_visits <- Visits %>% filter(PatientDurableKey %in% diagnosis_to_combine$PatientDurableKey) %>%
  mutate(
    EventDate = AppointmentDateKeyValue,
    HeightIncm = as.numeric(HeightInInches) * 2.54,
    WeightInkg = as.numeric(WeightInOunces) / 35.274,
    BMI = as.numeric(BodyMassIndex)
  )

# Step 1: Set BMI to NA for values greater than 60
filtered_visits <- filtered_visits %>%
  mutate(BMI = if_else(BMI > 60, NA_real_, BMI))

# Step 2: Identify visits with significant height changes and set BMI to NA for those visits
filtered_visits <- filtered_visits %>%
  arrange(PatientDurableKey, EventDate) %>%
  group_by(PatientDurableKey) %>%
  mutate(
    height_change_next = abs((HeightIncm - lead(HeightIncm, order_by = EventDate)) / HeightIncm),
    height_change_prev = abs((HeightIncm - lag(HeightIncm, order_by = EventDate)) / HeightIncm),
    height_flag = if_else(height_change_next > 0.10 & height_change_prev > 0.10, 1, 0)
  ) %>%
  ungroup() %>%
  mutate(BMI = if_else(height_flag == 1, NA_real_, BMI)) %>%
  dplyr::select(-height_change_next, -height_change_prev, -height_flag)

# Step 3: Identify visits with significant weight changes and set BMI to NA for those visits
filtered_visits <- filtered_visits %>%
  arrange(PatientDurableKey, EventDate) %>%
  group_by(PatientDurableKey) %>%
  mutate(
    weight_change_prev = abs((WeightInkg - lag(WeightInkg, order_by = EventDate)) / WeightInkg),
    weight_change_next = abs((WeightInkg - lead(WeightInkg, order_by = EventDate)) / WeightInkg),
    weight_flag = if_else(weight_change_prev > 0.50 & weight_change_next > 0.50, 1, 0)
  ) %>%
  ungroup() %>%
  mutate(BMI = if_else(weight_flag == 1, NA_real_, BMI)) %>%
  dplyr::select(-weight_change_prev, -weight_change_next, -weight_flag)

visit_to_combine <- filtered_visits %>% dplyr::select(PatientDurableKey, EventDate, HeightIncm, WeightInkg, BMI)

# Ensure all data frames have EventDate as a Date type
medication_to_combine  <- medication_to_combine %>%
  mutate(EventDate = as.Date(EventDate, format = "%Y-%m-%d"))

diagnosis_to_combine <- diagnosis_to_combine %>%
  mutate(EventDate = as.Date(EventDate, format = "%Y-%m-%d"))

TR_to_combine <- TR_to_combine %>%
  mutate(EventDate = as.Date(EventDate, format = "%Y-%m-%d"))

appendectomy_to_combine  <- appendectomy_to_combine  %>%
  mutate(EventDate = as.Date(EventDate, format = "%Y-%m-%d"))

psy_to_combine <- psy_to_combine %>% mutate(EventDate = as.Date(EventDate, format = "%Y-%m-%d"))

visit_to_combine <- visit_to_combine %>% mutate(EventDate = as.Date(EventDate, format = "%Y-%m-%d"))


# Now combine all data frames
CombinedDataFrame <- bind_rows(medication_to_combine, diagnosis_to_combine, 
                               TR_to_combine , 
                               appendectomy_to_combine, psy_to_combine, visit_to_combine)
CombinedDataFrame <- inner_join(x=CombinedDataFrame, y=dem_to_combine, by = c("PatientDurableKey" = "PatientDurableKey") )
CombinedDataFrame <- CombinedDataFrame %>%
  group_by(PatientDurableKey) %>%
  arrange(EventDate) %>%  # Ensure that events are ordered by date
  mutate(
    # Replace NA values with 0 before applying cummax
    immunodisorder = ifelse(is.na(immunodisorder), 0, immunodisorder),
    gord = ifelse(is.na(gord), 0, gord),
    ulcer = ifelse(is.na(ulcer), 0, ulcer),
    ibs = ifelse(is.na(ibs), 0, ibs),
    other_gi_disorder = ifelse(is.na(other_gi_disorder), 0, other_gi_disorder),
    colitis = ifelse(is.na(colitis), 0, colitis),
    melanoma = ifelse(is.na(melanoma), 0, melanoma),
    other_skin_tumour = ifelse(is.na(other_skin_tumour), 0 , other_skin_tumour),
    nsclc = ifelse(is.na(nsclc), 0, nsclc),
    other_lung_tumour = ifelse(is.na(other_lung_tumour), 0, other_lung_tumour),
    rcc = ifelse(is.na(rcc), 0, rcc),
    head_n_neck_cancer = ifelse(is.na(head_n_neck_cancer), 0, head_n_neck_cancer),
    non_gi_irae_susp = ifelse(is.na(non_gi_irae_susp), 0, non_gi_irae_susp),
    appendectomy = ifelse(is.na(appendectomy), 0, appendectomy),
    depression = ifelse(is.na(depression), 0, depression),
    bipolar = ifelse(is.na(bipolar), 0, bipolar),
    other_mood_disorder = ifelse(is.na(other_mood_disorder), 0, other_mood_disorder),
    anxiety = ifelse(is.na(anxiety), 0, anxiety),
    schizophrenia_related_psychosis = ifelse(is.na(schizophrenia_related_psychosis), 0, schizophrenia_related_psychosis),
    other_psy = ifelse(is.na(other_psy), 0, other_psy),
    # Apply cummax to carry forward once the condition has appeared
    immunodisorder = cummax(immunodisorder),
    gord = cummax(gord),
    ulcer = cummax(ulcer),
    ibs = cummax(ibs),
    other_gi_disorder = cummax(other_gi_disorder),
    colitis = cummax(colitis),
    melanoma = cummax(melanoma),
    other_skin_tumour = cummax(other_skin_tumour),
    nsclc = cummax(nsclc),
    other_lung_tumour = cummax(other_lung_tumour),
    rcc = cummax(rcc),
    head_n_neck_cancer = cummax(head_n_neck_cancer),
    non_gi_irae_susp = cummax(non_gi_irae_susp),
    appendectomy = cummax(appendectomy),
    depression = cummax(depression),
    bipolar = cummax(bipolar),
    other_mood_disorder = cummax(other_mood_disorder),
    anxiety = cummax(anxiety),
    schizophrenia_related_psychosis = cummax(schizophrenia_related_psychosis),
    other_psy = cummax(other_psy)
  ) %>%
  ungroup()


# Create a 'YearMonth' column
CombinedDataFrame <- CombinedDataFrame %>%
  mutate(YearMonth = ceiling_date(EventDate, "month") - days(1))

# View the combined data frame
print(CombinedDataFrame)

write.csv(CombinedDataFrame, "raw_combined.csv")

print(length(unique(CombinedDataFrame$PatientDurableKey)))


#### Exclude patients who has IBD before checkpoint inhibitor administration ####
# Step 1: Find the earliest PD1/PDL1/CTLA4 occurrence date per patient
pd_date <- CombinedDataFrame %>%
  filter(PD1 == 1 | PDL1 == 1 | CTLA4 == 1) %>%
  group_by(PatientDurableKey) %>%
  summarise(earliest_pd_date = min(EventDate), latest_pd_date = max(EventDate)) %>%
  ungroup()

# Step 2: Merge this earliest date back with the original data
CombinedDataFrame <- CombinedDataFrame %>%
  left_join(pd_date, by = "PatientDurableKey")

# Step 3: Filter out rows with colitis == 1 before the earliest PD date <- sth wrong from here, please troubleshoot
# Filter the data per patient
# Step 1: Calculate the earliest colitis date per patient and handle Inf values
colitis_date <- CombinedDataFrame %>%
  filter(colitis == 1) %>%
  group_by(PatientDurableKey) %>%
  summarize(
    earliest_colitis_date = as.Date(ifelse(
      all(is.na(EventDate)), NA, min(EventDate, na.rm = TRUE)
    ))
  ) %>%
  ungroup()

# Step 2: Combine PD dates (earliest and latest) with colitis dates
patient_dates <- pd_date %>%
  left_join(colitis_date, by = "PatientDurableKey")

# Step 3: Filter patients whose earliest colitis is after the earliest PD administration and exclude Inf values
eligible_patients <- patient_dates %>%
  filter(!is.infinite(earliest_colitis_date) & (is.na(earliest_colitis_date) | earliest_colitis_date >= earliest_pd_date)) %>%
  dplyr::select(PatientDurableKey)

total_patients_before <- length(unique(patient_dates$PatientDurableKey))
total_patients_after <- length(unique(eligible_patients$PatientDurableKey))
num_dumped_patients <- total_patients_before - total_patients_after
print(num_dumped_patients)

# Step 5: Filter CombinedDataFrame based on eligible patients and exclude pregnancy records within PD date range
# Step 1: Ensure `earliest_pd_date` and `latest_pd_date` are joined to CombinedDataFrame
# Step 1: Identify patients with pregnancy events between earliest and latest PD dates
excluded_patients <- CombinedDataFrame %>%
  filter(
    pregnancy == 1 & !is.na(earliest_pd_date) & !is.na(latest_pd_date) &
      EventDate >= earliest_pd_date & EventDate <= latest_pd_date
  ) %>%
  dplyr::select(PatientDurableKey) %>%
  distinct()

print(length(unique(excluded_patients$PatientDurableKey)))

# Step 2: Filter out rows for these patients
FilteredData <- CombinedDataFrame %>%
  filter(PatientDurableKey %in% eligible_patients$PatientDurableKey) %>%
  filter(!PatientDurableKey %in% excluded_patients$PatientDurableKey) %>%
  left_join(colitis_date, by = "PatientDurableKey")  # Add earliest_colitis_date

# View the filtered data
print(FilteredData)

# Ensure EventDate is in Date format
FilteredData <- FilteredData %>%
  mutate(EventDate = as.Date(EventDate))

FilteredData <- FilteredData %>%
  group_by(PatientDurableKey) %>%
  mutate(
    # Find the first date where PD1, PDL1, or CTLA4 is 1
    T0 = min(EventDate[(PD1 == 1 | PDL1 == 1 | CTLA4 == 1)], na.rm = TRUE),
    # Calculate the number of months since T0
    mth_since_T0 = if_else(is.na(T0), NA_integer_, interval(T0, YearMonth) %/% months(1))
  ) %>%
  ungroup()
saveRDS(FilteredData, "JustFilteredData.rds")

# Group by PatientDurableKey and YearMonth to summarize
  latest_non_na <- function(x) {
    if (all(is.na(x))) {
      return(NA)
    }
    tail(na.omit(x), 1)
  }
  
write.csv(FilteredData, "filtered_data_to_python.csv", row.names = F, na = "NaN")

# Aggregate the data by getting the latest non-NA value for each column
FilteredData_summarized <- FilteredData %>%
  group_by(PatientDurableKey, mth_since_T0) %>%
  summarize(
    # Keep the latest EventDate
    EventDate = if(all(is.na(EventDate))) NA else max(EventDate, na.rm = TRUE),
    PD1 = if(all(is.na(PD1))) NA else max(PD1, na.rm = TRUE),
    PDL1 = if(all(is.na(PDL1))) NA else max(PDL1, na.rm = TRUE),
    CTLA4 = if(all(is.na(CTLA4))) NA else max(CTLA4, na.rm = TRUE),
    Durvalumab = if(all(is.na(Durvalumab))) NA else max(Durvalumab, na.rm = TRUE),
    Atezolizumab = if(all(is.na(Atezolizumab))) NA else max(Atezolizumab, na.rm = TRUE),
    Pembrolizumab = if(all(is.na(Pembrolizumab))) NA else max(Pembrolizumab, na.rm = TRUE),
    Nivolumab = if(all(is.na(Nivolumab))) NA else max(Nivolumab, na.rm = TRUE),
    Ipilimumab = if(all(is.na(Ipilimumab))) NA else max(Ipilimumab, na.rm = TRUE),
    Cemiplimab = if(all(is.na(Cemiplimab))) NA else max(Cemiplimab, na.rm = TRUE),
    Vibostolimab_Pembrolizumab = if(all(is.na(Vibostolimab_Pembrolizumab))) NA else max(Vibostolimab_Pembrolizumab, na.rm = TRUE),
    corticosteroids = if(all(is.na(corticosteroids))) NA else max(corticosteroids, na.rm = TRUE),
    ibd_biologics = if(all(is.na(ibd_biologics))) NA else max(ibd_biologics, na.rm = TRUE),
    ppi = if(all(is.na(ppi))) NA else max(ppi, na.rm = TRUE),
    nsaid = if(all(is.na(nsaid))) NA else max(nsaid, na.rm = TRUE),
    other_cancer_tx = if(all(is.na(other_cancer_tx))) NA else max(other_cancer_tx, na.rm = TRUE),
    antibiotics = if(all(is.na(antibiotics))) NA else max(antibiotics, na.rm = TRUE),
    
    # Apply `latest_non_na` for all columns except EventDate
    across(
      -EventDate,
      latest_non_na,
      .names = "{.col}"
    ),
    .groups = "drop"
  ) %>%
  arrange(PatientDurableKey, YearMonth) %>%
  group_by(PatientDurableKey) %>%
  mutate(
    immunodisorder = cummax(immunodisorder),
    gord = cummax(gord),
    ulcer = cummax(ulcer),
    ibs = cummax(ibs),
    other_gi_disorder = cummax(other_gi_disorder),
    colitis = cummax(colitis),
    melanoma = cummax(melanoma),
    nsclc = cummax(nsclc),
    rcc = cummax(rcc),
    non_gi_irae_susp = cummax(non_gi_irae_susp)
  ) %>%
  ungroup()

FilteredData_summarized_alldate<- FilteredData_summarized %>% filter(!is.na(mth_since_T0))
saveRDS(FilteredData_summarized_alldate, "Data_All_Date.rds")
FilteredData_summarized_final <- FilteredData_summarized %>% filter(!is.na(mth_since_T0) & mth_since_T0 >= 0)

FilteredData_summarized_final_2 <- FilteredData_summarized_final %>% 
  group_by(PatientDurableKey) %>%
  arrange(EventDate) %>%
  mutate(
    # Replace NA with 0
    PD1 = ifelse(is.na(PD1), 0, PD1),
    PDL1 = ifelse(is.na(PDL1), 0, PDL1),
    CTLA4 = ifelse(is.na(CTLA4), 0, CTLA4),
    Durvalumab = ifelse(is.na(Durvalumab), 0, Durvalumab),
    Atezolizumab = ifelse(is.na(Atezolizumab), 0, Atezolizumab),
    Pembrolizumab = ifelse(is.na(Pembrolizumab), 0, Pembrolizumab),
    Nivolumab = ifelse(is.na(Nivolumab), 0, Nivolumab),
    Ipilimumab = ifelse(is.na(Ipilimumab), 0, Ipilimumab),
    Cemiplimab = ifelse(is.na(Cemiplimab), 0, Cemiplimab),
    Vibostolimab_Pembrolizumab = ifelse(is.na(Vibostolimab_Pembrolizumab), 0, Vibostolimab_Pembrolizumab),
    corticosteroids = ifelse(is.na(corticosteroids), 0, corticosteroids),
    ibd_biologics = ifelse(is.na(ibd_biologics), 0, ibd_biologics),
    ppi = ifelse(is.na(ppi), 0, ppi),
    nsaid = ifelse(is.na(nsaid), 0, nsaid),
    other_cancer_tx = ifelse(is.na(other_cancer_tx), 0, other_cancer_tx),
    antibiotics = ifelse(is.na(antibiotics), 0, antibiotics)
  ) %>% ungroup()

FilteredData_summarized_final_22 <- inner_join(x=FilteredData_summarized_final_2, y=dem_to_combine, by = c("PatientDurableKey" = "PatientDurableKey") )

gropd_df <- FilteredData_summarized_final_2 %>%
  arrange(
    PatientDurableKey, mth_since_T0, YearMonth, RaceEthnicity, sex, 
    sex_at_birth, Birth_Date, Death_Date, Survival_Status, 
    PostCode, Smoking_Status, PD1, PDL1, CTLA4, accumulated_pd1_doses, accumulated_pdl1_doses, accumulated_ctla4_doses, 
    Durvalumab, accumulated_durvalumab_doses,
    Atezolizumab, accumulated_atezolizumab_doses,
    Pembrolizumab, accumulated_pembrolizumab_doses,
    Nivolumab, accumulated_nivolumab_doses,
    Ipilimumab, accumulated_ipilimumab_doses,
    Cemiplimab, accumulated_cemiplimab_doses,
    Vibostolimab_Pembrolizumab, accumulated_vibostolimab_pembrolizumab_doses,
    corticosteroids, ibd_biologics, ppi, nsaid, 
    other_cancer_tx, antibiotics, immunodisorder, gord, ulcer, ibs, 
    other_gi_disorder, melanoma, other_skin_tumour, nsclc, other_lung_tumour, rcc, head_n_neck_cancer, non_gi_irae_susp, pregnancy,
    ProcedureName, VitD_Value, VitD_Unit, VitD_Abnormal, VitD_too_high, 
    VitD_too_low, appendectomy, depression, bipolar, other_mood_disorder, 
    anxiety, schizophrenia_related_psychosis, other_psy, HeightIncm, 
    WeightInkg, BMI, Highest_ed, Marital_Status
  ) %>%
  dplyr::select(
    PatientDurableKey, mth_since_T0, YearMonth, RaceEthnicity, sex,
    sex_at_birth, Birth_Date, Death_Date, Survival_Status, 
    PostCode, Smoking_Status, PD1, PDL1, CTLA4, accumulated_pd1_doses, accumulated_pdl1_doses, accumulated_ctla4_doses, 
    Durvalumab, accumulated_durvalumab_doses,
    Atezolizumab, accumulated_atezolizumab_doses,
    Pembrolizumab, accumulated_pembrolizumab_doses,
    Nivolumab, accumulated_nivolumab_doses,
    Ipilimumab, accumulated_ipilimumab_doses,
    Cemiplimab, accumulated_cemiplimab_doses,
    Vibostolimab_Pembrolizumab, accumulated_vibostolimab_pembrolizumab_doses, 
    corticosteroids, ibd_biologics, ppi, nsaid, 
    other_cancer_tx, antibiotics, immunodisorder, gord, ulcer, ibs, 
    other_gi_disorder, melanoma, other_skin_tumour, nsclc, other_lung_tumour, rcc, head_n_neck_cancer, non_gi_irae_susp, pregnancy,
    ProcedureName, VitD_Value, VitD_Unit, VitD_Abnormal, VitD_too_high, 
    VitD_too_low, appendectomy, depression, bipolar, other_mood_disorder, 
    anxiety, schizophrenia_related_psychosis, other_psy, HeightIncm, 
    WeightInkg, BMI, Highest_ed, Marital_Status, everything(), colitis
  )

# Create the new data frame with a death column
gropd_df_all <- gropd_df %>%
  mutate(
    # Check if YearMonth is greater than or equal to Death_Date's year and month
    death = if_else(
      !is.na(Death_Date) & (floor_date(YearMonth, "month") >= floor_date(Death_Date, "month")),
      1, 0
    )
  )

gropd_df_all2 <- gropd_df_all %>%
  mutate(
    # Check if other_cancer_tx became 1 after the latest PD date
    tx_switch = if_else(
      !is.na(latest_pd_date) & (EventDate > latest_pd_date) & other_cancer_tx == 1,
      1, 0
    ),
    non_gi_toxicity = if_else(
      !is.na(latest_pd_date) &
        (EventDate > latest_pd_date) &
        PD1 == 0 & PDL1 == 0 & CTLA4 == 0 & colitis == 0 & non_gi_irae_susp == 1,
      1, 0)
  )

gropd_df_all2_filtered <- gropd_df_all2 %>%
  dplyr::select(-pregnancy, -ProcedureName, -VitD_Value, -VitD_Unit, -VitD_Abnormal, -VitD_too_high, -VitD_too_low, -Highest_ed, -earliest_pd_date, -latest_pd_date, -EventDate)

# Add the `time_to_colitis` column with decimal months difference
gropd_df_all2_filtered <- gropd_df_all2_filtered %>%
  mutate(
    days_to_colitis = as.numeric(difftime(earliest_colitis_date, T0, units = "days"))
  )

# Arrange data by PatientDurableKey and mth_since_T0
gropd_df_all2_filtered <- gropd_df_all2_filtered %>%
  arrange(PatientDurableKey, mth_since_T0)

# Fill NA values with the last observed value for each accumulated dose column
gropd_df_all2_filtered <- gropd_df_all2_filtered %>%
  group_by(PatientDurableKey) %>%
  fill(accumulated_pd1_doses, .direction = "down") %>%
  fill(accumulated_pdl1_doses, .direction = "down") %>%
  fill(accumulated_ctla4_doses, .direction = "down") %>%
  fill(accumulated_durvalumab_doses, .direction = "down") %>%
  fill(accumulated_atezolizumab_doses, .direction = "down") %>%
  fill(accumulated_pembrolizumab_doses, .direction = "down") %>%
  fill(accumulated_nivolumab_doses, .direction = "down") %>%
  fill(accumulated_ipilimumab_doses, .direction = "down") %>%
  fill(accumulated_cemiplimab_doses, .direction = "down") %>%
  fill(accumulated_vibostolimab_pembrolizumab_doses, .direction = "down") %>%
  ungroup()

write.csv(gropd_df_all2 , "cleaned.csv", row.names = F)
write.csv(gropd_df_all2_filtered  , "cleaned_filtered.csv", row.names = F)
saveRDS(gropd_df_all2, "cleaned.rds")
saveRDS(gropd_df_all2_filtered, "cleaned_filtered.rds")
