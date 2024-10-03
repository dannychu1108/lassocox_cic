setwd("/Users/dchu4/Library/Mobile Documents/com~apple~CloudDocs/Documents/Clinical and Epi Research Year 1/Research - IBD/Analysis")
#cohort_original <- readRDS("/Users/dchu4/Library/Mobile Documents/com~apple~CloudDocs/Documents/Clinical and Epi Research Year 1/Research - IBD/Table exported/cleaned_filtered.rds")
library(glmnet)
library(mice)
library(dplyr)
library(imputeTS)
library(missForest)
#devtools::install_github(devtools::install_github("dannychu1108/grpreg_forked", force = TRUE))
#devtools::install_github(devtools::install_github("pbreheny/grpreg", force = TRUE))
library(survival)
library(survminer)
library(ggplot2)
library(ranger)
library(ggfortify)
library(ggsurvfit)
library(cmprsk)
library(riskRegression)
library(glmpath)
library(fastcmprsk)
library(survivalROC)
library(fastDummies)
library(grpreg)
library(tidyverse)
# 
# cohort_original <- ungroup(cohort_original)
# str(cohort_original)
# 
# cohort <- cohort_original
# 
# 
# library(dplyr)
# 
# # Step 3: Set the cutoff date
# cutoff_date <- as.Date("2024-05-07")
# 
# # Step 4: Filter rows that are on or before the cutoff date
# cohort <- cohort %>% filter(YearMonth <= cutoff_date) #removing those weird dates
# 
# # Convert "Unknown/Declined" to NA in the RaceEthnicity column
# cohort <- cohort %>% mutate(RaceEthnicity = na_if(RaceEthnicity, "Unknown/Declined"), Marital_Status = na_if(Marital_Status, "Unknown/Declined"))
# 
# # Replace missing values in the `sex` column with values from `sex_at_birth`
# cohort <- cohort %>%
#   mutate(sex = ifelse(is.na(sex), sex_at_birth, sex))
# 
# # Combining some categories into one
# # Combine different marital statuses into broader groups
# cohort <- cohort %>%
#   mutate(
#     Marital_Status = case_when(
#       Marital_Status %in% c("Married", "Significant Other", "Registered Domestic Partner") ~ "Partnered",
#       Marital_Status %in% c("Widowed", "RDP-Widowed") ~ "Widowed",
#       Marital_Status %in% c("Legally Separated", "Divorced") ~ "Separated/Divorced",
#       Marital_Status == "Single" ~ "Single",
#       TRUE ~ Marital_Status  # Preserve other categories like "Unknown/Declined"
#     )
#   )
# 
# cohort <- cohort %>%
#   mutate(
#     Smoking_Status = case_when(
#       Smoking_Status == "Never" ~ "Never",
#       Smoking_Status == "Passive Smoke Exposure - Never Smoker" ~ "Passive Never Smoker",
#       Smoking_Status %in% c("Some Days", "Light Smoker", "Smoker, Current Status Unknown") ~ "Occasional Smoker",
#       Smoking_Status %in% c("Every Day", "Heavy Smoker") ~ "Daily Smoker",
#       Smoking_Status == "Former" ~ "Former Smoker",
#       Smoking_Status %in% c("Unknown", "Never Assessed") ~ NA_character_,  # Convert to NA
#       TRUE ~ Smoking_Status  # Preserve other existing statuses
#     )
#   )
# 
# # Convert PostCode to character if it's currently a factor
# cohort <- cohort %>%
#   mutate(PostCode = as.character(PostCode)) %>%
#   mutate(
#     PostCode = case_when(
#       PostCode == "*Unknown" ~ NA_character_,
#       as.numeric(PostCode) < 900 ~ "Non-Cali",  # Assuming numeric comparison
#       TRUE ~ PostCode  # Retain original value if not applicable
#     )
#   )
# 
# # Combine Native American or Alaska Native together with Native Hawaiian or Other Pacific Islander, and Southest Asian and North African combined to Asian
# cohort <- cohort %>% mutate(
#   RaceEthnicity = case_when(
#     RaceEthnicity %in% c("Asian", "Southwest Asian and North African") ~ "Asian/Southwest Asian/North African",
#     RaceEthnicity %in% c("Native American or Alaska Native", "Native Hawaiian or Other Pacific Islander") ~ "Native Hawaiian, Native American, Alaska Native or Other Pacific Islander",
#     TRUE ~ RaceEthnicity  # Keep the original value for all other cases
#   )
# )
# 
# # Convert all character columns to factor
# cohort <- cohort %>%
#   mutate_if(is.character, as.factor)
# 
# 
# # Function to calculate age in months
# calculate_age_in_months <- function(start_date, end_date) {
#   # Calculate the difference in years and months
#   diff_years <- as.integer(format(end_date, "%Y")) - as.integer(format(start_date, "%Y"))
#   diff_months <- as.integer(format(end_date, "%m")) - as.integer(format(start_date, "%m"))
#   
#   # Total months = difference in years (in months) + difference in months
#   total_months <- diff_years * 12 + diff_months
#   
#   return(round(total_months/12, 1))
# }
# 
# # Ensure the date columns are in Date format
# cohort$YearMonth <- as.Date(cohort$YearMonth)
# cohort$Birth_Date <- as.Date(cohort$Birth_Date)
# 
# # Apply the function to each row to calculate age in months
# cohort$age <- mapply(calculate_age_in_months, cohort$Birth_Date, cohort$YearMonth)
# 
# # Identify PatientDurableKeys with at least one record of age < 18
# patients_below_18 <- cohort %>%
#   filter(age < 18) %>%
#   dplyr::select(PatientDurableKey) %>%
#   distinct()
# 
# # Filter out all patients whose keys are in patients_below_18
# cohort_filtered <- cohort %>%
#   filter(!PatientDurableKey %in% patients_below_18$PatientDurableKey)
# 
# # Convert specific character columns to Date
# cohort_filtered <- cohort_filtered %>%
#   mutate(
#     YearMonth = as.Date(YearMonth),
#     Birth_Date = as.Date(Birth_Date),
#     Death_Date = as.Date(Death_Date),
#     earliest_colitis_date = as.Date(earliest_colitis_date),
#     T0 = as.Date(T0)
#   )
# 
# # Convert the remaining character columns to factor
# cohort_filtered <- cohort_filtered %>%
#   mutate(across(where(is.character), as.factor))
# 
# str(cohort_filtered)
# 
# write.csv(cohort_filtered, "further_cleaned.csv")
# saveRDS(cohort_filtered, "further_cleaned.rds")
cohort_filtered <- readRDS("further_cleaned.rds") 
str(cohort_filtered)

# Define a function to fill NA values with the mean of the last observed and next observed non-NA values
fill_na_with_mean <- function(x) {
  na_idx <- which(is.na(x))
  if (length(na_idx) == length(x)) {
    return(x)  # All values are NA
  }
  
  for (i in na_idx) {
    prev_idx <- if (any(!is.na(x[1:i]))) max(which(!is.na(x[1:i])), na.rm = TRUE) else NA
    next_idx <- if (any(!is.na(x[i:length(x)]))) min(which(!is.na(x[i:length(x)])) + (i - 1), na.rm = TRUE) else NA
    
    if (!is.na(prev_idx) & !is.na(next_idx)) {
      x[i] <- mean(c(x[prev_idx], x[next_idx]), na.rm = TRUE)
    } else if (!is.na(prev_idx)) {
      x[i] <- x[prev_idx]
    } else if (!is.na(next_idx)) {
      x[i] <- x[next_idx]
    }
  }
  return(x)
}

cohort_filtered <- cohort_filtered %>%
  arrange(PatientDurableKey, YearMonth) %>%
  group_by(PatientDurableKey) %>%
  mutate(
    WeightInkg = fill_na_with_mean(WeightInkg),
    HeightIncm = fill_na_with_mean(HeightIncm)
  ) %>%
  ungroup()

cohort_filtered$BMI_reserve <- cohort_filtered$BMI
cohort_filtered <-cohort_filtered %>% mutate(BMI = ifelse(is.na(BMI_reserve), WeightInkg / (HeightIncm / 100)^2, BMI)) %>% mutate(BMI = ifelse(BMI>= 60, NA, BMI))

cohort_filtered$PostCode_char <- as.character(cohort_filtered$PostCode)
cohort_filtered$PostCode_char <- ifelse(cohort_filtered$PostCode_char == "Non-Cali", NA, cohort_filtered$PostCode_char)
cohort_filtered$PostCode <- as.factor(cohort_filtered$PostCode_char)

str(cohort_filtered)

postcode_conversion <- readRDS("three_digit_zip_api.RDS")
postcode_conversion <- postcode_conversion %>% dplyr::select(BENE_ZIP_3DIGIT, weighted_median_ADI_STATERNK, weighted_mean_miles_to_UCSF, weighted_mean_hours_to_UCSF)

cohort_filtered_orig <- cohort_filtered
cohort_filtered<- left_join(cohort_filtered, postcode_conversion, by = c("PostCode_char" = "BENE_ZIP_3DIGIT"))

cohort_filtered$PostCode_char <- NULL

closest_bmi_before_pd <- readRDS("/Users/dchu4/Library/Mobile Documents/com~apple~CloudDocs/Documents/Clinical and Epi Research Year 1/Research - IBD/Table exported/closest_bmi_before_pd.rds") 
cohort_filtered<- left_join(cohort_filtered, closest_bmi_before_pd, by = c("PatientDurableKey" = "PatientDurableKey"))
cohort_filtered <- cohort_filtered %>% mutate(closest_BMI_before_PD = ifelse(closest_BMI_before_PD>= 60, NA, closest_BMI_before_PD)) %>%
  rename(BMI_before_CI = closest_BMI_before_PD)

# Assume cohort_filtered is your data frame
missingness_summary <- cohort_filtered %>%
  summarise_all(~ sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "num_missing") %>%
  mutate(total = nrow(cohort_filtered),
         pct_missing = num_missing / total * 100)
sink("missingness_summary.txt")
# Print the summary table
print(missingness_summary, n = 100)

cat("Postcode was converted to distance to UCSF in both miles and hours, as well as ADI - Area Deprivation Index Rank (they only provide the rank, and as the dataset contains only Californian Post Code, so State rank was used.", "\n", 
    "5 sets of data were imputed, with each 15 iterations. The mean of BMI, distance (miles and hours) were imputed with mean of the 5. Rank was imputed with median to keep it constant and the nature of rank. Categorical variables are all imputed with the mode. \n",
    "BMI with missing of the same patient in between month was filled with the mean of next avaliable measurement and previous measurement. For first and last observation, it was filled with either the closet next avaliable or closet previous avaliable. \n",
    "Also, BMI above 60 were considered as error input, all converted to NA to be imputed")
sink()

cohort_filtered_unselect <- cohort_filtered
#cohort_filtered <- cohort_filtered_unselect 
cohort_filtered <- cohort_filtered %>% dplyr::select(1:9, 11:68, 70:73)

str(cohort_filtered)
cohort_filtered <- cohort_filtered %>%
  mutate(RaceEthnicity = ifelse(RaceEthnicity == "White", 1, 0),
         Smoking_Status = case_when(
           Smoking_Status %in% c("Never", "Passive Never Smoker") ~ "Never_Smoked",
           Smoking_Status %in% c("Daily Smoker", "Occasional Smoker") ~ "Current_Smoker",
           Smoking_Status == "Former Smoker" ~ "Former_Smoker",
           TRUE ~ NA_character_)
         ) # I call it RaceEthnicity here, but in fact it's a "White" variable. Not worth changing the variable names below
cohort_filtered$PatientDurableKey <- as.factor(cohort_filtered$PatientDurableKey)
cohort_filtered$Smoking_Status <- factor(cohort_filtered$Smoking_Status, levels = c("Never_Smoked", "Former_Smoker", "Current_Smoker"))

str(cohort_filtered)

# Prepare the methods vector with an empty string (no imputation) for all columns except BMI
# Define the methods to use for each target variable
method_vector <- rep("", ncol(cohort_filtered))
method_vector[which(names(cohort_filtered) == "BMI")] <- "rf"  # Random forest for BMI
method_vector[which(names(cohort_filtered) == "Smoking_Status")] <- "rf"  # Random forest for Smoking Status
method_vector[which(names(cohort_filtered) == "RaceEthnicity")] <- "rf"  # Random forest for Race/Ethnicity
method_vector[which(names(cohort_filtered) == "Marital_Status")] <- "rf"  # Random forest for Marital Status
method_vector[which(names(cohort_filtered) == "weighted_median_ADI_STATERNK")] <- "rf"
method_vector[which(names(cohort_filtered) == "weighted_mean_miles_to_UCSF")] <- "rf"
method_vector[which(names(cohort_filtered) == "weighted_mean_hours_to_UCSF")] <- "rf"
method_vector[which(names(cohort_filtered) == "BMI_before_CI")] <- "rf"

# Initialize a predictor matrix
predictor_matrix <- matrix(0, ncol = ncol(cohort_filtered), nrow = ncol(cohort_filtered))
colnames(predictor_matrix) <- rownames(predictor_matrix) <- names(cohort_filtered)

# Set predictors for BMI (exclude itself)
predictor_matrix["BMI", ] <- 1
predictor_matrix["BMI", "BMI"] <- 0

# Set predictors for Smoking_Status (exclude itself)
predictor_matrix["Smoking_Status", ] <- 1
predictor_matrix["Smoking_Status", "Smoking_Status"] <- 0

# Set predictors for Marital_Status (exclude itself)
predictor_matrix["Marital_Status", ] <- 1
predictor_matrix["Marital_Status", "Marital_Status"] <- 0

# Set predictors for RaceEthnicity (exclude itself)
predictor_matrix["RaceEthnicity", ] <- 1
predictor_matrix["RaceEthnicity", "RaceEthnicity"] <- 0

# Set predictors for weighted_median_ADI_STATERNK
predictor_matrix["weighted_median_ADI_STATERNK", ] <- 1
predictor_matrix["weighted_median_ADI_STATERNK", "weighted_median_ADI_STATERNK"] <- 0

# Set predictors for weighted_mean_miles_to_UCSF
predictor_matrix["weighted_mean_miles_to_UCSF", ] <- 1
predictor_matrix["weighted_mean_miles_to_UCSF", "weighted_mean_miles_to_UCSF"] <- 0

# Set predictors for weighted_mean_hours_to_UCSF
predictor_matrix["weighted_mean_hours_to_UCSF", ] <- 1
predictor_matrix["weighted_mean_hours_to_UCSF", "weighted_mean_hours_to_UCSF"] <- 0

# Set predictors for BMI_before_CI
predictor_matrix["BMI_before_CI", ] <- 1
predictor_matrix["BMI_before_CI", "BMI_before_CI"] <- 0

# Exclude unnecessary columns from all target variable predictors
exclude_cols <- c("Death_Date", "YearMonth", "sex_at_birth", "Birth_Date", "Survival_Status", "HeightIncm", "WeightInkg", "sex_at_birth", "days_to_colitis", "earliest_colitis_date", "T0")
predictor_matrix[, exclude_cols] <- 0

m = 5 # Number of imputations, should be 5 but input 1 to see if it runs
maxit = 10 # Number of iterations per imputation, should be 10

# Extract column names
column_names <- colnames(cohort_filtered)
names(method_vector) <- column_names
method_vector

# Perform multiple imputation using `mice` with the specified methods and predictor matrix
mice_result <- mice(
  cohort_filtered,
  method = method_vector,
  predictorMatrix = predictor_matrix,
  m = 5,       # Number of imputations
  maxit = 10,   # Number of iterations per imputation
  printFlag = TRUE
)
mice:::find.collinear(cohort_filtered)

saveRDS(mice_result, "imputed_5datasets_010724.rds")
mice_result <- readRDS("imputed_5datasets_010724.rds")

# Retrieve the imputed datasets as a list
all_imputed <- lapply(1:m, function(i) complete(mice_result, i))

# Combine all datasets into one
all_imputed_combined <- do.call(rbind, lapply(seq_along(all_imputed), function(i) {
  cbind(all_imputed[[i]], Imputation = i)
}))

# Calculate the mean of BMI values across all imputations
mean_bmi <- aggregate(BMI ~ PatientDurableKey + mth_since_T0, all_imputed_combined, mean)
mean_bmi <- mean_bmi %>% rename(BMI_imputed = BMI)

mean_base_bmi <- aggregate(BMI_before_CI ~ PatientDurableKey + mth_since_T0, all_imputed_combined, mean)
mean_base_bmi <- mean_base_bmi %>% rename(BMI_before_CI_imputed = BMI_before_CI)

mean_weighted_miles <- aggregate(weighted_mean_miles_to_UCSF ~ PatientDurableKey + mth_since_T0, all_imputed_combined, mean)
mean_weighted_miles <- mean_weighted_miles %>% rename(weighted_mean_miles_to_UCSF_imputed = weighted_mean_miles_to_UCSF)

mean_weighted_hours <- aggregate(weighted_mean_hours_to_UCSF ~ PatientDurableKey + mth_since_T0, all_imputed_combined, mean)
mean_weighted_hours <- mean_weighted_hours %>% rename(weighted_mean_hours_to_UCSF_imputed = weighted_mean_hours_to_UCSF)

median_ADI <- aggregate(weighted_median_ADI_STATERNK ~ PatientDurableKey + mth_since_T0, all_imputed_combined, median)
median_ADI <- median_ADI %>% rename(weighted_median_ADI_STATERNK_imputed = weighted_median_ADI_STATERNK)

# Define a function to compute the mode
compute_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Combine all imputed datasets
all_imputed_combined <- do.call(rbind, lapply(seq_along(all_imputed), function(i) {
  cbind(all_imputed[[i]], Imputation = i)
}))

# Aggregate modes for the selected categorical columns across all imputations
mode_summary <- all_imputed_combined %>%
  group_by(PatientDurableKey, mth_since_T0) %>%
  summarise(
    imputed_RaceEthnicity = compute_mode(RaceEthnicity),
    imputed_Smoking_Status = compute_mode(Smoking_Status),
    imputed_Marital_Status = compute_mode(Marital_Status),
    .groups = 'drop'
  )

# Merge back to the original cohort
cohort_imputed <- left_join(cohort_filtered, mean_bmi, by = c("PatientDurableKey", "mth_since_T0"))
cohort_imputed <- left_join(cohort_imputed, mean_base_bmi, by = c("PatientDurableKey", "mth_since_T0"))
cohort_imputed <- left_join(cohort_imputed, mean_weighted_miles, by = c("PatientDurableKey", "mth_since_T0"))
cohort_imputed <- left_join(cohort_imputed, mean_weighted_hours, by = c("PatientDurableKey", "mth_since_T0"))
cohort_imputed <- left_join(cohort_imputed, median_ADI, by = c("PatientDurableKey", "mth_since_T0"))
cohort_imputed <- left_join(cohort_imputed, mode_summary, by = c("PatientDurableKey", "mth_since_T0"))

# Assign 1 to imputed values, 0 otherwise
cohort_imputed <- cohort_imputed %>%
  mutate(
    Isit_Imputed_BMI = ifelse(is.na(BMI), 1, 0),
    Isit_Imputed_RaceEthnicity = ifelse(is.na(RaceEthnicity), 1, 0),
    Isit_Imputed_Smoking_Status = ifelse(is.na(Smoking_Status), 1, 0),
    Isit_Imputed_Marital_Status = ifelse(is.na(Marital_Status), 1, 0),
    Isit_Imputed_weighted_median_ADI_STATERNK = ifelse(is.na(PostCode), 1, 0),
    Isit_Imputed_weighted_mean_miles_to_UCSF = ifelse(is.na(PostCode ), 1, 0),
    Isit_Imputed_weighted_mean_hours_to_UCSF = ifelse(is.na(PostCode ), 1, 0)
  )

# Ensure consistency within each PatientDurableKey
cohort_imputed <- cohort_imputed %>%
  arrange(PatientDurableKey, mth_since_T0) %>%
  group_by(PatientDurableKey) %>%
  mutate(
    BMI_before_CI_imputed = first(BMI_before_CI_imputed),
    weighted_median_ADI_STATERNK_imputed = first(weighted_median_ADI_STATERNK_imputed),
    weighted_mean_miles_to_UCSF_imputed = first(weighted_mean_miles_to_UCSF_imputed),
    weighted_mean_hours_to_UCSF_imputed = first(weighted_mean_hours_to_UCSF_imputed)
  ) %>%
  ungroup()

write.csv(cohort_imputed, "imputed_full_data_010724.csv")
saveRDS(cohort_imputed, "imputed_full_data_010724.rds")
cohort_imputed <- readRDS("imputed_full_data_010724.rds")

# Check the structure to ensure the columns have been added
str(cohort_imputed)

# Filter out rows with NA in specified columns
cohort_nona <- cohort_imputed %>%
  filter(
    !is.na(imputed_RaceEthnicity) & 
      !is.na(weighted_mean_miles_to_UCSF_imputed) & 
      !is.na(weighted_mean_hours_to_UCSF_imputed) &
      !is.na(weighted_median_ADI_STATERNK_imputed) &
      !is.na(imputed_Smoking_Status) & 
      !is.na(BMI_imputed) & 
      !is.na(imputed_Marital_Status) &
      !is.na(BMI_before_CI_imputed)
  ) #nothing was really removed, so I guess the imputation was great

# Function to filter out subsequent rows after colitis == 1, with ordering
filter_colitis_ordered <- function(df) {
  # Ensure data is ordered by mth_since_T0
  df <- df %>% arrange(mth_since_T0)
  
  # Check if any occurrence of colitis == 1 exists
  if (any(df$colitis == 1)) {
    # Get index of the first colitis == 1 occurrence
    first_colitis_idx <- which(df$colitis == 1)[1]
    # Return rows up to this index
    return(df[1:first_colitis_idx, ])
  } else {
    # If colitis == 1 does not exist, return all rows
    return(df)
  }
}

# Create tstart and tstop, but exclude the last interval
cohort_imputed_intervals <- cohort_nona %>%
  arrange(PatientDurableKey, mth_since_T0) %>%
  group_by(PatientDurableKey) %>%
  mutate(
    tstart = mth_since_T0,
    tstop = lead(mth_since_T0)  # Take the next observation or NA if it's the last observation
  ) %>%
  ungroup()

cohort_imputed_intervals <- cohort_imputed_intervals %>%
  mutate(tstop = ifelse(is.na(tstop), tstart, tstop))

cohort_imputed_intervals <- cohort_imputed_intervals %>%
  mutate(
    tstop = ifelse(is.na(tstop), tstart, tstop), 
    tstart = ifelse(tstart == tstop, NA, tstart)
  )

cohort_imputed_intervals <- cohort_imputed_intervals %>%
  arrange(PatientDurableKey, mth_since_T0) %>%
  group_by(PatientDurableKey) %>%
  mutate(
    tstop = ifelse(lead(is.na(tstart), default = FALSE), NA, tstop)
  ) %>%
  ungroup()

cohort_imputed_intervals <- cohort_imputed_intervals %>%
  group_by(PatientDurableKey) %>%
  mutate(
    tstop = ifelse(is.na(tstop), (lead(tstop, default = last(tstop)) + tstart) / 2, tstop),
    tstart = ifelse(is.na(tstart), (lag(tstart, default = first(tstart)) + tstop) / 2, tstart)
  ) %>%
  ungroup()


# Generate the overall start and stop for each PatientDurableKey
cohort_imputed_intervals <- cohort_imputed_intervals %>%
  group_by(PatientDurableKey) %>%
  mutate(
    start = min(mth_since_T0, na.rm = TRUE),
    stop = max(mth_since_T0, na.rm = TRUE)  # No extra increment needed
  ) %>%
  ungroup()

# Apply the function to filter rows by PatientDurableKey
up_to_colitis_occur <- cohort_imputed_intervals %>%
  group_by(PatientDurableKey) %>%
  do(filter_colitis_ordered(.)) %>%
  ungroup()

# Verify the new dataset structure
str(up_to_colitis_occur)

# List of variables to convert to factor
up_to_colitis_occur$sex <- factor(up_to_colitis_occur$sex, levels = c(0, 1), labels = c("female", "male"))

factor_vars <- c("sex", "PD1", "PDL1", "CTLA4", "Durvalumab", "Atezolizumab", "Pembrolizumab", 
                 "Nivolumab", "Ipilimumab", "Cemiplimab", "Vibostolimab_Pembrolizumab", 
                 "ppi", "nsaid", "antibiotics", "immunodisorder", "gord", "ulcer", 
                 "ibs", "other_gi_disorder", "melanoma", "other_skin_tumour", "nsclc", 
                 "other_lung_tumour", "rcc", "head_n_neck_cancer", "appendectomy", 
                 "depression", "bipolar", "other_mood_disorder", "anxiety", 
                 "schizophrenia_related_psychosis", "other_psy")

# Convert to factor
for (var in factor_vars) {
  up_to_colitis_occur[[var]] <- factor(up_to_colitis_occur[[var]])
}

# Create the new status variable
up_to_colitis_occur <- up_to_colitis_occur %>%
  mutate(status = case_when(
    colitis == 1 ~ 1,
    death == 1 ~ 2,
    tx_switch == 1 ~ 2,
    non_gi_toxicity == 1 ~ 2,
    TRUE ~ 0  # Default value if none of the conditions are met
  ) %>% as.factor())

up_to_colitis_or_event_occur <- up_to_colitis_occur

# Identify the first occurrence of status 1 or 2 for each patient
first_event_time <- up_to_colitis_or_event_occur %>%
  group_by(PatientDurableKey) %>%
  summarize(first_event_time = min(mth_since_T0[status != 0], na.rm = TRUE)) %>%
  mutate(first_event_time = ifelse(is.infinite(first_event_time), NA, first_event_time))

# Merge the first_event_time back to the main dataset
up_to_colitis_or_event_occur <- up_to_colitis_or_event_occur %>%
  left_join(first_event_time, by = "PatientDurableKey")

# Filter the rows based on the first_event_time
up_to_colitis_or_event_occur <- up_to_colitis_or_event_occur %>%
  filter((is.na(first_event_time) | mth_since_T0 <= first_event_time) & (stop != 0)) %>% # you keep all row who did not experience the event, and keep only up to the event time for those who experienced
  dplyr::select(-first_event_time)

anyNA(up_to_colitis_or_event_occur$tstart)

saveRDS(up_to_colitis_or_event_occur, "up_to_colitis_or_event_occur_010724.RDS")
#up_to_colitis_or_event_occur <- readRDS("up_to_colitis_or_event_occur_280624.RDS")

wide_for_km <- up_to_colitis_or_event_occur %>% group_by(PatientDurableKey) %>% slice_max(mth_since_T0)

# Create Surv objects for colitis and other events
surv_object_colitis <- Surv(time = wide_for_km$mth_since_T0, event = wide_for_km$status == 1)
surv_object_other <- Surv(time = wide_for_km$mth_since_T0, event = wide_for_km$status == 2)

# Fit the Kaplan-Meier models
km_fit_colitis <- survfit(surv_object_colitis ~ 1, data = wide_for_km)
km_fit_other <- survfit(surv_object_other ~ 1, data = wide_for_km)

# Combine the Kaplan-Meier fits for colitis and other events
km_fit_combined <- list(colitis = km_fit_colitis, other_events = km_fit_other)

# Plot the Kaplan-Meier curves for the combined events with confidence intervals
ggsurvplot_combine(
  fit = km_fit_combined,
  conf.int = TRUE,
  risk.table = TRUE,
  xlab = "Months Since Checkpoint Inhibitor Administration",
  ylab = "Risk",
  ggtheme = theme_minimal(),
  palette = c("#E7B800", "#2E9FDF"), # Colors for the different events
  legend.labs = c("Colitis", "Competing Events"),
  fun = "event"
) 

# Trial - only to fit a Cox to see will it run 
library(fastDummies)
# # updated_colitis <- dummy_cols(up_to_colitis_or_event_occur, select_columns = c("imputed_RaceEthnicity", "imputed_Smoking_Status", "imputed_Marital_Status"))
# # 
# # cox_model_trial <- coxph(Surv(tstart, tstop, colitis) ~ imputed_RaceEthnicity +
# #                           #`imputed_RaceEthnicity_Asian/Southwest Asian/North African` +  `imputed_RaceEthnicity_Black or African American` + `imputed_RaceEthnicity_Latinx` +`imputed_RaceEthnicity_Multi-Race/Ethnicity` + `imputed_RaceEthnicity_Native Hawaiian, Native American, Alaska Native or Other Pacific Islander` + imputed_RaceEthnicity_Other + imputed_RaceEthnicity_White +
# #                            sex + age + imputed_PostCode + imputed_Smoking_Status +
# #                            #`imputed_Smoking_Status_Daily Smoker` + `imputed_Smoking_Status_Former Smoker`+ `imputed_Smoking_Status_Never` + `imputed_Smoking_Status_Occasional Smoker` + `imputed_Smoking_Status_Passive Never Smoker` +
# #                            #imputed_Marital_Status_Partnered + `imputed_Marital_Status_Separated/Divorced`  +imputed_Marital_Status_Single +imputed_Marital_Status_Widowed  +
# #                            imputed_Marital_Status+
# #                            BMI_imputed + PD1:accumulated_pd1_doses + PDL1:accumulated_pdl1_doses + CTLA4:accumulated_ctla4_doses + PD1 + CTLA4 + PDL1+
# #                            #PD1:CTLA4 + PDL1:CTLA4 + no one has both medication together in the cohort
# #                            Durvalumab + Atezolizumab + Pembrolizumab + Nivolumab + Ipilimumab + Cemiplimab + Vibostolimab_Pembrolizumab +
# #                            #ppi + no one ever taken ppi in the cohort
# #                            nsaid + antibiotics +
# #                            immunodisorder + gord + ulcer + ibs + other_gi_disorder + melanoma + other_skin_tumour + nsclc + other_lung_tumour + rcc + head_n_neck_cancer + appendectomy +
# #                            depression + bipolar + other_mood_disorder + anxiety + schizophrenia_related_psychosis,
# #                            #other_psy +
# #                            id = PatientDurableKey, data = updated_colitis, iter.max = 1000, lambda = 0.001)
# # summary(cox_model_trial)
# # 
# # # Try to look at it with Fine Gray regression
# up_to_colitis_or_event_occur$status_factor <- factor(up_to_colitis_or_event_occur$status, 0:2, labels = c("censor", "colitis", "death/treatment_discon"))
# up_to_colitis_or_event_occur <- up_to_colitis_or_event_occur %>%
#   filter(!is.na(tstart))
# fgdata <- finegray(Surv(tstart, tstop, status_factor) ~ PatientDurableKey + White +
#            #`imputed_RaceEthnicity_Asian/Southwest Asian/North African` +  `imputed_RaceEthnicity_Black or African American` + `imputed_RaceEthnicity_Latinx` +`imputed_RaceEthnicity_Multi-Race/Ethnicity` + `imputed_RaceEthnicity_Native Hawaiian, Native American, Alaska Native or Other Pacific Islander` + imputed_RaceEthnicity_Other + imputed_RaceEthnicity_White +
#            sex + age + weighted_median_ADI_STATERNK_imputed+ weighted_mean_miles_to_UCSF_imputed + weighted_mean_hours_to_UCSF_imputed+
#            imputed_Smoking_Status +
#            #`imputed_Smoking_Status_Daily Smoker` + `imputed_Smoking_Status_Former Smoker`+ `imputed_Smoking_Status_Never` + `imputed_Smoking_Status_Occasional Smoker` + `imputed_Smoking_Status_Passive Never Smoker` +
#            #imputed_Marital_Status_Partnered + `imputed_Marital_Status_Separated/Divorced`  +imputed_Marital_Status_Single +imputed_Marital_Status_Widowed  +
#            imputed_Marital_Status+
#            #BMI_imputed +  commented out because BMI_before_CI_imputed is the baseline
#            BMI_before_CI_imputed +
#            PD1:accumulated_pd1_doses + PDL1:accumulated_pdl1_doses + CTLA4:accumulated_ctla4_doses + PD1 + CTLA4 + PDL1+
#            #PD1:CTLA4 + PDL1:CTLA4 + no one has both medication together in the cohort
#            Durvalumab + Atezolizumab + Pembrolizumab + Nivolumab + Ipilimumab + Cemiplimab + Vibostolimab_Pembrolizumab +
#            #ppi + no one ever taken ppi in the cohort
#            nsaid + antibiotics +
#            immunodisorder + gord + ulcer + ibs + other_gi_disorder + melanoma + other_skin_tumour + nsclc + other_lung_tumour + rcc + head_n_neck_cancer + appendectomy +
#            depression + bipolar + other_mood_disorder + anxiety + schizophrenia_related_psychosis,
#          #other_psy +
#          id = PatientDurableKey, data = up_to_colitis_or_event_occur)
# # 
# # fgfit <- survival::coxph(Surv(fgstart, fgstop, fgstatus)~ imputed_RaceEthnicity +
# #                  #`imputed_RaceEthnicity_Asian/Southwest Asian/North African` +  `imputed_RaceEthnicity_Black or African American` + `imputed_RaceEthnicity_Latinx` +`imputed_RaceEthnicity_Multi-Race/Ethnicity` + `imputed_RaceEthnicity_Native Hawaiian, Native American, Alaska Native or Other Pacific Islander` + imputed_RaceEthnicity_Other + imputed_RaceEthnicity_White + 
# #                  sex + age + imputed_PostCode + imputed_Smoking_Status +
# #                  #`imputed_Smoking_Status_Daily Smoker` + `imputed_Smoking_Status_Former Smoker`+ `imputed_Smoking_Status_Never` + `imputed_Smoking_Status_Occasional Smoker` + `imputed_Smoking_Status_Passive Never Smoker` + 
# #                  #imputed_Marital_Status_Partnered + `imputed_Marital_Status_Separated/Divorced`  +imputed_Marital_Status_Single +imputed_Marital_Status_Widowed  + 
# #                  imputed_Marital_Status+
# #                  BMI_imputed + 
# #                  accumulated_pd1_doses + accumulated_pdl1_doses + accumulated_ctla4_doses + PD1 + PDL1 + CTLA4 +
# #                  #PD1:CTLA4 + PDL1:CTLA4 + PD1:PDL1 +# no one has both medication together in the cohort
# #                  Durvalumab + Atezolizumab + Pembrolizumab + Nivolumab + Ipilimumab + Cemiplimab + Vibostolimab_Pembrolizumab + 
# #                  #ppi + no one ever taken ppi in the cohort
# #                  nsaid + antibiotics + 
# #                  immunodisorder + gord + ulcer + ibs + other_gi_disorder + melanoma + other_skin_tumour + nsclc + other_lung_tumour + rcc + head_n_neck_cancer + appendectomy + 
# #                  depression + bipolar + other_mood_disorder + anxiety + schizophrenia_related_psychosis, id = PatientDurableKey ,data = fgdata, weights = fgwt)
# # 
# # summary(fgfit)
# 
# #### GLMNET
# 
# # Create dummy variables using model.matrix
# # Create dummy variables using model.matrix and exclude reference levels
# race_dummies <- model.matrix(~ imputed_RaceEthnicity - 1, fgdata) %>%
#   as.data.frame() %>%
#   dplyr::select(-imputed_RaceEthnicityWhite)
# 
# postcode_dummies <- model.matrix(~ imputed_PostCode - 1, fgdata) %>%
#   as.data.frame() %>%
#   dplyr::select(-imputed_PostCode941)
# 
# smoking_dummies <- model.matrix(~ imputed_Smoking_Status - 1, fgdata) %>%
#   as.data.frame() %>%
#   dplyr::select(-imputed_Smoking_StatusNever)
# 
# marital_dummies <- model.matrix(~ imputed_Marital_Status - 1, fgdata) %>%
#   as.data.frame() %>%
#   dplyr::select(-imputed_Marital_StatusSingle)
# 
# # Combine dummy variables with the original data (excluding original categorical columns and Fine-Gray variables)
# X <- fgdata %>%
#   dplyr::select(-imputed_RaceEthnicity, -imputed_PostCode, -imputed_Smoking_Status, -imputed_Marital_Status, -fgstart, -fgstop, -fgstatus) %>%
#   cbind(race_dummies, postcode_dummies, smoking_dummies, marital_dummies)
# 
# # Recode the sex variable
# X <- X %>%
#   mutate(PatientDurableKey = as.character(PatientDurableKey)) %>%
#   mutate(across(where(is.factor), ~ as.numeric(.) - 1))
# 
# X$age_scale <- scale(X$age, center = T, scale = T)
# X$accumulated_pd1_doses_scale <- scale(X$accumulated_pd1_doses, center = T, scale = T)
# X$accumulated_pdl1_doses_scale <- scale(X$accumulated_pdl1_doses, center = T, scale = T)
# X$accumulated_ctla4_doses_scale <- scale(X$accumulated_ctla4_doses, center = T, scale = T)
# X$BMI_imputed_scale <- scale(X$BMI_imputed, center = T, scale = T)
# 
# # Convert the data frame to a numeric matrix
# X_matrix <- data.matrix(X %>% dplyr::select(-fgwt, -PatientDurableKey, -sex, -age, -accumulated_pd1_doses, -accumulated_pdl1_doses, -accumulated_ctla4_doses, -BMI_imputed))
# 
# # Create the survival object `y`
# y <- Surv(fgdata$fgstart, fgdata$fgstop, fgdata$fgstatus)
# 
# y2 <- Surv(fgdata$fgstop, fgdata$fgstatus)
# # 
# # # Fit the glmnet model
# # lasso_fit <- glmnet(X_matrix, y, family = "cox", weights = X$fgwt, id = factor(X$PatientDurableKey), trace.it = 1, maxit = 1e+09, standardize = F)
# # 
# # lasso_fit_trial <- glmnet(X_matrix, y, family = "cox", weights = X$fgwt, id = factor(X$PatientDurableKey), trace.it = 1, maxit = 1e+09, standarize = F)
# # View(coef(lasso_fit_trial))
# # 
# # cv_result <- cv.glmnet(X_matrix, y = y, weights = fgdata$fgwt, family = "cox", trace.it = 1, id = X$PatientDurableKey, standardize = F, maxit = 1e+09, lambda = lasso_fit$lambda)
# # sink("output_model.txt")
# # print(cv_result)
# # lasso_fit_cv <- glmnet(X_matrix, y, family = "cox", weights = X$fgwt, id = X$PatientDurableKey, trace.it = 1, maxit = 1e+09, scale = F, lambda = cv_result$lambda.min)
# # coef(lasso_fit_cv)
# # lasso_fit_cv_1se <- glmnet(X_matrix, y, family = "cox", weights = X$fgwt, id = X$PatientDurableKey, trace.it = 1, maxit = 1e+09, scale = F, lambda = cv_result$lambda.1se)
# # coef(lasso_fit_cv_1se)
# # sink()
# #The 1se model will generally have fewer nonzero coefficients (sparser model) because it corresponds to a larger λ value, which increases regularization and thus shrinks more coefficients to zero.
# 
# # With one row per participant, with time recorded
# 
# str(up_to_colitis_or_event_occur)
# 
# # Convert factor variables to numeric (0 and 1)
# up_to_colitis_or_event_occur_numeric <- up_to_colitis_or_event_occur %>%
#   mutate(across(c(PD1, PDL1, CTLA4, Durvalumab, Atezolizumab, Pembrolizumab, Nivolumab, Ipilimumab, Cemiplimab, Vibostolimab_Pembrolizumab, antibiotics, nsaid, immunodisorder, gord, ulcer, ibs, other_gi_disorder, melanoma, other_skin_tumour, nsclc, other_lung_tumour, rcc, head_n_neck_cancer, appendectomy, depression, bipolar, other_mood_disorder, anxiety, schizophrenia_related_psychosis), ~ as.integer(.) - 1))
# 
# # Summarize whether the participant ever had specific conditions or treatments
# ever_conditions_treatments <- up_to_colitis_or_event_occur_numeric %>%
#   group_by(PatientDurableKey) %>%
#   summarize(across(c(PD1, PDL1, CTLA4, Durvalumab, Atezolizumab, Pembrolizumab, Nivolumab, Ipilimumab, Cemiplimab, Vibostolimab_Pembrolizumab, antibiotics, nsaid, immunodisorder, gord, ulcer, ibs, other_gi_disorder, melanoma, other_skin_tumour, nsclc, other_lung_tumour, rcc, head_n_neck_cancer, appendectomy, depression, bipolar, other_mood_disorder, anxiety, schizophrenia_related_psychosis), max, na.rm = TRUE))
# 
# # Extract the last observation for each participant
# last_observation <- up_to_colitis_or_event_occur %>%
#   arrange(PatientDurableKey, desc(mth_since_T0)) %>%
#   group_by(PatientDurableKey) %>%
#   slice(1) %>%
#   ungroup()
# 
# # Extract baseline characteristics
# baseline_data <- up_to_colitis_or_event_occur %>%
#   arrange(PatientDurableKey, mth_since_T0) %>%
#   group_by(PatientDurableKey) %>%
#   slice(1) %>%
#   ungroup() %>%
#   dplyr::select(PatientDurableKey, imputed_RaceEthnicity, sex, age,  weighted_mean_miles_to_UCSF_imputed,weighted_mean_hours_to_UCSF_imputed, weighted_median_ADI_STATERNK_imputed  , imputed_Smoking_Status, BMI_imputed, imputed_Marital_Status)
# 
# # Get the final values for cumulative doses
# final_doses <- last_observation %>%
#   dplyr::select(PatientDurableKey, accumulated_pd1_doses, accumulated_pdl1_doses, accumulated_ctla4_doses)
# 
# # Combine all data
# final_data <- last_observation %>%
#   dplyr::select(PatientDurableKey, mth_since_T0, status) %>%
#   rename(month_to_event_or_censoring = mth_since_T0) %>%
#   left_join(baseline_data, by = "PatientDurableKey") %>%
#   left_join(ever_conditions_treatments, by = "PatientDurableKey") %>%
#   left_join(final_doses, by = "PatientDurableKey")
# 
# # List of variables to convert from numeric 0/1 to factors
# binary_vars <- c("PD1", "PDL1", "CTLA4", "Durvalumab", "Atezolizumab", "Pembrolizumab", 
#                  "Nivolumab", "Ipilimumab", "Cemiplimab", "Vibostolimab_Pembrolizumab", 
#                  "antibiotics", "nsaid", "immunodisorder", "gord", "ulcer", "ibs", 
#                  "other_gi_disorder", "melanoma", "other_skin_tumour", "nsclc", 
#                  "other_lung_tumour", "rcc", "head_n_neck_cancer", "appendectomy", 
#                  "depression", "bipolar", "other_mood_disorder", "anxiety", 
#                  "schizophrenia_related_psychosis")
# 
# # Convert specified variables to factors
# final_data <- final_data %>%
#   mutate(across(all_of(binary_vars), ~ factor(.)))
# 
# # View the final dataset
# str(final_data)
# 
# # Identify the continuous variables
# continuous_vars <- c("age", "BMI_imputed", "accumulated_pd1_doses", "accumulated_pdl1_doses", "accumulated_ctla4_doses")
# 
# final_data_to_standard <- final_data%>%
#   filter(month_to_event_or_censoring > 0)
# 
# # Standardize the continuous 
# population_sd <- function(x) {
#   n <- length(x)
#   sqrt(sum((x - mean(x))^2) / n)
# }
# final_data_standard <- final_data_to_standard %>%
#   mutate(across(all_of(continuous_vars), ~ (. - mean(.)) / population_sd(.))) %>%
#   mutate(PatientDurableKey = as.character(PatientDurableKey))
# 
# str(final_data_standard)
# # Relevel the factors
# final_data_no_key <- final_data_standard %>%
#   mutate(across(where(is.factor), as.character)) %>%
#   mutate(across(where(is.character), ~ ifelse(is.na(.), "Unknown", .))) %>%
#   mutate(across(where(is.character), as.factor)) %>%
#   mutate(imputed_RaceEthnicity = relevel(imputed_RaceEthnicity, ref = "White"), 
#          imputed_PostCode = relevel(imputed_PostCode, ref = "941"), 
#          imputed_Smoking_Status = relevel(imputed_Smoking_Status, ref = "Never"), 
#          imputed_Marital_Status = relevel(imputed_Marital_Status, ref = "Single"))
# 
# # Ensure all factors are converted to characters and handle NAs
# final_data_no_key <- final_data_no_key %>%
#   mutate(across(where(is.factor), as.character)) %>%
#   mutate(across(where(is.character), ~ ifelse(is.na(.), "Unknown", .))) %>%
#   mutate(across(where(is.character), as.factor)) %>%
#   mutate(imputed_RaceEthnicity = relevel(imputed_RaceEthnicity, ref = "White"), 
#          imputed_PostCode = relevel(imputed_PostCode, ref = "941"), 
#          imputed_Smoking_Status = relevel(imputed_Smoking_Status, ref = "Never"), 
#          imputed_Marital_Status = relevel(imputed_Marital_Status, ref = "Single"))
# 
# 
# # Create dummy variables for the relevant columns excluding the reference levels
# final_data_standard <- final_data_no_key %>%
#   dummy_cols(select_columns = c("imputed_RaceEthnicity", "imputed_PostCode", "imputed_Smoking_Status", "imputed_Marital_Status"),
#              remove_first_dummy = TRUE,
#              remove_selected_columns = TRUE) %>%
#   mutate(PatientDurableKey = as.character(PatientDurableKey))
# 
# # Check the column names to ensure reference levels are excluded
# colnames(final_data_standard)
# 
# # Ensure column names are properly quoted
# colnames(final_data_standard) <- make.names(colnames(final_data_standard))
# 
# 
# # Fit glmnet cause-specific cox
# str(final_data_standard)
# 
# y_colitis <- Surv(time = final_data_standard$month_to_event_or_censoring , event = final_data_standard$status == 1)
# y_compete <- Surv(time = final_data_standard$month_to_event_or_censoring, event = final_data_standard$status == 2)
# 
# # Specify the columns to be selected
# selected_columns <- c(
#   "imputed_RaceEthnicity_Asian.Southwest.Asian.North.African", "imputed_RaceEthnicity_Black.or.African.American", 
#   "imputed_RaceEthnicity_Latinx", "imputed_RaceEthnicity_Multi.Race.Ethnicity", 
#   "imputed_RaceEthnicity_Native.Hawaiian..Native.American..Alaska.Native.or.Other.Pacific.Islander", "imputed_RaceEthnicity_Other", 
#   "sex", "age", "imputed_PostCode_902", "imputed_PostCode_911", "imputed_PostCode_913", 
#   "imputed_PostCode_916", "imputed_PostCode_920", "imputed_PostCode_921", "imputed_PostCode_922", 
#   "imputed_PostCode_926", "imputed_PostCode_930", "imputed_PostCode_931", "imputed_PostCode_932", 
#   "imputed_PostCode_934", "imputed_PostCode_935", "imputed_PostCode_936", "imputed_PostCode_937", 
#   "imputed_PostCode_939", "imputed_PostCode_940", "imputed_PostCode_943", "imputed_PostCode_944", 
#   "imputed_PostCode_945", "imputed_PostCode_946", "imputed_PostCode_947", "imputed_PostCode_948", 
#   "imputed_PostCode_949", "imputed_PostCode_950", "imputed_PostCode_951", "imputed_PostCode_952", 
#   "imputed_PostCode_953", "imputed_PostCode_954", "imputed_PostCode_955", "imputed_PostCode_956", 
#   "imputed_PostCode_957", "imputed_PostCode_958", "imputed_PostCode_959", "imputed_PostCode_960", 
#   "imputed_PostCode_961", "imputed_PostCode_967", "imputed_PostCode_968", "imputed_PostCode_972", 
#   "imputed_PostCode_974", "imputed_PostCode_976", "imputed_PostCode_977", "imputed_PostCode_980", 
#   "imputed_PostCode_986", "imputed_PostCode_Non.Cali", "imputed_Smoking_Status_Daily.Smoker", 
#   "imputed_Smoking_Status_Former.Smoker", "imputed_Smoking_Status_Occasional.Smoker", 
#   "imputed_Smoking_Status_Passive.Never.Smoker", "BMI_imputed", "imputed_Marital_Status_Partnered", 
#   "imputed_Marital_Status_Separated.Divorced", "imputed_Marital_Status_Widowed", 
#   "PD1", "PDL1", "CTLA4", "Durvalumab", "Atezolizumab", "Pembrolizumab", "Nivolumab", 
#   "Ipilimumab", "Cemiplimab", "Vibostolimab_Pembrolizumab", "accumulated_pd1_doses", 
#   "accumulated_pdl1_doses", "accumulated_ctla4_doses", "antibiotics", "nsaid", 
#   "immunodisorder", "gord", "ulcer", "ibs", "other_gi_disorder", "melanoma", 
#   "other_skin_tumour", "nsclc", "other_lung_tumour", "rcc", "head_n_neck_cancer", 
#   "appendectomy", "depression", "bipolar", "other_mood_disorder", "anxiety", 
#   "schizophrenia_related_psychosis"
# )
# 
# # Select the columns from the data frame
# selected_data <- final_data_standard %>%
#   dplyr::select(all_of(selected_columns))
# 
# str(selected_data)
# 
# # Convert all factor columns to integer
# selected_data <- selected_data %>%
#   mutate(across(where(is.factor), ~ as.integer(.) - 1))
# 
# str(selected_data)
# 
# # Convert to matrix
# selected_matrix <- as.matrix(selected_data)
# 
# # Check the result
# str(selected_matrix)
# 
# # Check for non-positive event times
# non_positive_times <- which(y_colitis[, 1] <= 0)
# if (length(non_positive_times) > 0) {
#   # Remove rows with non-positive event times
#   y_colitis <- y_colitis[-non_positive_times, ]
#   y_compete <- y_compete[-non_positive_times,]
#   selected_matrix <- selected_matrix[-non_positive_times, ]
# }
# 
# # This is doing glmnet, you should try grpreg::grpSurv
# 
# glm_colitis <- glmnet(x = selected_matrix, y = y_colitis, family = "cox", standardize = FALSE)
# coefs_colitis <- as.matrix(coef(glm_colitis))
# colitis_lambda<- glm_colitis$lambda
# log_colitis_lambda <- log(colitis_lambda)
# 
# # Perform cross-validation, cv.grpsurv is the eqv to group LASSO
# cv_fit_colitis <- cv.glmnet(x = selected_matrix, y = y_colitis, family = "cox", type.measure = "deviance", nfolds = 20, lambda = colitis_lambda, standardize = FALSE)
# # Plot the cross-validation results
# plot(cv_fit_colitis)
# # Get the optimal lambda value
# optimal_lambda_colitis <- cv_fit_colitis$lambda.min
# # Get the coefficients for the optimal lambda
# coef(cv_fit_colitis, s = "lambda.min")
# 
# plot(log_colitis_lambda, coefs_colitis[1, ], type = "l", xlab = "Log(λ)", ylab = "Coefficients",
#      main = "LASSO Coefficient Paths for colitis hazard model", ylim = range(coefs_colitis))
# for (i in 2:nrow(coefs_colitis)) {
#   lines(log_colitis_lambda, coefs_colitis[i, ], col = i)
# }
# abline(v = log(optimal_lambda_colitis), col = "red", lwd = 2, lty = 2)
# 
# sink("output_model_colitis.txt")
# # Fit the Cox modelfor colitis using glmnet
# glm_colitis_final <- glmnet(x = selected_matrix, y = y_colitis, family = "cox", lambda = optimal_lambda_colitis, standardize = FALSE)
# summary(glm_colitis_final)
# coef(glm_colitis_final)
# glm_colitis_final$lambda
# sink()
# 
# # Fit Model for competing risk
# 
# glm_compete <- glmnet(x = selected_matrix, y = y_compete, family = "cox", standardize = FALSE)
# coefs_compete <- as.matrix(coef(glm_compete))
# glm_compete_lambda <- glm_compete$lambda
# log_compete_lambda <- log(glm_compete_lambda)
#   
# # Perform cross-validation
# cv_fit_compete <- cv.glmnet(x = selected_matrix, y = y_compete, family = "cox", type.measure = "deviance", nfolds = 20, lambda = glm_compete_lambda, standardize = FALSE)
# # Plot the cross-validation results
# plot(cv_fit_compete)
# # Get the optimal lambda value
# optimal_lambda_compete <- cv_fit_compete$lambda.min
# # Get the coefficients for the optimal lambda
# coef(cv_fit_compete, s = "lambda.min")
# 
# plot(log_compete_lambda, coefs_compete[1, ], type = "l", xlab = "Log(λ)", ylab = "Coefficients",
#      main = "LASSO Coefficient Paths for competing risk hazard model", ylim = range(coefs_compete))
# for (i in 2:nrow(coefs_compete)) {
#   lines(log_compete_lambda, coefs_compete[i, ], col = i)
# }
# abline(v = log(optimal_lambda_compete), col = "red", lwd = 2, lty = 2)
# 
# sink("output_model_compete.txt")
# glm_compete_final <- glmnet(x = selected_matrix, y = y_compete, family = "cox", lambda = optimal_lambda_compete, standardize = FALSE)
# coef(glm_compete_final)
# glm_compete_final$lambda
# sink()
# 
# # Now try group lasso - as it will standardize to fit and unstandardize for an interpretable coefficient, will unscale everything
# # This code is not capable to include time-varying covariate, so we use baseline and see what will this happen. Just simply for DATASCI224 project and fun!
# 
# # Take fgdata first
# str(fgdata)
# 
# # Take only the baseline information
# # Step 1: Filter out `fgwt != 1`
# filtered_data_fg1 <- fgdata %>%
#   filter(fgwt == 1)
# 
# # Step 2: Select baseline information for the specified columns
# baseline_data <- filtered_data_fg1 %>%
#   filter(fgstart == 0) %>%
#   dplyr::select(
#     PatientDurableKey, imputed_RaceEthnicity, sex, age, weighted_mean_miles_to_UCSF_imputed, weighted_mean_hours_to_UCSF_imputed, weighted_median_ADI_STATERNK_imputed,
#     imputed_Smoking_Status, imputed_Marital_Status, BMI_imputed,
#     PD1, PDL1, CTLA4,
#     immunodisorder, gord, ulcer, 
#     ibs, other_gi_disorder, melanoma, other_skin_tumour, nsclc, 
#     other_lung_tumour, rcc, head_n_neck_cancer, appendectomy, depression, 
#     bipolar, other_mood_disorder, anxiety, schizophrenia_related_psychosis
#   )
# 
# # Step 3: Get the final `fgstatus` and `fgstop` for each `PatientDurableKey`
# final_status_data <- filtered_data_fg1 %>%
#   group_by(PatientDurableKey) %>%
#   summarize(
#     end_status = last(fgstatus),
#     time_in_study_mth = last(fgstop)
#   ) %>%
#   ungroup()
# 
# # Step 4: Merge the baseline data with the final status data
# result_data <- baseline_data %>%
#   left_join(final_status_data, by = "PatientDurableKey")
# 
# # View the resulting data
# head(result_data)
# 
# # Relevel the factors
# final_data_no_key_unstandard <- result_data %>%
#   mutate(across(where(is.factor), as.character)) %>%
#   mutate(across(where(is.character), ~ ifelse(is.na(.), "Unknown", .))) %>%
#   mutate(across(where(is.character), as.factor)) %>%
#   mutate(imputed_RaceEthnicity = relevel(imputed_RaceEthnicity, ref = "White"), 
#          imputed_PostCode = relevel(imputed_PostCode, ref = "941"), 
#          imputed_Smoking_Status = relevel(imputed_Smoking_Status, ref = "Never"), 
#          imputed_Marital_Status = relevel(imputed_Marital_Status, ref = "Single"))
# 
# # Ensure all factors are converted to characters and handle NAs
# final_data_no_key_unstandard <- final_data_no_key_unstandard %>%
#   mutate(across(where(is.factor), as.character)) %>%
#   mutate(across(where(is.character), ~ ifelse(is.na(.), "Unknown", .))) %>%
#   mutate(across(where(is.character), as.factor)) %>%
#   mutate(imputed_RaceEthnicity = relevel(imputed_RaceEthnicity, ref = "White"), 
#          imputed_PostCode = relevel(imputed_PostCode, ref = "941"), 
#          imputed_Smoking_Status = relevel(imputed_Smoking_Status, ref = "Never"), 
#          imputed_Marital_Status = relevel(imputed_Marital_Status, ref = "Single"))
# 
# 
# # Create dummy variables for the relevant columns excluding the reference levels
# final_data_unstandard <- final_data_no_key_unstandard %>%
#   dummy_cols(select_columns = c("imputed_RaceEthnicity", "imputed_PostCode", "imputed_Smoking_Status", "imputed_Marital_Status"),
#              remove_first_dummy = TRUE,
#              remove_selected_columns = TRUE) %>%
#   mutate(PatientDurableKey = as.character(PatientDurableKey))
# 
# # Check the column names to ensure reference levels are excluded
# colnames(final_data_unstandard)
# 
# # Ensure column names are properly quoted
# colnames(final_data_unstandard) <- make.names(colnames(final_data_unstandard))
# 
# # Fit glmnet cause-specific cox
# str(final_data_unstandard)
# 
# # Create a new data frame with the last status for each PatientDurableKey
# last_status_df <- up_to_colitis_or_event_occur %>%
#   arrange(PatientDurableKey, YearMonth) %>%
#   group_by(PatientDurableKey) %>%
#   slice_tail(n = 1) %>%
#   ungroup() %>%
#   dplyr::select(PatientDurableKey, status, status_factor, YearMonth)
# 
# y_colitis <- Surv(time = final_data_unstandard$time_in_study_mth , event = last_status_df$status == 1)
# y_compete <- Surv(time = final_data_unstandard$time_in_study_mth, event = last_status_df$status == 2)
# 
# # Specify the columns to be selected
# selected_columns <- c(
#   "imputed_RaceEthnicity_Asian.Southwest.Asian.North.African", "imputed_RaceEthnicity_Black.or.African.American", 
#   "imputed_RaceEthnicity_Latinx", "imputed_RaceEthnicity_Multi.Race.Ethnicity", 
#   "imputed_RaceEthnicity_Native.Hawaiian..Native.American..Alaska.Native.or.Other.Pacific.Islander", "imputed_RaceEthnicity_Other", 
#   "sex", "age", "imputed_PostCode_902", "imputed_PostCode_911", "imputed_PostCode_913", 
#   "imputed_PostCode_916", "imputed_PostCode_920", "imputed_PostCode_921", "imputed_PostCode_922", 
#   "imputed_PostCode_926", "imputed_PostCode_930", "imputed_PostCode_931", "imputed_PostCode_932", 
#   "imputed_PostCode_934", "imputed_PostCode_935", "imputed_PostCode_936", "imputed_PostCode_937", 
#   "imputed_PostCode_939", "imputed_PostCode_940", "imputed_PostCode_943", "imputed_PostCode_944", 
#   "imputed_PostCode_945", "imputed_PostCode_946", "imputed_PostCode_947", "imputed_PostCode_948", 
#   "imputed_PostCode_949", "imputed_PostCode_950", "imputed_PostCode_951", "imputed_PostCode_952", 
#   "imputed_PostCode_953", "imputed_PostCode_954", "imputed_PostCode_955", "imputed_PostCode_956", 
#   "imputed_PostCode_957", "imputed_PostCode_958", "imputed_PostCode_959", "imputed_PostCode_960", 
#   "imputed_PostCode_961", "imputed_PostCode_967", "imputed_PostCode_968", "imputed_PostCode_972", 
#   "imputed_PostCode_974", "imputed_PostCode_976", "imputed_PostCode_977", "imputed_PostCode_980", 
#   "imputed_PostCode_986", "imputed_PostCode_Non.Cali", "imputed_Smoking_Status_Daily.Smoker", 
#   "imputed_Smoking_Status_Former.Smoker", "imputed_Smoking_Status_Occasional.Smoker", 
#   "imputed_Smoking_Status_Passive.Never.Smoker", "BMI_imputed","PD1", "PDL1","CTLA4", "imputed_Marital_Status_Partnered", 
#   "imputed_Marital_Status_Separated.Divorced", "imputed_Marital_Status_Widowed", 
#   "immunodisorder", "gord", "ulcer", "ibs", "other_gi_disorder", "melanoma", 
#   "other_skin_tumour", "nsclc", "other_lung_tumour", "rcc", "head_n_neck_cancer", 
#   "appendectomy", "depression", "bipolar", "other_mood_disorder", "anxiety", 
#   "schizophrenia_related_psychosis"
# )
# 
# # Select the columns from the data frame
# selected_data_unstand <- final_data_unstandard %>%
#   dplyr::select(all_of(selected_columns))
# 
# str(selected_data_unstand)
# 
# # Convert all factor columns to integer
# selected_data_unstand <- selected_data_unstand %>%
#   mutate(across(where(is.factor), ~ as.integer(.) - 1))
# 
# # Create new columns overweight and obesity based on BMI_imputed
# selected_data_unstand <- selected_data_unstand %>%
#   mutate(
#     overweight = ifelse(BMI_imputed >= 25 & BMI_imputed < 30, 1, 0),
#     obesity = ifelse(BMI_imputed >= 30, 1, 0)
#   )
# 
# str(selected_data_unstand)
# 
# # Convert to matrix
# selected_matrix_unstand <- as.matrix(selected_data_unstand)
# 
# # Check the result
# str(selected_matrix_unstand)
# 
# # Define the number of columns in your selected matrix
# nn <- ncol(selected_matrix_unstand)
# 
# # Initialize the group vector with zeros
# group <- rep(0, nn)
# 
# colnames(selected_data_unstand)
# # Assign group numbers to the specified ranges
# # Define the group vector based on the column names
# group <- c(
#   rep(1, 6),    # Race/Ethnicity
#   2,            # Sex
#   3,            # Age
#   rep(4, 45),   # PostCode
#   rep(5, 4),    # Smoking Status
#   6,            # BMI
#   7:9,
#   rep(10, 3),    # Marital Status
#   11:29          # Medical conditions
# )
# group <- as.factor(group)
# length(group)
# 
# print(group)
# 
# # Fit model with colitis
# glmsurv_colitis <- grpsurv(X = selected_matrix_unstand, y = y_colitis, penalty = "grLasso", group = group, returnX = T)
# glmsurv_colitis_lambda <- glmsurv_colitis$lambda
# log_glmsurv_colitis_lambda <- log(glmsurv_colitis_lambda)
# coefs_glmsurv_colitis <- as.matrix(coef(glmsurv_colitis))
# 
# # Cross validation for glmsurv - colitis
# cv_fit_glmsurv_colitis <- cv.grpsurv(X = selected_matrix_unstand, y= y_colitis, group = group, nfolds = 20, lambda = glmsurv_colitis_lambda)
# 
# # Find the index of the minimum value in cve
# min_cve_index <- which.min(cv_fit_glmsurv_colitis$cve)
# 
# # Get the corresponding lambda value
# optimal_lambda <- cv_fit_glmsurv_colitis$lambda[min_cve_index]
# 
# # Get 1se value
# min_cv <- cv_fit_glmsurv_colitis$cve[min_cve_index]
# min_cvse <- cv_fit_glmsurv_colitis$cvse[min_cve_index]
# one_se_threshold <- min_cv + min_cvse
# 
# lambda_1se <- max(cv_fit_glmsurv_colitis$lambda[cv_fit_glmsurv_colitis$cve < one_se_threshold])
# 
# png("cv_colitis_010724_010724.png", width = 500, height = 500)
# plot(cv_fit_glmsurv_colitis)
# abline(v = log(optimal_lambda), col = "gray", lwd = 1, lty = 2)
# abline(v = log(lambda_1se), col = "black", lwd = 1, lty = 2)
# #optimal_lambda_glmsurv_colitis <- cv_fit_colitis$lambda.min
# #coef(cv_fit_glmsurv_colitis, s = "lambda.min")
# dev.off()
# 
# # Print the index and the corresponding lambda value
# cat("The index of the minimum cve for colitis is:", min_cve_index, "\n")
# cat("The corresponding lambda value for colitis is:", optimal_lambda, "\n")
# 
# png("coef_path_colitis_010724.png",width = 600, height = 500)
# plot(log_glmsurv_colitis_lambda, coefs_glmsurv_colitis[1, ], type = "l", xlab = "Log(λ)", ylab = "Coefficients",
#      main = "", ylim = range(coefs_glmsurv_colitis))
# for (i in 2:nrow(coefs_glmsurv_colitis)) {
#   lines(log_glmsurv_colitis_lambda, coefs_glmsurv_colitis[i, ], col = i)
# }
# 
# # Add vertical lines for the optimal lambda and 1-SE lambda
# abline(v = log(optimal_lambda), col = "red", lwd = 1, lty = 2)
# abline(v = log(lambda_1se), col = "blue", lwd = 1, lty = 2)
# 
# # Calculate the number of non-zero coefficients for each lambda
# n_groups_selected <- apply(coefs_glmsurv_colitis, 2, function(x) sum(x != 0))
# 
# # Add top axis with the number of groups selected
# axis(3, at = log_glmsurv_colitis_lambda, labels = n_groups_selected, tick = FALSE, line = -0.5)
# mtext("Groups selected", side = 3, line = 2, cex = 0.8)
# 
# # Add the main title, moving it upward by adjusting the 'line' parameter
# title(main = "Group LASSO Coefficient Paths for Colitis Hazard Model", line = 3)
# dev.off()
# 
# glmsurv_colitis_coef <- as.data.frame(coef(glmsurv_colitis))
# 
# sink("output_model_colitis_grouplasso_010724.txt")
# cat("The index of the minimum cve for colitis risk is:", min_cve_index, "\n")
# cat("The minimum cve for colitis risk is:", min_cv, "\n")
# cat("The corresponding lambda value for colitis risk is:", optimal_lambda, "\n")
# cat("lambda.1se:", lambda_1se, "\n")
# # Fit the Cox modelfor colitis using glmnet
# lambda_position <- which.min(abs(glmsurv_colitis$lambda - optimal_lambda))
# # Number of columns in glmsurv_colitis$beta
# corresponding_beta_column_colitis_min <- glmsurv_colitis$beta[, lambda_position]
# print(corresponding_beta_column_colitis_min)
# cat("\n","coef of lambda.1se:", "\n")
# # Fit the Cox model for compete using glmnet
# lambda_position <- which.min(abs(glmsurv_colitis$lambda - lambda_1se))
# # Number of columns in glmsurv_colitis$beta
# corresponding_beta_column_colitis_1se <- glmsurv_colitis$beta[, lambda_position]
# print(corresponding_beta_column_colitis_1se)
# sink()
# 
# beta_df_colitis_min <- data.frame(Predictor = names(corresponding_beta_column_colitis_min), 
#                       Coefficient = as.numeric(corresponding_beta_column_colitis_min), 
#                       stringsAsFactors = FALSE)
# 
# # Print the data frame
# print(beta_df_colitis_min)
# 
# beta_df_colitis_min<- beta_df_colitis_min %>% mutate(Coefficient = round(Coefficient, 2))
# 
# # Create a new Word document
# doc <- read_docx()
# 
# # Add the table to the document
# doc <- doc %>%
#   body_add_table(value =beta_df_colitis_min, style = "table_template")
# 
# # Save the document
# print(doc, target = "colitis_coefficients_table_rounded.docx")
# 
# # Fit model with competing event
# glmsurv_compete <- grpsurv(X = selected_matrix_unstand, y = y_compete, penalty = "grLasso", group = group, returnX = T)
# glmsurv_compete_lambda <- glmsurv_compete$lambda
# log_glmsurv_compete_lambda <- log(glmsurv_compete_lambda)
# coefs_glmsurv_compete <- as.matrix(coef(glmsurv_compete))
# 
# # Cross validation for glmsurv - compete
# cv_fit_glmsurv_compete <- cv.grpsurv(X = selected_matrix_unstand, y= y_compete, group = group, nfolds = 20, lambda = glmsurv_compete_lambda)
# 
# # Find the index of the minimum value in cve
# min_cve_index <- which.min(cv_fit_glmsurv_compete$cve)
# 
# # Get the corresponding lambda value
# optimal_lambda <- cv_fit_glmsurv_compete$lambda[min_cve_index]
# 
# # Get 1se value
# min_cv <- cv_fit_glmsurv_compete$cve[min_cve_index]
# min_cvse <- cv_fit_glmsurv_compete$cvse[min_cve_index]
# one_se_threshold <- min_cv + min_cvse
# 
# lambda_1se <- max(cv_fit_glmsurv_compete$lambda[cv_fit_glmsurv_compete$cve < one_se_threshold])
# 
# png("cv_compete_010724.png", width = 500, height = 500)
# plot(cv_fit_glmsurv_compete)
# abline(v = log(optimal_lambda), col = "gray", lwd = 1, lty = 2)
# abline(v = log(lambda_1se), col = "black", lwd = 1, lty = 2)
# #optimal_lambda_glmsurv_compete <- cv_fit_compete$lambda.min
# #coef(cv_fit_glmsurv_compete, s = "lambda.min")
# dev.off()
# 
# # Print the index and the corresponding lambda value
# cat("The index of the minimum cve for competing risk is:", min_cve_index, "\n")
# cat("The minimum cve for competing risk is:", min_cv, "\n")
# cat("The corresponding lambda value for competing risk is:", optimal_lambda, "\n")
# cat("lambda.1se:", lambda_1se, "\n")
# 
# png("coef_path_compete_010724.png",width = 600, height = 500)
# plot(log_glmsurv_compete_lambda, coefs_glmsurv_compete[1, ], type = "l", xlab = "Log(λ)", ylab = "Coefficients",
#      main = "", ylim = range(coefs_glmsurv_compete))
# for (i in 2:nrow(coefs_glmsurv_compete)) {
#   lines(log_glmsurv_compete_lambda, coefs_glmsurv_compete[i, ], col = i)
# }
# 
# # Add vertical lines for the optimal lambda and 1-SE lambda
# abline(v = log(optimal_lambda), col = "red", lwd = 1, lty = 2)
# abline(v = log(lambda_1se), col = "blue", lwd = 1, lty = 2)
# 
# # Calculate the number of non-zero coefficients for each lambda
# n_groups_selected <- apply(coefs_glmsurv_compete, 2, function(x) sum(x != 0))
# 
# # Add top axis with the number of groups selected
# axis(3, at = log_glmsurv_compete_lambda, labels = n_groups_selected, tick = FALSE, line = -0.5)
# mtext("Groups selected", side = 3, line = 2, cex = 0.8)
# 
# # Add the main title, moving it upward by adjusting the 'line' parameter
# title(main = "Group LASSO Coefficient Paths for Compete Hazard Model", line = 3)
# dev.off()
# 
# sink("output_model_compete_grouplasso_010724.txt")
# cat("The index of the minimum cve for competing risk is:", min_cve_index, "\n")
# cat("The minimum cve for competing risk is:", min_cv, "\n")
# cat("The corresponding lambda value for competing risk is:", optimal_lambda, "\n")
# cat("lambda.1se:", lambda_1se, "\n")
# cat("coef of lambda.min:", "\n")
# # Fit the Cox modelfor compete using glmnet
# lambda_position <- which.min(abs(glmsurv_compete$lambda - optimal_lambda))
# # Number of columns in glmsurv_compete$beta
# corresponding_beta_column_compete_min <- glmsurv_compete$beta[, lambda_position]
# print(corresponding_beta_column_compete_min)
# cat("\n","coef of lambda.1se:", "\n")
# # Fit the Cox modelfor compete using glmnet
# lambda_position <- which.min(abs(glmsurv_compete$lambda - lambda_1se))
# # Number of columns in glmsurv_compete$beta
# corresponding_beta_column_compete_1se <- glmsurv_compete$beta[, lambda_position]
# print(corresponding_beta_column_compete_1se)
# sink()
# 
# beta_df_compete_min <- data.frame(Predictor = names(corresponding_beta_column_compete_min), 
#                                   Coefficient = as.numeric(corresponding_beta_column_compete_min), 
#                                   stringsAsFactors = FALSE)
# 
# # Print the data frame
# print(beta_df_compete_min)
# 
# beta_df_compete_min<- beta_df_compete_min %>% mutate(Coefficient = round(Coefficient, 2))
# 
# # Create a new Word document
# doc <- read_docx()
# 
# # Add the table to the document
# doc <- doc %>%
#   body_add_table(value =beta_df_compete_min, style = "table_template")
# 
# # Save the document
# print(doc, target = "compete_coefficients_table_rounded.docx")
# 
# 
# 
# # Table one - only baseline data
# library(tableone)
# library(flextable)
# library(officer)
# 
# # Create a new categorical column BMI_cat
# result_data <- result_data %>%
#   mutate(BMI_cat = case_when(
#     BMI_imputed < 25 ~ "Normal",
#     BMI_imputed >= 25 & BMI_imputed < 30 ~ "Overweight",
#     BMI_imputed >= 30 ~ "Obesity"
#   ))
# 
# # Convert BMI_cat to a factor
# result_data$BMI_cat <- factor(result_data$BMI_cat, levels = c("Normal", "Overweight", "Obesity"))
# 
# # Display the updated data frame structure
# str(result_data)
# 
# # Define the variables
# variables <- c("imputed_RaceEthnicity", "sex", "age", "weighted_mean_hours_to_UCSF_imputed", "weighted_mean_miles_to_UCSF_imputed", "weighted_median_ADI_STATERNK_imputed", "imputed_Smoking_Status",
#                "imputed_Marital_Status", "BMI_imputed","BMI_cat", "PD1", "PDL1", "CTLA4", "immunodisorder",
#                "gord", "ulcer", "ibs", "other_gi_disorder", "melanoma", "other_skin_tumour", "nsclc",
#                "other_lung_tumour", "rcc", "head_n_neck_cancer", "appendectomy", "depression", "bipolar",
#                "other_mood_disorder", "anxiety", "schizophrenia_related_psychosis")
# 
# # Create the table
# table1 <- CreateTableOne(vars = variables, strata = "end_status", data = result_data, addOverall = TRUE)
# # Convert the tableone object to a matrix
# matrix_table1 <- print(table1, printToggle = FALSE, nonnormal = c("weighted_median_ADI_STATERNK_imputed"))
# 
# # Convert the matrix to a data frame
# df_table1 <- as.data.frame(matrix_table1)
# df_table1$Variable <- rownames(df_table1)
# rownames(df_table1) <- NULL
# df_table1 <- df_table1[, c(ncol(df_table1), 1:(ncol(df_table1) - 1))]
# 
# # Convert the data frame to a flextable object
# ft <- qflextable(df_table1)
# # Create a Word document
# doc <- read_docx()
# 
# # Add a title
# doc <- doc %>%
#   body_add_par("Baseline Statistics", style = "heading 1")
# 
# # Add the table
# doc <- doc %>%
#   body_add_flextable(ft)
# 
# # Save the document
# print(doc, target = "Baseline_Statistics.docx")

# penalized
print(up_to_colitis_or_event_occur)
str(up_to_colitis_or_event_occur)
up_to_colitis_or_event_occur <- up_to_colitis_or_event_occur %>% rename(White = imputed_RaceEthnicity)

time_varying_colitis <- dummy_cols(up_to_colitis_or_event_occur, select_columns = c("imputed_Smoking_Status", "imputed_Marital_Status", "sex"), remove_selected_columns = F)
# Categorizing BMI
time_varying_colitis <- time_varying_colitis %>% mutate(overweight_time_varying = ifelse(BMI_imputed >= 25 & BMI_imputed < 30, 1, 0),
                                                        obesity_time_varying = ifelse(BMI_imputed > 30, 1, 0),
                                                        overweight_baseline = ifelse(BMI_before_CI_imputed >= 25 & BMI_before_CI_imputed < 30, 1, 0),
                                                        obesity_baseline = ifelse(BMI_before_CI_imputed > 30, 1, 0))

time_varying_colitis <- time_varying_colitis %>% mutate(colitis_status = ifelse(status == 1, 1, 0),
                                                        compete_status = ifelse(status == 2, 1, 0))

# Display the first few rows to check the result
head(time_varying_colitis)

str(time_varying_colitis)
#View(time_varying_colitis)

sink("time_to_follow_up.txt")
# Calculate time-to-follow-up for each PatientDurableKey

# Determine which patients ever had a status of 1
patients_with_status_1 <- time_varying_colitis %>%
  group_by(PatientDurableKey) %>%
  summarise(ever_had_status_1 = any(status == "1")) %>%
  ungroup()

# Calculate time-to-follow-up for each PatientDurableKey
time_to_follow_up <- time_varying_colitis %>%
  group_by(PatientDurableKey) %>%
  summarise(time_to_follow_up = max(mth_since_T0) - min(mth_since_T0) + 1) %>%
  ungroup()

# Merge the ever_had_status_1 information with time_to_follow_up
time_to_follow_up <- time_to_follow_up %>%
  left_join(patients_with_status_1, by = "PatientDurableKey")

# Overall median and range
overall_median <- median(time_to_follow_up$time_to_follow_up)
overall_range <- range(time_to_follow_up$time_to_follow_up)

# Print overall median and range
cat("Overall Median Time-to-Follow-Up:", overall_median, "\n")
cat("Overall Range of Time-to-Follow-Up:", overall_range, "\n")

# Median and range stratified by ever having status 1
stratified_median <- time_to_follow_up %>%
  mutate(status_group = ifelse(ever_had_status_1, "Had Status 1", "Never Had Status 1")) %>%
  group_by(status_group) %>%
  summarise(median_time_to_follow_up = median(time_to_follow_up),
            min_time_to_follow_up = min(time_to_follow_up),
            max_time_to_follow_up = max(time_to_follow_up)) %>%
  ungroup()

# Print stratified median and range
stratified_median %>%
  rowwise() %>%
  mutate(range_time_to_follow_up = paste(min_time_to_follow_up, max_time_to_follow_up, sep = " - ")) %>%
  dplyr::select(status_group, median_time_to_follow_up, range_time_to_follow_up) %>%
  print()

sink()

response_colitis <- Surv(time_varying_colitis$tstart, time_varying_colitis$tstop, time_varying_colitis$colitis_status)

colnames(time_varying_colitis)

##### Debug #####

patients_with_C34 <- readRDS("missed_patients_with_C34.rds")
time_varying_colitis <- time_varying_colitis %>%
  mutate(other_lung_tumour = ifelse(PatientDurableKey %in% patients_with_C34$PatientDurableKey, 1, other_lung_tumour))

patients_with_C44 <- readRDS("missed_patients_with_C44.rds")
time_varying_colitis <- time_varying_colitis %>%
  mutate(other_skin_tumour = ifelse(PatientDurableKey %in% patients_with_C44$PatientDurableKey, 1, other_skin_tumour))

patients_with_C64 <- readRDS("missed_patients_with_C64.rds")
time_varying_colitis <- time_varying_colitis %>%
  mutate(rcc = ifelse(PatientDurableKey %in% patients_with_C64$PatientDurableKey, 1, rcc))

patients_with_hnn <- readRDS("patients_with_hnn.rds")
time_varying_colitis <- time_varying_colitis %>%
  mutate(head_n_neck_cancer = ifelse(PatientDurableKey %in% patients_with_hnn$PatientDurableKey, 1, head_n_neck_cancer))

##### Debug #####

# Create a new column for combination therapy dose
time_varying_colitis <- time_varying_colitis %>%
  mutate(
    combo_therapy = ifelse(PD1 == 1 & CTLA4 == 1, pmin(accumulated_pd1_doses, accumulated_ctla4_doses), 0)
  )

# Calculate the accumulated combination therapy doses using cummax
time_varying_colitis <- time_varying_colitis %>%
  group_by(PatientDurableKey) %>%
  mutate(accumulated_pd1_ctla4_dose = cummax(combo_therapy)) %>%
  ungroup()

# View the updated dataset
head(time_varying_colitis)

saveRDS(time_varying_colitis, "time_varying_colitis_180724.RDS")
time_varying_colitis <- readRDS("time_varying_colitis_180724.RDS")

formula <- ~ White+ # Non-white as reference
  imputed_Smoking_Status_Current_Smoker + `imputed_Smoking_Status_Former_Smoker`  + # Never smoked as reference
  sex_female + age + 
  weighted_mean_miles_to_UCSF_imputed + 
  weighted_mean_hours_to_UCSF_imputed +
  weighted_median_ADI_STATERNK_imputed +
  imputed_Marital_Status_Partnered + `imputed_Marital_Status_Separated/Divorced` + imputed_Marital_Status_Widowed + # Single as reference
  BMI_imputed +overweight_time_varying + obesity_time_varying + BMI_before_CI_imputed +overweight_baseline+obesity_baseline+
  sex_female*accumulated_pd1_doses + sex_female*accumulated_pdl1_doses + sex_female*accumulated_ctla4_doses + sex_female*PD1 + sex_female*PDL1 + sex_female*CTLA4 +
  sex_female* (PD1:CTLA4) + #sex_female*(PDL1:CTLA4) + No one with PDL1:CTLA4, although there's one record but only last a month, probably switch of medication and it won't be affecting the model
  sex_female* (accumulated_pd1_ctla4_dose)  + 
  #sex_female* (accumulated_pdl1_doses:accumulated_ctla4_doses)  +
  #sex_female*Durvalumab + sex_female*accumulated_durvalumab_doses +
  #sex_female*Atezolizumab + sex_female*accumulated_atezolizumab_doses+
  #sex_female*Pembrolizumab + sex_female*accumulated_pembrolizumab_doses +
  #sex_female*Nivolumab + sex_female*accumulated_nivolumab_doses +
  #sex_female*Ipilimumab + perfectly collinear with anti-CTLA-4
  #sex_female*accumulated_ipilimumab_doses+ perfectly collinear with anti-CTLA-4
  #sex_female*Cemiplimab + sex_female*accumulated_cemiplimab_doses +
  #sex_female*Vibostolimab_Pembrolizumab + sex_female*accumulated_vibostolimab_pembrolizumab_doses +
  # Individual drug removed as there may be risk of positivity violation
  nsaid + antibiotics + immunodisorder + gord + ulcer + ibs + other_gi_disorder + melanoma + other_skin_tumour + 
  nsclc + other_lung_tumour + rcc + head_n_neck_cancer + appendectomy + depression + bipolar + other_mood_disorder + 
  anxiety + schizophrenia_related_psychosis - 1

model_matrix <- model.matrix(formula, data = time_varying_colitis)
model_matrix <- model_matrix[, colnames(model_matrix) != "PD10"]
colnames(model_matrix)
saveRDS(model_matrix, "model_matrix_010724.rds")
model_matrix <- readRDS("model_matrix_010724.rds")

#View(model_matrix)
dim(model_matrix)
colnames(model_matrix)

saveRDS(response_colitis, "response_colitis_010724.rds")
response_colitis <- readRDS("response_colitis_010724.rds")

# Perform profL1 analysis for colitis
profL1_cox_colitis <- penalized::profL1(response = response_colitis, penalized = model_matrix, model = "cox", standardize = TRUE, trace = TRUE, fold = 10, maxlambda1 = 100, minlambda1 = 1, steps = 25, log = T)
saveRDS(profL1_cox_colitis, "profL1_cox_colitis_010724.rds")
profL1_cox_colitis<- readRDS("profL1_cox_colitis_010724.rds")

colitis_cvls <- profL1_cox_colitis$cvl
colitis_folds <- profL1_cox_colitis$fold
cvls_matrix_colitis <- matrix(unlist(colitis_cvls), ncol = length(colitis_cvls), byrow = TRUE)

std_errors <- apply(cvls_matrix_colitis, 2, function(x) sd(x) / sqrt(length(x)))


# Plot cross-validated log likelihood vs lambda
png(file = "likelihood_vs_lambda_colitis_010724_010724.png", width = 1800, height = 1800, res = 300)
plot(profL1_cox_colitis$lambda, profL1_cox_colitis$cvl, type = "l", xlab = "λ", ylab = "Cross-validated log likelihood", log = "x", xaxt = "n")
axis(1, at = c(1, 10, 100))
dev.off()

# Extract lambda values and CVL values
lambda_values_colitis <- profL1_cox_colitis$lambda
cvl_values_colitis <- profL1_cox_colitis$cvl

# Find the index of the maximum CVL value
max_cvl_index_colitis <- which.max(cvl_values_colitis)
optimal_lambda_L1_colitis <- lambda_values_colitis[max_cvl_index_colitis]
cat("Lambda with maximum likelihood:", optimal_lambda_L1_colitis, "\n")

round_up_to_nearest_5 <- function(x) {
  return(ceiling(x / 5) * 5)
}

round_down_to_nearest_5 <- function(x) {
  return(floor(x / 5) * 5)
}

# Run optL1 analysis for colitis
colitis_optL1 <- penalized::optL1(response = response_colitis, penalized = model_matrix, model = "cox", standardize = TRUE, trace = TRUE, fold = 10,
                                  maxlambda1 = 100,
                                  #maxlambda1 = round_up_to_nearest_5(optimal_lambda_L1_colitis),
                                  minlambda1 = 1
                                  #minlambda1 = round_down_to_nearest_5(optimal_lambda_L1_colitis)
                                  )
saveRDS(colitis_optL1, "optL1_cox_colitis_010724.rds")
colitis_optL1 <- readRDS("optL1_cox_colitis_010724.rds")
optL1_lambda_colitis <- colitis_optL1$lambda

# Save results to a text file
sink("output_model_colitis_lasso_timevarying_010724.txt", split = TRUE)
cat("Lambda with maximum likelihood from profL1:", optimal_lambda_L1_colitis, " with a log likelihood of ", cvl_values_colitis[max_cvl_index_colitis], "\n")
cox_colitis_profL1 <- penalized::penalized(response = response_colitis, penalized = model_matrix, lambda1 = optimal_lambda_L1_colitis, model = "cox", standardize = TRUE, trace = TRUE)
print(sapply(penalized::coefficients(cox_colitis_profL1, "all"), \(x) sprintf("%.3f", x)))
cat("\nLambda with maximum likelihood from optL1:", optL1_lambda_colitis, " with a log likelihood of ", colitis_optL1$cvl, "\n")
cox_colitis_optL1 <- penalized::penalized(response = response_colitis, penalized = model_matrix, lambda1 = optL1_lambda_colitis, model = "cox", standardize = TRUE, trace = TRUE)
print(sapply(penalized::coefficients(cox_colitis_optL1, "all"), \(x) sprintf("%.3f", x)))
sink()

sink("output_model_colitis_lasso_timevarying_010724.txt", split = TRUE)
cat("Lambda with maximum likelihood from profL1:", optimal_lambda_L1_colitis, " with a log likelihood of ", cvl_values_colitis[max_cvl_index_colitis], "\n")
print(sapply(penalized::coefficients(cox_colitis_profL1, "all"), \(x) sprintf("%.3f", x)))
cat("\nLambda with maximum likelihood from optL1:", optL1_lambda_colitis, " with a log likelihood of ", colitis_optL1$cvl, "\n")
print(sapply(penalized::coefficients(cox_colitis_optL1, "all"), \(x) sprintf("%.3f", x)))
sink()

# Plot coefficient paths and add a vertical line for the optimal lambda
png(file = "coef_path_colitis_010724_010724.png", width = 3600, height = 1800, res = 300)
penalized::plotpath(profL1_cox_colitis$fullfit, log = "x", xaxt = "n")
axis(1, at = c(1, 10, 100))
abline(v = optimal_lambda_L1_colitis, col = "red", lty = 2)
abline(v = optL1_lambda_colitis, col = "blue", lty = 2)
dev.off()

# Perform profL1 analysis for competing risks
response_compete <- Surv(time_varying_colitis$tstart, time_varying_colitis$tstop, time_varying_colitis$compete_status)
saveRDS(response_compete, "response_compete_010724.rds")
response_compete<-readRDS("response_compete_010724.rds")

profL1_cox_compete <- penalized::profL1(response = response_compete, penalized = model_matrix, model = "cox", standardize = TRUE, trace = TRUE, fold = 10, 
                                        maxlambda1 = 1000, minlambda1 = 1, steps = 25, log = T)
saveRDS(profL1_cox_compete, "profL1_cox_compete_010724.rds")

# Plot cross-validated log likelihood vs lambda for competing risks
png(file = "likelihood_vs_lambda_compete_010724_010724.png", width = 1800, height = 1800, res = 300)
plot(profL1_cox_compete$lambda, profL1_cox_compete$cvl, type = "l", xlab = "λ", ylab = "Cross-validated log likelihood", log = "x", xaxt = "n")
axis(1, at = c(1, 10, 100, 1000))
dev.off()

# Extract lambda values and CVL values for competing risks
lambda_values_compete <- profL1_cox_compete$lambda
cvl_values_compete <- profL1_cox_compete$cvl

# Find the index of the maximum CVL value for competing risks
max_cvl_index_compete <- which.max(cvl_values_compete)
optimal_lambda_L1_compete <- lambda_values_compete[max_cvl_index_compete]
cat("Lambda with maximum likelihood:", optimal_lambda_L1_compete, "\n")

# Run optL1 analysis for competing risks
compete_optL1 <- penalized::optL1(response = response_compete, penalized = model_matrix, model = "cox", standardize = TRUE, trace = TRUE, fold = 10,
                                  maxlambda1 = 1000,
                                  #maxlambda1 = round_up_to_nearest_5(optimal_lambda_L1_compete),
                                  minlambda1 = 0.1
                                  #minlambda1 = round_down_to_nearest_5(optimal_lambda_L1_compete)
                                  )
saveRDS(compete_optL1, "optL1_cox_compete_010724.rds")
optL1_lambda_compete <- compete_optL1$lambda

# Save results to a text file for competing risks
sink("output_model_compete_lasso_timevarying_010724.txt", split = TRUE)
cat("Lambda with maximum likelihood from profL1:", optimal_lambda_L1_compete, " with a log likelihood of ", cvl_values_compete[max_cvl_index_compete], "\n")
cox_compete_profL1 <- penalized::penalized(response = response_compete, penalized = model_matrix, lambda1 = optimal_lambda_L1_compete, model = "cox", standardize = TRUE, trace = TRUE)
print(penalized::coefficients(cox_compete_profL1, "all"))
cat("\nLambda with maximum likelihood from optL1:", optL1_lambda_compete, " with a log likelihood of ", compete_optL1$cvl, "\n")
cox_compete_optL1 <- penalized::penalized(response = response_compete, penalized = model_matrix, lambda1 = optL1_lambda_compete, model = "cox", standardize = TRUE, trace = TRUE)
print(penalized::coefficients(cox_compete_optL1, "all"))
sink()

sink("output_model_compete_lasso_timevarying_010724.txt", split = TRUE)
cat("Lambda with maximum likelihood from profL1:", optimal_lambda_L1_compete, " with a log likelihood of ", cvl_values_compete[max_cvl_index_compete], "\n")
print(sapply(penalized::coefficients(cox_compete_profL1, "all"), \(x) sprintf("%.3f", x)))
cat("\nLambda with maximum likelihood from optL1:", optL1_lambda_compete, " with a log likelihood of ", compete_optL1$cvl, "\n")
print(sapply(penalized::coefficients(cox_compete_optL1, "all"), \(x) sprintf("%.3f", x)))
sink()

# Plot coefficient paths for competing risks and add a vertical line for the optimal lambda
png(file = "coef_path_compete_010724_010724.png", width = 3600, height = 1800, res = 300)
penalized::plotpath(profL1_cox_compete$fullfit, log = "x", xaxt = "n")
axis(1, at = c(1, 10, 100, 1000))
abline(v = optimal_lambda_L1_compete, col = "red", lty = 2)
abline(v = optL1_lambda_compete, col = "blue", lty = 2)
dev.off()


# Table one
# 
# # Try to look at it with Fine Gray regression
up_to_colitis_or_event_occur$status_factor <- factor(up_to_colitis_or_event_occur$status, 0:2, labels = c("censor", "colitis", "death/treatment_discon"))
up_to_colitis_or_event_occur <- up_to_colitis_or_event_occur %>%
  filter(!is.na(tstart))
up_to_colitis_or_event_occur$White <- up_to_colitis_or_event_occur$imputed_RaceEthnicity
up_to_colitis_or_event_occur$sex_female <- ifelse(up_to_colitis_or_event_occur$sex == "female", 1, 0)
up_to_colitis_or_event_occur$overweight_time_varying <- ifelse(25<=up_to_colitis_or_event_occur$BMI_imputed & up_to_colitis_or_event_occur$BMI_imputed < 30, 1, 0)
up_to_colitis_or_event_occur$obesity_time_varying <- ifelse(up_to_colitis_or_event_occur$BMI_imputed >= 30, 1, 0)
up_to_colitis_or_event_occur$overweight_baseline <- ifelse(25<=up_to_colitis_or_event_occur$BMI_before_CI_imputed & up_to_colitis_or_event_occur$BMI_before_CI_imputed < 30, 1, 0)
up_to_colitis_or_event_occur$obesity_baseline <- ifelse(up_to_colitis_or_event_occur$BMI_before_CI_imputed>=30, 1, 0)


# ####!! TASK!!:: Relevel the factor, and then change the dummy variables below

fgdata <- finegray(Surv(tstart, tstop, status_factor) ~ PatientDurableKey + White+ # Non-white as reference
                     imputed_Smoking_Status + # Never smoked as reference
                     sex_female + age + 
                     weighted_mean_miles_to_UCSF_imputed + 
                     weighted_mean_hours_to_UCSF_imputed +
                     weighted_median_ADI_STATERNK_imputed +
                     imputed_Marital_Status + # Single as reference
                     BMI_imputed +overweight_time_varying + obesity_time_varying + BMI_before_CI_imputed +overweight_baseline+obesity_baseline+
                     sex_female*accumulated_pd1_doses + sex_female*accumulated_pdl1_doses + sex_female*accumulated_ctla4_doses + sex_female*PD1 + sex_female*PDL1 + sex_female*CTLA4 +
                     sex_female* (PD1:CTLA4) + sex_female*(PDL1:CTLA4) + 
                     sex_female* (accumulated_pd1_doses:accumulated_ctla4_doses)  + 
                     sex_female* (accumulated_pdl1_doses:accumulated_ctla4_doses)  +
                     sex_female*Durvalumab + sex_female*accumulated_durvalumab_doses +
                     sex_female*Atezolizumab + sex_female*accumulated_atezolizumab_doses+
                     sex_female*Pembrolizumab + sex_female*accumulated_pembrolizumab_doses +
                     sex_female*Nivolumab + sex_female*accumulated_nivolumab_doses +
                     #sex_female*Ipilimumab + perfectly collinear with anti-CTLA-4
                     #sex_female*accumulated_ipilimumab_doses+ perfectly collinear with anti-CTLA-4
                     sex_female*Cemiplimab + sex_female*accumulated_cemiplimab_doses +
                     sex_female*Vibostolimab_Pembrolizumab + sex_female*accumulated_vibostolimab_pembrolizumab_doses +
                     nsaid + antibiotics + immunodisorder + gord + ulcer + ibs + other_gi_disorder + melanoma + other_skin_tumour + 
                     nsclc + other_lung_tumour + rcc + head_n_neck_cancer + appendectomy + depression + bipolar + other_mood_disorder + 
                     anxiety + schizophrenia_related_psychosis,
                   id = PatientDurableKey, data = up_to_colitis_or_event_occur)

# Take fgdata first
str(fgdata)

# Take only the baseline information
# Step 1: Filter out `fgwt != 1`
filtered_data_fg1 <- fgdata %>%
  filter(fgwt == 1)

# Step 2: Select baseline information for the specified columns
baseline_data <- filtered_data_fg1 %>%
  filter(fgstart == 0) %>%
  dplyr::select(
    PatientDurableKey, White, sex_female, age, weighted_mean_miles_to_UCSF_imputed, weighted_mean_hours_to_UCSF_imputed, weighted_median_ADI_STATERNK_imputed,
    imputed_Smoking_Status, imputed_Marital_Status, BMI_before_CI_imputed,
    PD1, PDL1, CTLA4, PD1:CTLA4, PDL1:CTLA4,
    immunodisorder, gord, ulcer, 
    ibs, other_gi_disorder, melanoma, other_skin_tumour, nsclc, 
    other_lung_tumour, rcc, head_n_neck_cancer, appendectomy, depression, 
    bipolar, other_mood_disorder, anxiety, schizophrenia_related_psychosis
  )

# Step 3: Get the final `fgstatus` and `fgstop` for each `PatientDurableKey`
final_status_data <- filtered_data_fg1 %>%
  group_by(PatientDurableKey) %>%
  arrange(PatientDurableKey, fgstart) %>%
  summarize(
    end_status = last(fgstatus),
    time_in_study_mth = last(fgstop),
    pd1_acc_dose = last(accumulated_pd1_doses),
    pdl1_acc_dose = last(accumulated_pdl1_doses),
    ctla4_acc_dose = last(accumulated_ctla4_doses),
    pd1_ctla4_acc_dose = sum(ifelse(PD1 == 1 & CTLA4 == 1, 1, 0))
  ) %>%
  ungroup()

# Step 4: Merge the baseline data with the final status data
result_data <- baseline_data %>%
  left_join(final_status_data, by = "PatientDurableKey")

# View the resulting data
head(result_data)

# Create a new categorical column BMI_cat
result_data <- result_data %>%
  mutate(BMI_cat = case_when(
    BMI_before_CI_imputed < 25 ~ "Normal",
    BMI_before_CI_imputed >= 25 & BMI_before_CI_imputed  < 30 ~ "Overweight",
    BMI_before_CI_imputed >= 30 ~ "Obesity"
  ))

# Convert BMI_cat to a factor
result_data$BMI_cat <- factor(result_data$BMI_cat, levels = c("Normal", "Overweight", "Obesity"))

# Display the updated data frame structure
str(result_data)

result_data$White <- factor(result_data$White, levels = c(0, 1), labels = c("Non-White", "White"))
result_data$sex_female <- factor(result_data$sex_female, levels = c(0, 1), labels = c("Male", "Female"))

# Define the variables
variables <- c("White", "sex_female", "age", "weighted_mean_hours_to_UCSF_imputed", "weighted_mean_miles_to_UCSF_imputed", "weighted_median_ADI_STATERNK_imputed", "imputed_Smoking_Status",
               "imputed_Marital_Status", "BMI_before_CI_imputed","BMI_cat", "PD1", "PDL1", "CTLA4","pd1_acc_dose","pdl1_acc_dose","ctla4_acc_dose","pd1_ctla4_acc_dose", "immunodisorder",
               "gord", "ulcer", "ibs", "other_gi_disorder", "melanoma", "other_skin_tumour", "nsclc",
               "other_lung_tumour", "rcc", "head_n_neck_cancer", "appendectomy", "depression", "bipolar",
               "other_mood_disorder", "anxiety", "schizophrenia_related_psychosis", "time_in_study_mth")

library(tableone)
# Create the table
table1 <- CreateTableOne(vars = variables, strata = "end_status", data = result_data, addOverall = TRUE)
# Convert the tableone object to a matrix
matrix_table1 <- print(table1, printToggle = FALSE, nonnormal = c("weighted_median_ADI_STATERNK_imputed", "age", "weighted_mean_hours_to_UCSF_imputed", "weighted_mean_miles_to_UCSF_imputed","pd1_acc_dose","pdl1_acc_dose","ctla4_acc_dose","pd1_ctla4_acc_dose", "time_in_study_mth"))

# Convert the matrix to a data frame
df_table1 <- as.data.frame(matrix_table1)
df_table1$Variable <- rownames(df_table1)
rownames(df_table1) <- NULL
df_table1 <- df_table1[, c(ncol(df_table1), 1:(ncol(df_table1) - 1))]

library(flextable)
# Convert the data frame to a flextable object
ft <- qflextable(df_table1)
library(officer)
# Create a Word document
doc <- read_docx()

# Add a title
doc <- doc %>%
  body_add_par("Baseline Statistics", style = "heading 1")

# Add the table
doc <- doc %>%
  body_add_flextable(ft)

# Save the document
print(doc, target = "Baseline_Statistics_2.docx")


# Define custom functions for median, 25th percentile, and 75th percentile
median_nonzero <- function(x) {
  x <- x[x != 0]
  if (length(x) == 0) {
    return(NA)
  } else {
    return(median(x, na.rm = TRUE))
  }
}

quantile_nonzero <- function(x, probs) {
  x <- x[x != 0]
  if (length(x) == 0) {
    return(NA)
  } else {
    return(quantile(x, probs = probs, na.rm = TRUE))
  }
}

iqr_nonzero <- function(x) {
  q1 <- quantile_nonzero(x, 0.25)
  q3 <- quantile_nonzero(x, 0.75)
  return(c(q1, q3))
}

# Define the variables
variables <- c("White", "sex_female", "age", "weighted_mean_hours_to_UCSF_imputed", "weighted_mean_miles_to_UCSF_imputed", "weighted_median_ADI_STATERNK_imputed", "imputed_Smoking_Status",
               "imputed_Marital_Status", "BMI_before_CI_imputed","BMI_cat", "PD1", "PDL1", "CTLA4","pd1_acc_dose","pdl1_acc_dose","ctla4_acc_dose","pd1_ctla4_acc_dose", "immunodisorder",
               "gord", "ulcer", "ibs", "other_gi_disorder", "melanoma", "other_skin_tumour", "nsclc",
               "other_lung_tumour", "rcc", "head_n_neck_cancer", "appendectomy", "depression", "bipolar",
               "other_mood_disorder", "anxiety", "schizophrenia_related_psychosis", "time_in_study_mth")

# Create the table
table1 <- CreateTableOne(vars = variables, strata = "end_status", data = result_data, addOverall = TRUE,
                         factorVars = c("White", "sex_female", "imputed_Smoking_Status", "imputed_Marital_Status", "BMI_cat", "PD1", "PDL1", "CTLA4",
                                        "immunodisorder", "gord", "ulcer", "ibs", "other_gi_disorder", "melanoma",
                                        "other_skin_tumour", "nsclc", "other_lung_tumour", "rcc", "head_n_neck_cancer",
                                        "appendectomy", "depression", "bipolar", "other_mood_disorder", "anxiety",
                                        "schizophrenia_related_psychosis"))

# Convert the tableone object to a matrix and apply the custom median functions
matrix_table1 <- print(table1, printToggle = FALSE, nonnormal = c("weighted_median_ADI_STATERNK_imputed", "age", "weighted_mean_hours_to_UCSF_imputed", "weighted_mean_miles_to_UCSF_imputed",
                                                                  "pd1_acc_dose","pdl1_acc_dose","ctla4_acc_dose","pd1_ctla4_acc_dose", "time_in_study_mth"))

# Extract the columns with doses and apply custom functions to calculate medians and IQR ranges
doses <- result_data %>%
  summarize(
    pd1_acc_dose_median = median_nonzero(pd1_acc_dose),
    pdl1_acc_dose_median = median_nonzero(pdl1_acc_dose),
    ctla4_acc_dose_median = median_nonzero(ctla4_acc_dose),
    pd1_ctla4_acc_dose_median = median_nonzero(pd1_ctla4_acc_dose),
    pd1_acc_dose_iqr = paste(iqr_nonzero(pd1_acc_dose), collapse = " - "),
    pdl1_acc_dose_iqr = paste(iqr_nonzero(pdl1_acc_dose), collapse = " - "),
    ctla4_acc_dose_iqr = paste(iqr_nonzero(ctla4_acc_dose), collapse = " - "),
    pd1_ctla4_acc_dose_iqr = paste(iqr_nonzero(pd1_ctla4_acc_dose), collapse = " - ")
  )

# Print the medians and IQR ranges for non-zero values
print(doses)

# Extract the columns with doses and apply custom functions to calculate medians and IQR ranges stratified by end_status
doses_by_status <- result_data %>%
  group_by(end_status) %>%
  summarize(
    pd1_acc_dose_median = median_nonzero(pd1_acc_dose),
    pdl1_acc_dose_median = median_nonzero(pdl1_acc_dose),
    ctla4_acc_dose_median = median_nonzero(ctla4_acc_dose),
    pd1_ctla4_acc_dose_median = median_nonzero(pd1_ctla4_acc_dose),
    pd1_acc_dose_iqr = paste(iqr_nonzero(pd1_acc_dose), collapse = " - "),
    pdl1_acc_dose_iqr = paste(iqr_nonzero(pdl1_acc_dose), collapse = " - "),
    ctla4_acc_dose_iqr = paste(iqr_nonzero(ctla4_acc_dose), collapse = " - "),
    pd1_ctla4_acc_dose_iqr = paste(iqr_nonzero(pd1_ctla4_acc_dose), collapse = " - ")
  ) %>%
  ungroup()

# Print the medians and IQR ranges for non-zero values stratified by end_status
print(doses_by_status)

# Correlation matrix between variables
cor_matrix <- cor(model_matrix)

library(corrplot)
plot.new()
dev.off()
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.6, order = "hclust")

# Flatten the correlation matrix into a long format
cor_long <- as.data.frame(as.table(cor_matrix))

# Filter out correlations between -0.3 and 0.3 and correlations with itself
cor_filtered <- cor_long %>%
  filter(Var1 != Var2 & (Freq <= -0.3 | Freq >= 0.3))

# Rename columns for clarity
colnames(cor_filtered) <- c("Variable1", "Variable2", "Correlation")

# Display the filtered correlations
print(cor_filtered)
write.csv(cor_filtered, "filtered_correlations.csv", row.names = FALSE)

# Load necessary libraries
library(dplyr)

# Convert model_matrix to a data frame to work with column names
model_df <- as.data.frame(model_matrix)

# Create a new variable combining the smoking status
model_df$smoking_status_combined <- ifelse(model_df$imputed_Smoking_Status_Current_Smoker == 1, "Current Smoker",
                                           ifelse(model_df$imputed_Smoking_Status_Former_Smoker == 1, "Former Smoker", "Non Smoker"))

# Function to calculate risk ratio and its confidence interval
calculate_risk_ratio <- function(contingency_table, reference_group) {
  risk_ratios <- c()
  lower_bounds <- c()
  upper_bounds <- c()
  
  for (i in 1:nrow(contingency_table)) {
    if (rownames(contingency_table)[i] != reference_group) {
      a <- contingency_table[i, 2]
      b <- sum(contingency_table[i, ]) - a
      c <- contingency_table[reference_group, 2]
      d <- sum(contingency_table[reference_group, ]) - c
      risk_ratio <- (a / (a + b)) / (c / (c + d))
      se_log_rr <- sqrt((1 / a) + (1 / c) - (1 / (a + b)) - (1 / (c + d)))
      ci_lower <- exp(log(risk_ratio) - 1.96 * se_log_rr)
      ci_upper <- exp(log(risk_ratio) + 1.96 * se_log_rr)
      
      risk_ratios <- c(risk_ratios, risk_ratio)
      lower_bounds <- c(lower_bounds, ci_lower)
      upper_bounds <- c(upper_bounds, ci_upper)
    } else {
      risk_ratios <- c(risk_ratios, 1)  # Reference group risk ratio is 1
      lower_bounds <- c(lower_bounds, NA)
      upper_bounds <- c(upper_bounds, NA)
    }
  }
  
  return(data.frame(Risk_Ratio = risk_ratios, CI_Lower = lower_bounds, CI_Upper = upper_bounds))
}

# Cancer outcomes to analyze
cancer_outcomes <- c("other_lung_tumour1", "nsclc1", "melanoma1", "other_skin_tumour1", "rcc1", "head_n_neck_cancer1")
cancer_labels <- c("Other Lung Cancer", "NSCLC", "Melanoma", "Other Skin Tumour", "RCC", "Head and Neck Cancer")

# Initialize lists to store results
risk_ratios_list <- list()
chi_sq_tests_list <- list()

# Loop through each cancer outcome
for (i in 1:length(cancer_outcomes)) {
  cancer <- cancer_outcomes[i]
  label <- cancer_labels[i]
  
  # Create contingency table
  contingency_table <- table(model_df$smoking_status_combined, model_df[[cancer]])
  colnames(contingency_table) <- c(paste0("No ", label), label)
  
  # Calculate risk ratios and confidence intervals
  reference_group <- "Non Smoker"
  risk_ratios_df <- calculate_risk_ratio(contingency_table, reference_group)
  
  # Add smoking status
  risk_ratios_df <- cbind(Smoking_Status = rownames(contingency_table), risk_ratios_df)
  colnames(risk_ratios_df)[2] <- paste0("Risk_Ratio_", gsub(" ", "_", label))
  
  # Add to list
  risk_ratios_list[[cancer]] <- risk_ratios_df
  
  # Chi-square test
  chi_sq_test <- chisq.test(contingency_table)
  chi_sq_tests_list[[cancer]] <- chi_sq_test
  
  # Print results
  cat("\nRisk Ratios and 95% CI for Smoking Status vs.", label, ":\n")
  print(risk_ratios_df)
  cat("\nChi-square test results for smoking status vs.", label, ":\n")
  print(chi_sq_test)
}

# Combine risk ratios into one data frame
combined_risk_ratios <- Reduce(function(x, y) merge(x, y, by = "Smoking_Status"), risk_ratios_list)

# Save the combined risk ratios to a CSV file
write.csv(combined_risk_ratios, "combined_risk_ratios_with_CI.csv", row.names = FALSE)

# Save chi-square test results to a text file
sink("chi_square_test_results.txt")
for (i in 1:length(cancer_outcomes)) {
  cancer <- cancer_outcomes[i]
  label <- cancer_labels[i]
  
  cat("\nChi-square test results for smoking status vs.", label, ":\n")
  print(chi_sq_tests_list[[cancer]])
}
sink()




# Define the cancer columns
cancer_columns <- c("melanoma", "other_skin_tumour", "nsclc", "other_lung_tumour", "rcc", "head_n_neck_cancer")

# Convert factor columns to numeric
time_varying_colitis_new <- time_varying_colitis %>%
  mutate(across(all_of(cancer_columns), ~ as.numeric(as.character(.))))

# Calculate the number of cancers per patient
patient_cancer_counts <- time_varying_colitis_new %>%
  group_by(PatientDurableKey) %>%
  summarise(across(all_of(cancer_columns), max, na.rm = TRUE)) %>%
  mutate(num_cancers = rowSums(across(all_of(cancer_columns)) == 1)) %>%
  ungroup()

# Identify patients with more than or equal to 2 cancers and count each combination
cancer_combinations <- patient_cancer_counts %>%
  filter(num_cancers >= 2)

# Count each unique combination of cancers
combination_counts <- cancer_combinations %>%
  group_by(across(all_of(cancer_columns))) %>%
  summarise(count = n()) %>%
  ungroup()

# Calculate the total percentage of patients with more than or equal to 2 cancers
total_patients <- n_distinct(time_varying_colitis$PatientDurableKey)
patients_with_multiple_cancers_count <- n_distinct(cancer_combinations$PatientDurableKey)

percentage_multiple_cancers <- (patients_with_multiple_cancers_count / total_patients) * 100

# Print the percentage
print(percentage_multiple_cancers)

# Save the combination counts and percentage to a txt file
# Calculate the percentage for each combination
combination_counts <- combination_counts %>%
  mutate(Percentage = (count / sum(count)) * 100)

# Save as CSV file
write.csv(combination_counts, "cancer_combinations.csv", row.names = FALSE, col.names = TRUE)

# Display the updated data frame
print(combination_counts)
write.table(data.frame(percentage_multiple_cancers), "percentage_multiple_cancers.txt", row.names = FALSE, col.names = TRUE)

# Display combination counts
combination_counts

library(dplyr)

# Define lung cancer and non-lung cancer columns
lung_cancer_columns <- c("nsclc", "other_lung_tumour")
non_lung_cancer_columns <- setdiff(cancer_columns, lung_cancer_columns)

# Calculate the number of lung and non-lung cancers per patient
patient_cancer_counts <- time_varying_colitis_new %>%
  group_by(PatientDurableKey) %>%
  filter(imputed_Smoking_Status_Current_Smoker == 1) %>%
  summarise(across(all_of(cancer_columns), max, na.rm = TRUE)) %>%
  ungroup()

# Create a new column indicating if the patient has any lung cancer
patient_cancer_counts <- patient_cancer_counts %>%
  mutate(lung_cancer = ifelse(rowSums(across(all_of(lung_cancer_columns))) >= 1, 1, 0),
         non_lung_cancer = ifelse(rowSums(across(all_of(non_lung_cancer_columns))) >= 1, 1, 0))

# Create a labeled contingency table
contingency_table <- table(patient_cancer_counts$lung_cancer, patient_cancer_counts$non_lung_cancer)
dimnames(contingency_table) <- list("Lung Cancer" = c("No", "Yes"), "Non-Lung Cancer" = c("No", "Yes"))

# Perform chi-square test
chi_square_test <- chisq.test(contingency_table)

# Calculate the odds ratio
odds_ratio <- (contingency_table[2, 2] * contingency_table[1, 1]) / (contingency_table[1, 2] * contingency_table[2, 1])

# Save all results to a single text file
output_file <- "lung_vs_non_lung_cancer_results_in_smokers.txt"
sink(output_file)

cat("Contingency Table:\n")
print(contingency_table)

cat("\nChi-Square Test Results:\n")
print(chi_square_test)

cat("\nOdds Ratio:\n")
print(odds_ratio)

sink() # Close the sink

# Optionally, you can read back the results to check the content
file.show(output_file)




# Univariate Cox for each variable

# Define the response variable (Surv object) for your data

up_to_colitis_or_event_occur <- readRDS("up_to_colitis_or_event_occur_010724.RDS")

# Assuming you have a response variable defined
response <- Surv(time = up_to_colitis_or_event_occur$tstart, 
                 time2 = up_to_colitis_or_event_occur$tstop, 
                 event = up_to_colitis_or_event_occur$status)

# Convert model_matrix back to a data frame
model_df <- as.data.frame(model_matrix)
colnames(model_df)<- make.names(colnames(model_df))
model_df_2 <- cbind(up_to_colitis_or_event_occur$tstart,up_to_colitis_or_event_occur$tstop,up_to_colitis_or_event_occur$status, up_to_colitis_or_event_occur$PatientDurableKey , model_df)

# List of predictor variables
predictors <- colnames(model_df)

# Initialize an empty list to store the results
univariate_results <- list()

# Loop over each predictor variable
for (predictor in predictors) {
  # Ensure the predictor is properly quoted if it contains special characters
  predictor_quoted <- ifelse(grepl("[[:punct:] ]", paste0("`\`", predictor, "\``")), predictor, predictor)
  print(predictor_quoted)
  formula <- as.formula(paste("response ~", predictor_quoted))
  print(formula)
  model <- coxph(formula, data = model_df_2, id = model_df_2$`up_to_colitis_or_event_occur$PatientDurableKey`)
  univariate_results[[predictor]] <- summary(model)$coefficients
}

# Convert the results list to a data frame for easier viewing
univariate_results_df <- do.call(rbind, univariate_results)
univariate_results_df <- as.data.frame(univariate_results_df)
colnames(univariate_results_df) <- c("coef", "exp(coef)", "se(coef)","robust se", "z", "Pr(>|z|)")

# Print the univariate results
print(univariate_results_df)

variables_of_interest <- names(penalized::coefficients(cox_colitis_optL1))
variables_of_interest[9] <- "X.imputed_Marital_Status_Separated.Divorced."

# Create the formula for the Cox model
formula_multi <- as.formula(paste("response ~", paste(variables_of_interest, collapse = " + ")))

# Fit the multivariate Cox model
multivariate_cox_model <- coxph(formula_multi, data = model_df_2, id = `up_to_colitis_or_event_occur$PatientDurableKey`)

# Print the summary of the model
summary(multivariate_cox_model)

ggplot(model_df_2, aes(x = `up_to_colitis_or_event_occur$tstop`, y = BMI_imputed, group = `up_to_colitis_or_event_occur$PatientDurableKey`, color = as.factor(`up_to_colitis_or_event_occur$PatientDurableKey`))) +
  geom_line(alpha = 0.3) +
  labs(title = "Change in BMI Over Time for Each Patient",
       x = "Time (months)",
       y = "BMI") +
  theme_minimal() +
  theme(legend.position = "none")  # Hides the legend for clarity
