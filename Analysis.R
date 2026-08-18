# PACKAGES

library(dplyr)
library(tidyr)
library(readr)
library(janitor)
library(ggplot2)
library(ggalluvial)

Datafull <- read_csv("Data1.csv")

# Original HIPE dataset: all admissions, 2016–2024 (n = 952,059)

View(Datafull)

## Excluding all childrens Age<20 years (0-4, 5-9, 10-14, 15-19)

Data1 <- Datafull[Datafull$age_group >=5, ]

# 22,359 admissions excluded
# Remaining admissions: n = 929,700

table(Data1$age_group)

##All codes
UTIcodes <- c("N390", "N391", "N393", "N394","N10", "N110", "N111", "N118","N119","N300","N301","N308","N309","T835")
# T835 was used as recorded in the HIPE dataset.

table(Data1$dx01[Data1$dx01 %in% UTIcodes])

# IDENTIFY ADMISSIONS WITH UTI

# Flag admissions with a UTI diagnosis in any diagnosis field (dx01–dx30)
Data1 <- Data1 %>%
  mutate(UTI_flag = if_any(dx01:dx30, ~ . %in% UTIcodes))
table(Data1$UTI_flag)

### OtherUTIcodes <- c("Y846", "N398", "N341","N342","N17","N18","N19", "A415","A419","A412", "A414", "R650","R651", "R652")
table(Data1$dx01[Data1$dx01 %in% OtherUTIcodes])
table(Data1$dx02[Data1$dx02 %in% OtherUTIcodes])
table(Data1$dx03[Data1$dx03 %in% OtherUTIcodes])
Data1 <- Data1 %>%
  mutate(OtherUTIflag = if_any(dx01:dx30, ~ . %in% OtherUTIcodes))

table(Data1$OtherUTIflag)
table(Data1$UTI_flag, Data1$OtherUTIflag)

AllUTIcodes <- c("Y846", "N398", "N341","N342","N17","N18","N19", "A415","A419","A412", "A414", "R650","R651", "R652",
                 "N390", "N391", "N393", "N394","N10", "N110", "N111", "N118","N119","N300","N301","N308","N309","T835")
table(Data1$dx01[Data1$dx01 %in% AllUTIcodes])
table(Data1$dx02[Data1$dx02 %in% AllUTIcodes])
table(Data1$dx03[Data1$dx03 %in% AllUTIcodes])

Data1 <- Data1 %>%
  mutate(AllUTIflag = if_any(dx01:dx30, ~ . %in% AllUTIcodes))
table(Data1$AllUTIflag)

# From allUTIflag 732013 were excluded as they are no UTI related code

DataNoUTI <- Data1[Data1$AllUTIflag ==FALSE, ]
table(DataNoUTI$dx01[DataNoUTI$dx01 %in% AllUTIcodes])
table(DataNoUTI$dx02[DataNoUTI$dx02 %in% AllUTIcodes])
table(DataNoUTI$dx03[DataNoUTI$dx03 %in% AllUTIcodes])

###  Z491 (578755), N179 (9887) some common diagnosis in excluded records
sort(table(DataNoUTI$dx01), decreasing = TRUE)


Data1 <- Data1[Data1$UTI_flag ==TRUE, ]
#### Save data with only  UTI in Data1
saveRDS(Data1, "Data1.rds")

#### the dataset now contains (n=163,282) keeping only UTI admissions

####checking the type of admission (elem)
table(Data1$elem)

# Exclude maternity admissions
Data1_clean <- Data1 %>%
  filter(elem != "Maternity")

## checking elem if maternal data is removed
table(Data1_clean$elem)

### Now the dataset Data1_clean contains (n= 160861) after removing maternity admissions 

## checking the year of admission
table(Data1_clean$year_adm)

####Removing admissions before study period (keep only 2020–2024)

Data1_clean <- Data1_clean %>%
  filter(year_adm >= 2020 & year_adm <= 2024)

## checking the year of admission remaining
table(Data1_clean$year_adm)


# Final analytical UTI cohort: n = 159,576

### Assigning ID for each admission
Data1_clean$ID <- 1:nrow(Data1_clean)

Data1_clean <- Data1_clean[, c("ID", setdiff(names(Data1_clean), "ID"))]
Data1_clean

###### performing the UTIcodes check in the filtered dataset Data1_clean

UTIcodes <- c("N390", "N391", "N393", "N394","N10", "N110", "N111", "N118","N119","N300","N301","N308","N309","T835")
# T835 was used as recorded in the HIPE dataset.

table(Data1_clean$UTI_flag)

##### Classification into UTI groups

#### Create variable for CA-UTI as primary diagnosis - 
# Identify UTI recorded as the primary diagnosis (dx01)

Data1_clean$CA_UTI <- ifelse(Data1_clean$dx01 %in% UTIcodes, "Yes", "No")

table(Data1_clean$CA_UTI)


# Identify UTI in secondary diagnosis fields (dx02-dx30)
for(i in 2:30){
  
  dx_var  <- paste0("dx", sprintf("%02d", i))
  uti_var <- paste0("UTIdx", sprintf("%02d", i))
  
  Data1_clean[[uti_var]] <- ifelse(
    Data1_clean[[dx_var]] %in% UTIcodes,
    "Yes",
    "No"
  )
}

#### # Define secondary UTI and corresponding hospital-acquired flag columns
uti_cols <- paste0("UTIdx", sprintf("%02d", 2:30))
ha_cols  <- paste0("hadx", sprintf("%02d", 2:30))

# Count HA-UTI occurrences across secondary diagnosis fields

Data1_clean$UTI_HA_secondary_count <- rowSums(
  (Data1_clean[uti_cols] == "Yes") & (Data1_clean[ha_cols] == 1),
  na.rm = TRUE
)

# Check distribution
table(Data1_clean$UTI_HA_secondary_count)

### 13447  +  368  +    2 =  13817  this is total of hospital acquired uti diagnosis for dx02-dx30

#####  checking Hospital acquired UTI  in dx02 & hadx02 = 1 
Data1_clean$HA_UTIdx02_flag <- ifelse(Data1_clean$UTIdx02 == "Yes" & Data1_clean$hadx02 == 1, "Yes", "No")
table(Data1_clean$HA_UTIdx02_flag)

#### HA-UTI as secondary diagnosis
# Flag admissions with at least one hospital-acquired UTI
Data1_clean$UTI_HA_secondary_flag <-
  Data1_clean$UTI_HA_secondary_count > 0

# Check final HA-UTI classification
table(Data1_clean$UTI_HA_secondary_flag)

## note- this variable UTI_HA_secondary flag include the total of Patients with hospital-acquired UTI (13817)

### Checking overlap between primary-diagnosis UTI and secondary HA-UTI

table(Data1_clean$UTI_HA_secondary_flag, Data1_clean$CA_UTI)

View(Data1_clean[Data1_clean$UTI_HA_secondary_flag == TRUE & Data1_clean$CA_UTI == "Yes", ])

# 224 admissions had a UTI as the principal diagnosis and
# also met the secondary HA-UTI definition

## first creating a new column called Ca_UTI_Primary_final copying evreything from CA_UTI so that original variable is not destroyed
Data1_clean$CA_UTI_Primary_final <- Data1_clean$CA_UTI

### Now R searches for patients satisfying both conditions- 1) patient has HA_UTI and condition 2) patients currently labelled as CA-UTI so if one patient is having both the conditions then it gives command to create a copy of CA-UTI that remove patients who also have hospital-acquired UTI 
### so means removing 224 patients from CA_UTI 
# Exclude admissions that also meet the HA-UTI definition

Data1_clean$CA_UTI_Primary_final[Data1_clean$UTI_HA_secondary_flag == TRUE & Data1_clean$CA_UTI_Primary_final == "Yes"] <- "No"

table(Data1_clean$CA_UTI_Primary_final)

table(Data1_clean$UTI_HA_secondary_flag, Data1_clean$CA_UTI_Primary_final)


## Community acqiured UTI in secondary diagnosis

uti_mat <- as.matrix(Data1_clean[uti_cols]) == "Yes"
ha_mat  <- is.na(as.matrix(Data1_clean[ha_cols]))

Data1_clean$UTI_CA_secondary_flag <- rowSums(uti_mat & ha_mat, na.rm = TRUE) > 0

table(Data1_clean$UTI_CA_secondary_flag)

table(Data1_clean$UTI_CA_secondary_flag, Data1_clean$UTI_HA_secondary_flag)


# Creating final Secondary CA-UTI indicator
Data1_clean$CA_UTI_Secondary_final <- Data1_clean$UTI_CA_secondary_flag

# Exclude admissions that also meet the HA-UTI definition
# 391 admissions met both Secondary CA-UTI and HA-UTI criteria;
# these were retained in the HA-UTI group.

Data1_clean$CA_UTI_Secondary_final[Data1_clean$UTI_HA_secondary_flag == TRUE & Data1_clean$CA_UTI_Secondary_final == TRUE] <- FALSE

table(Data1_clean$CA_UTI_Secondary_final)


# Remove admissions already classified as Primary CA-UTI
# 2,585 admissions met both Primary and Secondary CA-UTI criteria;
# these were retained in the Primary CA-UTI group.

# Create final Secondary CA-UTI indicator
Data1_clean$CA_UTI_Secondary_final2 <- Data1_clean$CA_UTI_Secondary_final


# Exclude admissions already classified as Primary CA-UTI
Data1_clean$CA_UTI_Secondary_final2[Data1_clean$CA_UTI_Secondary_final2 == TRUE & Data1_clean$CA_UTI_Primary_final == "Yes"] <- FALSE

table(Data1_clean$CA_UTI_Secondary_final2)



# Classify community-acquired UTI as primary or secondary
Data1_clean$UTI_type_CA <- with(Data1_clean, ifelse(
  CA_UTI_Primary_final == "Yes" & CA_UTI_Secondary_final2 == TRUE, "Both",
  ifelse(CA_UTI_Primary_final == "Yes", "Primary",
         ifelse(CA_UTI_Secondary_final2 == TRUE, "Secondary", NA))
))

## checking
table(Data1_clean$UTI_type_CA, useNA = "ifany")

# Check final three UTI groups

cat("Primary CA-UTI:",
    sum(Data1_clean$CA_UTI_Primary_final == "Yes", na.rm = TRUE), "\n")

cat("Secondary CA-UTI:",
    sum(Data1_clean$CA_UTI_Secondary_final2 == TRUE, na.rm = TRUE), "\n")

cat("HA-UTI:",
    sum(Data1_clean$UTI_HA_secondary_flag == TRUE, na.rm = TRUE), "\n")


# Create final three-group UTI variable
Data1_clean$UTI_group <- case_when(
  Data1_clean$UTI_HA_secondary_flag == TRUE ~ "HA-UTI",
  Data1_clean$UTI_type_CA == "Primary" ~ "Primary CA-UTI",
  Data1_clean$UTI_type_CA == "Secondary" ~ "Secondary CA-UTI",
  TRUE ~ NA_character_
)


### checking the new variable
table(Data1_clean$UTI_group, useNA = "ifany")

##### for ordering
Data1_clean$UTI_group <- factor(
  Data1_clean$UTI_group,
  levels = c("Primary CA-UTI", "Secondary CA-UTI", "HA-UTI")
)

####Descriptive analysis of all the 3 groups

# VARIABLE PREPARATION

#### sex
Data1_clean$sex <- factor(
  Data1_clean$sex,
  levels = c(1, 2),
  labels = c("Male", "Female")
)
Data1_clean$sex

#### cross table
table(Data1_clean$UTI_type_CA, Data1_clean$sex)


# Create labelled age groups from the original age_group variable
Data1_clean$age_group_label <- factor(
  Data1_clean$age_group,
  levels = c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18),
  labels = c("20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
             "50-54", "55-59", "60-64", "65-69", "70-74", "75-79",
             "80-84", ">85"),
  ordered = TRUE
)

table(Data1_clean$age_group_label, useNA = "ifany")

## to find % in age group
prop.table(table(Data1_clean$age_group)) * 100

### CREATE AGE GROUPS FOR BASELINE CHARACTERISTICS

Data1_clean <- Data1_clean %>%
  mutate(
    age_group_simple = case_when(
      age_group_label %in% c(
        "20-24","25-29","30-34","35-39",
        "40-44","45-49","50-54","55-59",
        "60-64"
      ) ~ "<65 years",
      
      age_group_label %in% c(
        "65-69","70-74"
      ) ~ "65–74 years",
      
      age_group_label %in% c(
        "75-79","80-84"
      ) ~ "75–84 years",
      
      age_group_label %in% c(
        ">85"
      ) ~ "≥85 years",
      
      TRUE ~ NA_character_
    )                  # closes case_when()
  )                  # closes mutate()

Data1_clean$age_group_simple <- factor(
  Data1_clean$age_group_simple,
  levels = c(
    "<65 years",
    "65–74 years",
    "75–84 years",
    "≥85 years"
  )
)

## Creating a critical care flag
Data1_clean$CriticalCare_flag <- ifelse(Data1_clean$itulos > 0, "Yes", "No")

Data1_clean$CriticalCare_flag 

# Create ventilator-use indicator
table(Data1_clean$Vent_flag, useNA = "ifany")

Data1_clean$Vent_flag <- ifelse(is.na(Data1_clean$cvs) | Data1_clean$cvs == 0, 
                                "No", "Yes")


### Table 1 DESCRIPTIVE ANALYSIS

#### SEX

### Male and female for Primary CA-UTI
sex_primary_CA <- Primary_CA_UTI %>%
  group_by(sex) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(percent = round(100 * n / sum(n), 2))

sex_primary_CA

####Male and female for Secondary CA-UTI
sex_secondary_CA <- Secondary_CA_UTI %>%
  group_by(sex) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(percent = round(100 * n / sum(n), 2))

sex_secondary_CA

#### Male and female for HA-UTI TRUE
sex_HA_TRUE <- HA_UTI_TRUE %>%
  group_by(sex) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(percent = round(100 * n / sum(n), 2))

sex_HA_TRUE


# Age distribution: Primary CA-UTI

age_primary_CA <- Primary_CA_UTI %>%
  group_by(age_group_simple) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(percent = round(100 * n / sum(n), 1))

age_primary_CA


# Age distribution: Secondary CA-UTI

age_secondary_CA <- Secondary_CA_UTI %>%
  group_by(age_group_simple) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(percent = round(100 * n / sum(n), 1))

age_secondary_CA


# Age distribution: HA-UTI

age_HA <- HA_UTI_TRUE %>%
  group_by(age_group_simple) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(percent = round(100 * n / sum(n), 1))

age_HA


#### To find out from where the patient came from before admission

## code for source of admission Primary CA-UTI

source_primary <- Primary_CA_UTI %>%
  group_by(source_agg) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(percent = round(100 * n / sum(n), 2))

source_primary

## code for source of admission for Secondary CA-UTI

source_secondary <- Secondary_CA_UTI %>%
  group_by(source_agg) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(percent = round(100 * n / sum(n), 2))

source_secondary

##### code for source of admission  Secondary HA-UTI (TRUE)

source_HA <- HA_UTI_TRUE %>%
  group_by(source_agg) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(percent = round(100 * n / sum(n), 2))

source_HA


### Type of admission for Primary CA-UTI

elem_primary <- Primary_CA_UTI %>%
  group_by(elem) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(percent = round(100 * n / sum(n), 2))

elem_primary

#### Type of admission for Secondary CA-UTI

elem_secondary <- Secondary_CA_UTI %>%
  group_by(elem) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(percent = round(100 * n / sum(n), 2))

elem_secondary

#### Type of admission for HA-UTI

elem_HA <- HA_UTI_TRUE %>%
  group_by(elem) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(percent = round(100 * n / sum(n), 2))

elem_HA


# TABLE 1: OVERALL P-VALUES for table 1
## Age group
chisq.test(
  table(Data1_clean$age_group_simple,
        Data1_clean$UTI_group)
)

## Sex
chisq.test(
  table(Data1_clean$sex,
        Data1_clean$UTI_group)
)

## Type of admission
chisq.test(
  table(Data1_clean$elem,
        Data1_clean$UTI_group)
)

## Source of admission
chisq.test(
  table(Data1_clean$source_agg,
        Data1_clean$UTI_group)
)



####################     OUTCOME SECTION         
# TABLE 2: CLINICAL OUTCOMES

# Length of stay

##### to create combined LOS table

los_group_compare <- Data1_clean %>%
  filter(!is.na(UTI_group)) %>%
  group_by(UTI_group) %>%
  summarise(
    n = n(),
    mean_los = round(mean(los, na.rm = TRUE), 1),
    median_los = round(median(los, na.rm = TRUE), 1),
    sd_los = round(sd(los, na.rm = TRUE), 1),
    min_los = round(min(los, na.rm = TRUE), 1),
    max_los = round(max(los, na.rm = TRUE), 1),
    .groups = "drop"
  )

los_group_compare

#### critical care group comparison

criticalcare_group_compare <- Data1_clean %>%
  filter(!is.na(UTI_group)) %>%
  group_by(UTI_group, CriticalCare_flag) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(UTI_group) %>%
  mutate(percent = round(100 * n / sum(n), 1))

criticalcare_group_compare


#### Creating the combined itulos summary table

itulos_group_compare <- Data1_clean %>%
  filter(!is.na(UTI_group), itulos > 0) %>%
  group_by(UTI_group) %>%
  summarise(
    n = n(),
    mean_itulos = round(mean(itulos, na.rm = TRUE), 1),
    median_itulos = round(median(itulos, na.rm = TRUE), 1),
    sd_itulos = round(sd(itulos, na.rm = TRUE), 1),
    min_itulos = round(min(itulos, na.rm = TRUE), 1),
    max_itulos = round(max(itulos, na.rm = TRUE), 1),
    .groups = "drop"
  )

itulos_group_compare

# Create ventilator-use indicator
table(Data1_clean$Vent_flag, useNA = "ifany")
Data1_clean$Vent_flag <- ifelse(is.na(Data1_clean$cvs) | Data1_clean$cvs == 0, 
                                "No", "Yes")

#### Combined ventilator usage table
#### % of patients in each group who required ventilator
vent_group_compare <- Data1_clean %>%
  filter(!is.na(UTI_group)) %>%
  group_by(UTI_group, Vent_flag) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(UTI_group) %>%
  mutate(percent = round(100 * n / sum(n), 1))

vent_group_compare

#### Ventilator duration table
cvs_group_compare <- Data1_clean %>%
  filter(!is.na(UTI_group), cvs > 0) %>%
  group_by(UTI_group) %>%
  summarise(
    n = n(),
    mean_cvs = round(mean(cvs, na.rm = TRUE), 1),
    median_cvs = round(median(cvs, na.rm = TRUE), 1),
    sd_cvs = round(sd(cvs, na.rm = TRUE), 1),
    min_cvs = round(min(cvs, na.rm = TRUE), 1),
    max_cvs = round(max(cvs, na.rm = TRUE), 1),
    .groups = "drop"
  )

cvs_group_compare

     
# MEDIAN AND IQR FOR TABLE 2

# Length of stay
los_IQR_compare <- Data1_clean %>%
  filter(!is.na(UTI_group)) %>%
  group_by(UTI_group) %>%
  summarise(
    median_los = round(median(los, na.rm = TRUE), 1),
    Q1_los = round(quantile(los, 0.25, na.rm = TRUE), 1),
    Q3_los = round(quantile(los, 0.75, na.rm = TRUE), 1),
    .groups = "drop"
  )

los_IQR_compare


# Critical care length of stay
# Among patients with a critical care stay
itulos_IQR_compare <- Data1_clean %>%
  filter(!is.na(UTI_group), itulos > 0) %>%
  group_by(UTI_group) %>%
  summarise(
    median_itulos = round(median(itulos, na.rm = TRUE), 1),
    Q1_itulos = round(quantile(itulos, 0.25, na.rm = TRUE), 1),
    Q3_itulos = round(quantile(itulos, 0.75, na.rm = TRUE), 1),
    .groups = "drop"
  )

itulos_IQR_compare


# Duration of ventilatory support
# Among patients who received ventilatory support
cvs_IQR_compare <- Data1_clean %>%
  filter(!is.na(UTI_group), cvs > 0) %>%
  group_by(UTI_group) %>%
  summarise(
    median_cvs = round(median(cvs, na.rm = TRUE), 1),
    Q1_cvs = round(quantile(cvs, 0.25, na.rm = TRUE), 1),
    Q3_cvs = round(quantile(cvs, 0.75, na.rm = TRUE), 1),
    .groups = "drop"
  )

cvs_IQR_compare



# SEPSIS BY UTI GROUP

### TABLE 2: OVERALL P-VALUES FOR CLINICAL OUTCOMES

### Length of stay (3 groups → Kruskal–Wallis)
kruskal.test(
  los ~ UTI_group,
  data = Data1_clean
)
#### In-hospital mortality (Pearson's chi-square)
chisq.test(
  table(
    Data1_clean$Death,
    Data1_clean$UTI_group
  )
)
#### Critical care utilisation (Pearson's chi-square)
# Among patients with a critical care stay

chisq.test(
  table(
    Data1_clean$CriticalCare_flag,
    Data1_clean$UTI_group
  )
)
### ICU stay (3 groups → Kruskal–Wallis)
kruskal.test(
  itulos ~ UTI_group,
  data = Data1_clean
)
#### Ventilator use (Pearson's chi-square)
chisq.test(
  table(
    Data1_clean$Vent_flag,
    Data1_clean$UTI_group
  )
)
#### Duration of ventilatory support (3 groups → Kruskal–Wallis)
# Among patients who received ventilatory support

kruskal.test(
  cvs ~ UTI_group,
  data = Data1_clean
)


##### DIAGNOSIS ANALYSIS - REVIEW LATER



# Diagnosis preparation


# Reduce diagnosis codes dx01–dx30 to 3-character ICD-10-AM codes

Data1_clean <- Data1_clean %>%
  mutate(across(dx01:dx30, ~ substr(., 1, 3), .names = "{.col}_3"))

# Create 3-character UTI code list

UTIcodes_3 <- unique(substr(UTIcodes, 1, 3))

# OTHER DIAGNOSES IN PRIMARY CA-UTI
# In Primary CA-UTI, UTI is the principal diagnosis (dx01).
# Therefore, examine other diagnoses recorded in dx02–dx30.


# OTHER DIAGNOSES IN PRIMARY CA-UTI
# UTI is the principal diagnosis (dx01),
# so examine other diagnoses in dx02–dx30.

###### Select Primary CA-UTI patients

Primary_CA_UTI <- Data1_clean %>%
  filter(UTI_type_CA == "Primary")

### Convert secondary diagnoses dx02–dx30 into long format

Primary_CA_UTI_all <- Primary_CA_UTI %>%
  select(ID, matches("^dx(0[2-9]|[12][0-9]|30)_3$")) %>%
  pivot_longer(
    cols = -ID,
    names_to = "dx_position",
    values_to = "dx_code"
  ) %>%
  filter(!is.na(dx_code))

#### To check we have dx_code
head(Primary_CA_UTI_all)


# Remove UTI diagnoses because we want other diagnoses only
Primary_CA_UTI_all <- Primary_CA_UTI_all %>%
  filter(!dx_code %in% UTIcodes_3)


# Remove duplicate diagnoses within the same patient
# so each patient is counted only once per diagnosis

Primary_CA_UTI_all_unique <- Primary_CA_UTI_all %>%
  distinct(ID, dx_code)

### Count the top 10 other diagnoses at patient level

top10_primary_other_dx_patient <- Primary_CA_UTI_all_unique %>%
  group_by(dx_code) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)

# Denominator = full Primary CA-UTI cohort
total_primary <- nrow(Primary_CA_UTI)

# Calculate percentages
top10_primary_other_dx_patient_pct <- top10_primary_other_dx_patient %>%
  mutate(
    percent = round(100 * n / total_primary, 1)
  )

top10_primary_other_dx_patient_pct



# OTHER DIAGNOSES IN SECONDARY CA-UTI

# UTI is recorded in secondary diagnosis fields (dx02–dx30),
# so examine the principal diagnosis (dx01).


####  Select Secondary CA-UTI patients
Secondary_CA_UTI <- Data1_clean %>%
  filter(UTI_type_CA == "Secondary")

### Remove patients with missing principal diagnosis AND UTI diagnoses from the principal diagnosis
# because we want other diagnoses only

Secondary_CA_UTI_clean <- Secondary_CA_UTI %>%
  filter(
    !is.na(dx01_3),
    !dx01_3 %in% UTIcodes_3
  )


## Count the top 10 principal diagnoses in Secondary CA-UTI
top10_secondary_other_dx <- Secondary_CA_UTI_clean %>%
  group_by(dx01_3) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)

top10_secondary_other_dx

### Denominator = full Secondary CA-UTI cohort
total_secondary <- nrow(Secondary_CA_UTI)

#### Calculate percentage using the total Secondary CA-UTI cohort

top10_secondary_other_dx_pct <- top10_secondary_other_dx %>%
  mutate(percent = round(100 * n / total_secondary, 1))

top10_secondary_other_dx_pct

#### OTHER DIAGNOSES IN HA-UTI
### For HA-UTI, examine the principal diagnosis (dx01)

#### Select HA-UTI patients

HA_UTI_TRUE <- Data1_clean %>%
  filter(UTI_HA_secondary_flag == TRUE)


# For HA-UTI, UTI is identified in secondary diagnosis fields (dx02–dx30).
# Therefore, examine the principal diagnosis (dx01) to identify the main reason for admission.

# Remove missing principal diagnoses AND UTI DIAGNOSIS because we want other diagnoses only

HA_UTI_TRUE_clean <- HA_UTI_TRUE %>%
  filter(!is.na(dx01_3)) %>%
  filter(!dx01_3 %in% UTIcodes_3)

###  Count the top 10 principal diagnoses in HA-UTI

top10_HA_TRUE_dx <- HA_UTI_TRUE_clean %>%
  group_by(dx01_3) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)

top10_HA_TRUE_dx

# Denominator = full HA-UTI cohort
total_HA_TRUE <- nrow(HA_UTI_TRUE)

top10_HA_TRUE_dx_pct <- top10_HA_TRUE_dx %>%
  mutate(percent = round(100 * n / total_HA_TRUE, 1))

total_HA_TRUE
top10_HA_TRUE_dx_pct



#### when we were doing 3 group comparison for regression model 

### creating mortality
Data1_clean$Death <- ifelse(
  Data1_clean$discode_agg == "04 Died",
  "Yes",
  "No"
)

Data1_clean$Death<- factor(
  Data1_clean$Death,
  levels = c("No","Yes")
)

table(Data1_clean$Death)

## Create sepsis variable
Data1_clean$sepsis <- apply(
  Data1_clean[, paste0("dx", sprintf("%02d", 1:30), "_3")],
  1,
  function(x) any(x == "A41", na.rm = TRUE)
)

Data1_clean$sepsis <- factor(
  ifelse(Data1_clean$sepsis, "Yes", "No"),
  levels = c("No", "Yes")
)
table(Data1_clean$sepsis)

### Create LOS groups
Data1_clean <- Data1_clean%>%
  mutate(
    LOS_group = case_when(
      los <= 3 ~ "0–3 days",
      los >= 4 & los <= 7 ~ "4–7 days",
      los >= 8 & los <= 14 ~ "8–14 days",
      los > 14 ~ ">14 days"
    )
  )

Data1_clean$LOS_group <- factor(
  Data1_clean$LOS_group,
  levels = c(
    "0–3 days",
    "4–7 days",
    "8–14 days",
    ">14 days"
  )
)
table(Data1_clean$LOS_group)


### Create numeric age
Data1_clean$age_m <- as.numeric(
  Data1_clean$age_group_label
)
table(Data1_clean$age_m)

#### recoding sex as it is in ordered factor
Data1_clean$sex <- factor(
  Data1_clean$sex,
  levels = c("Female", "Male"),
  ordered = FALSE
)
## in source of admission setting home as refernce
Data1_clean$source_agg <- relevel(
  factor(Data1_clean$source_agg),
  ref = "01 Home"
)
Data1_clean$UTI_group <- factor(
  Data1_clean$UTI_group,
  levels = c(
    "Primary CA-UTI",
    "Secondary CA-UTI",
    "HA-UTI"
  )
)
### MAIN MULTIVARIABLE LOGISTIC REGRESSION

## Final multivariable logistic regression
M1 <- glm(
  Death ~
    sepsis +
    UTI_group +
    age_m +
    sex +
    source_agg +
    LOS_group,
  data = Data1_clean,
  family = binomial
)

summary(M1)

### Create the adjusted OR table
result <- data.frame(
  Variable = names(coef(M1)),
  OR = exp(coef(M1)),
  exp(confint(M1)),
  p_value = summary(M1)$coefficients[,4]
)

colnames(result) <- c(
  "Variable",
  "OR",
  "CI_lower",
  "CI_upper",
  "p_value"
)

result <- result %>%
  mutate(
    OR = round(OR,2),
    CI_lower = round(CI_lower,2),
    CI_upper = round(CI_upper,2),
    p_value = ifelse(
      p_value < 0.001,
      "<0.001",
      sprintf("%.3f", p_value)
    )
  )
result


#### Check the interaction
M2 <- glm(
  Death ~
    sepsis * UTI_group +
    age_m +
    sex +
    source_agg +
    LOS_group,
  data = Data1_clean,
  family = binomial
)

summary(M2)

### OR for interaction table
result1 <- data.frame(
  Variable = names(coef(M2)),
  OR = exp(coef(M2)),
  exp(confint(M2)),
  p_value = summary(M2)$coefficients[,4]
)

colnames(result1) <- c(
  "Variable",
  "OR",
  "CI_lower",
  "CI_upper",
  "p_value"
)

result1 <- result1 %>%
  mutate(
    OR = round(OR,2),
    CI_lower = round(CI_lower,2),
    CI_upper = round(CI_upper,2),
    p_value = ifelse(
      p_value < 0.001,
      "<0.001",
      sprintf("%.3f", p_value)
    )
  )

result1



#### for getting overall p value for baseline characterstics



### CREATE AGE GROUPS FOR BASELINE CHARACTERISTICS

Data1_clean <- Data1_clean %>%
  mutate(
    age_group_simple = case_when(
      age_group_label %in% c(
        "20-24","25-29","30-34","35-39",
        "40-44","45-49","50-54","55-59",
        "60-64"
      ) ~ "<65 years",
      
      age_group_label %in% c(
        "65-69","70-74"
      ) ~ "65–74 years",
      
      age_group_label %in% c(
        "75-79","80-84"
      ) ~ "75–84 years",
      
      age_group_label %in% c(
        ">85"
      ) ~ "≥85 years",
      
      TRUE ~ NA_character_
    )                  # closes case_when()
  )                  # closes mutate()

Data1_clean$age_group_simple <- factor(
  Data1_clean$age_group_simple,
  levels = c(
    "<65 years",
    "65–74 years",
    "75–84 years",
    "≥85 years"
  )
)





##Create survival outcome from Death
## Death = Yes means Survival = No
## Death = No means Survival = Yes

Data1_clean <- Data1_clean %>%
  mutate(
    Survival = case_when(
      Death == "Yes" ~ "No",
      Death == "No"  ~ "Yes",
      TRUE           ~ NA_character_
    )
  )

## Put survivors first and deaths second
Data1_clean$Survival <- factor(
  Data1_clean$Survival,
  levels = c("Yes", "No")
)

## Check the variables
table(Data1_clean$UTI_group, useNA = "ifany")
table(Data1_clean$LOS_group, useNA = "ifany")
table(Data1_clean$Survival, useNA = "ifany")

## Create the Sankey flow data

## Sankey: UTI type → Length of stay → Survival outcome

sankey_v4_data <- Data1_clean %>%
  filter(
    !is.na(UTI_group),
    !is.na(sepsis),
    !is.na(Death)
  ) %>%
  count(
    UTI_group,
    sepsis,
    Survival,
    name = "n"
  )

## Create final Sankey plot
Sankey_V4_Final <- ggplot(
  sankey_v4_data,
  aes(
    axis1 = UTI_group,
    axis2 = sepsis,
    axis3 = Survival,
    y = n
  )
) +
  geom_alluvium(
    aes(fill = sepsis),
    width = 1/12,
    alpha = 0.80
  ) +
  geom_stratum(
    width = 1/12,
    fill = "grey90",
    colour = "black",
    linewidth = 0.5
  ) +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 6,
    fontface = "bold"
  ) +
  scale_fill_manual(
    name = "Sepsis status",
    values = c(
      "No" = "#7F9E99",
      "Yes" = "#D4965E"
    ),
    labels = c(
      "No" = "Sepsis No",
      "Yes" = "Sepsis Yes"
    )
  ) +
  scale_x_discrete(
    limits = c(
      "UTI type",
      "Sepsis status",
      "Survival outcome"
    ),
    expand = c(0.08, 0.08)
  ) +
  labs(
    title = "Distribution of UTI type, sepsis status and survival outcome",
    subtitle = "Primary CA-UTI, Secondary CA-UTI and HA-UTI patients",
    x = NULL,
    y = "Number of patients"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(
      size = 26,
      face = "bold",
      hjust = 0
    ),
    plot.subtitle = element_text(
      size = 18,
      hjust = 0
    ),
    axis.title.y = element_text(
      size = 18,
      face = "bold"
    ),
    axis.text.x = element_text(
      size = 16,
      face = "bold"
    ),
    axis.text.y = element_text(
      size = 14
    ),
    legend.title = element_text(
      size = 18,
      face = "bold"
    ),
    legend.text = element_text(
      size = 16
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(
      t = 15,
      r = 25,
      b = 15,
      l = 15
    )
  )

## Display the plot
print(Sankey_V4_Final)

## Save as a separate version
ggsave(
  filename = "Figure_Sankey_V4_Final_Three_UTI_Groups.png",
  plot = Sankey_V4_Final,
  width = 16,
  height = 10,
  dpi = 600,
  bg = "white"
)
getwd()

shell.exec(getwd())
