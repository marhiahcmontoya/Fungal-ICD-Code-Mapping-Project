# Project: Fungal ICD Code Matching from ICD-9, ICD-10, and ICD-11
# Owner: Marhiah C. Montoya, PhD
# Coding Start Date: January 16, 2026
# Coding End Date: June 30, 2026
# Goal: Combine ICD-9 with ICD-9-CM, Combine, Remove Duplicates, Repeat for ICD-10 and ICD-10-CM
#######################################################################################################################

#Install needed packages if not already on your system and set library
install.packages("tidyverse")
install.packages("tidytext")
install.packages("igraph")
install.packages("dendextend")
install.packages("corrplot")
install.packages ("patchwork")

#Call libraries 
library(tidyverse)
library (tidytext)
library(igraph)
library(dplyr)
library(reshape2)
library(dendextend)
library(corrplot)
library(networkD3)
library(patchwork)
library(stringdist)
library(openxlsx)
library(htmltools)
library(htmlwidgets)
library(networkD3)
#######################################################################################################################
#Read in the original excel files
library(readxl)
icd9 <- read_excel("C:/Users/mmvmk/Desktop/icd_r/icd_data/section111validicd9-jan2025_0.xlsx", 
                   col_types = c("text", "text", "numeric"))
View(icd9)
colnames(icd9)

library(readxl)
icd9cm <- read_excel("C:/Users/mmvmk/Desktop/icd_r/icd_data/CMS32_DESC_LONG_SHORT_DX.xlsx")
View(icd9cm)
colnames(icd9cm)
#######################################################################################################################
#TABLE PREPARATION AND COMBINING ICD-9 with ICD-9-CM
library(tidyverse)

#Prepare ICD-9, Rename the specific ICD code (code) and description columns and select only those two (dropping the rest)
icd9_clean <- icd9 %>%
  rename(
    code = `CODE`,
    description = `LONG DESCRIPTION (VALID ICD-9 FY2025)`
  ) %>%
  select(code, description)

#Prepare ICD-9-CM, Rename the specific ICD code (code) and Description columns and select only those two (dropping the rest)
icd9cm_clean <- icd9cm %>%
  rename(
    code = `DIAGNOSIS CODE`,
    description = `LONG DESCRIPTION`
  ) %>%
  select(code, description)

#Combine ICD-9 and ICD-9-CM 
combined_icd9 <- bind_rows(icd9_clean, icd9cm_clean)

#Standardize text (lowercase/whitespace) and remove duplicates
combined_icd9nodup <- combined_icd9 %>%
  mutate(
    code = str_squish(code), 
    description = description %>%
      str_to_lower() %>%                 #Change Text to Lowercase
      str_replace_all("[[:punct:]]", "") %>% #Remove all punctuation
      str_squish()                       #Normalize whitespace
  ) %>% 
  distinct()

#Export combined ICD-9 and ICD-9-CM to CSV
write_csv(combined_icd9nodup, "C:/Users/mmvmk/Desktop/icd_r/icd_data/combined_icd9_cleaned.csv")

#View the result
View(combined_icd9nodup)
#######################################################################################################################
#TABLE PREPARATION AND COMBINING ICD-10 with ICD-10-CM

library(readxl)
icd10 <- read_excel("C:/Users/mmvmk/Desktop/icd_r/icd_data/section111validicd10-jan2025_0.xlsx")
View(icd10)
colnames(icd10)
library(readxl)
icd10cm <- read_excel("C:/Users/mmvmk/Desktop/icd_r/icd_data/20260116_icd10cm.xlsx")
View(icd10cm)
colnames(icd10cm)

library(tidyverse)
library(readxl)

#Prepare ICD-10, Rename the specific ICD code (code) and description columns and select only those two (dropping the rest)
icd10_clean <- icd10 %>%
  rename(
    code = `CODE`,
    description = `LONG DESCRIPTION (VALID ICD-10 FY2025)`
  ) %>%
  select(code, description)

#Prepare ICD-10-CM, Columns are already named 'code' and 'description', we just select them to drop 'txt_file_data'
icd10cm_clean <- icd10cm %>%
  select(code, description)

#Combine ICD-10 and ICD-10-CM
combined_icd10 <- bind_rows(icd10_clean, icd10cm_clean)

#Standardize text (lowercase/whitespace) and remove duplicates, Normalize 'code' and ensure it remains character/text to keep leading zero
combined_icd10nodup <- combined_icd10 %>%
  mutate(
    code = str_squish(as.character(code)), 
    description = description %>%
      str_to_lower() %>%                 #Change Text to Lowercase
      str_replace_all("[[:punct:]]", "") %>% #Remove all punctuation
      str_squish()                       #Normalize whitespace
  ) %>% 
  distinct()

#Export combined ICD-10 and ICD-10-CM to CSV
write_csv(combined_icd10nodup, "C:/Users/mmvmk/Desktop/icd_r/icd_data/combined_icd10_cleaned.csv")

#View the result
View(combined_icd10nodup)
#######################################################################################################################
#Import Included and Excluded ICD Codes from ICD-9 and ICD-10
#Included
library(readxl)
icd9_included <- read_excel("C:/Users/mmvmk/Desktop/icd_r/icd_data/ICD9_ICD10_IncludedCodes/20260120_ICD9_IncludedCodes.xlsx")
View(icd9_included)

library(readxl)
icd10_included <- read_excel("C:/Users/mmvmk/Desktop/icd_r/icd_data/ICD9_ICD10_IncludedCodes/20260120_ICD10_IncludedCodes.xlsx")
View(icd10_included)

#Excluded
library(readxl)
icd9_excluded <- read_excel("C:/Users/mmvmk/Desktop/icd_r/icd_data/ICD9_ICD10_ExcludedCodes/20260120_ICD9_ExcludedCodes.xlsx")
View(icd9_excluded)

library(readxl)
icd10_excluded <- read_excel("C:/Users/mmvmk/Desktop/icd_r/icd_data/ICD9_ICD10_ExcludedCodes/20260120_ICD10_ExcludedCodes.xlsx")
View(icd10_excluded)

#######################################################################################################################
#Clean included and excluded reference tables

#Standardize text (lowercase/whitespace) and remove duplicates
icd9_included_clean <- icd9_included %>%
  mutate(
    code = str_squish(code), 
    description = description %>%
      str_to_lower() %>%                 #Change Text to Lowercase
      str_replace_all("[[:punct:]]", "") %>% #Remove all punctuation
      str_squish()                       #Normalize whitespace
  )

icd10_included_clean <- icd10_included %>%
  mutate(
    code = str_squish(code), 
    description = description %>%
      str_to_lower() %>%                 #Change Text to Lowercase
      str_replace_all("[[:punct:]]", "") %>% #Remove all punctuation
      str_squish()                       #Normalize whitespace
  )

icd9_excluded_clean <- icd9_excluded %>%
  mutate(
    code = str_squish(code), 
    description = description %>%
      str_to_lower() %>%                 #Change Text to Lowercase
      str_replace_all("[[:punct:]]", "") %>% #Remove all punctuation
      str_squish()                       #Normalize whitespace
  )

icd10_excluded_clean <- icd10_excluded %>%
  mutate(
    code = str_squish(code), 
    description = description %>%
      str_to_lower() %>%                 #Change Text to Lowercase
      str_replace_all("[[:punct:]]", "") %>% #Remove all punctuation
      str_squish()                       #Normalize whitespace
  )

#######################################################################################################################
#Remove the included and excluded ICD-9 and ICD-10 codes to identify any codes that are unique to ICD-9-CM and ICD-10-CM

#Create the unique table by filtering out codes found in the two reference tables
icd9cm_unique <- combined_icd9nodup %>%
  # Remove rows where the code exists in the excluded table
  anti_join(icd9_excluded_clean, by = "code") %>%
  # Remove rows where the code exists in the included table
  anti_join(icd9_included_clean, by = "code")

#Check how many rows were removed
print(paste("Original count:", nrow(combined_icd9nodup)))
print(paste("Unique count:", nrow(icd9cm_unique)))

#View the result
View(icd9cm_unique)

#Create the unique table by filtering out codes found in the two reference tables
icd10cm_unique <- combined_icd10nodup %>%
  # Remove rows where the code exists in the excluded table
  anti_join(icd10_excluded_clean, by = "code") %>%
  # Remove rows where the code exists in the included table
  anti_join(icd10_included_clean, by = "code")

#Check how many rows were removed
print(paste("Original count:", nrow(combined_icd10nodup)))
print(paste("Unique count:", nrow(icd10cm_unique)))

#View the result
View(icd10cm_unique)

#######################################################################################################################
#Make and export CSV files for the ICD-9-CM and ICD-10-CM unique codes
write_csv(icd9cm_unique, "C:/Users/mmvmk/Desktop/icd_r/icd_data/icd9cm_unique.csv")

write_csv(icd10cm_unique, "C:/Users/mmvmk/Desktop/icd_r/icd_data/icd10cm_unique.csv")

#######################################################################################################################














