if(!is.null(dev.list())) dev.off()
rm(list = ls())
cat("\014")

library(haven)
library(tidyverse)
library(labelled)

#SECTION 01 

section01 <- read_dta("NLSS/NLSS IV 2022_23/data/stata format/S01.dta")

section01 <- section01 %>%
  mutate(
    uniq_id = paste0(psu_number, "-", hh_number, "-", idcode)
  )

respondent_father <- section01 %>%
  filter(!is.na(q01_11)) %>%
  mutate(
    hhid = paste0(psu_number, "-", hh_number),
    father_id = paste0(psu_number, "-", hh_number, "-", q01_11), 
    uniq_id = father_id
  ) %>%
  select(
    hhid, father_id, uniq_id, q01_03
  ) %>%
  rename(
    firstchild_age = q01_03
  ) %>%
  group_by(father_id) %>%
  slice_max(firstchild_age, n = 1, with_ties = FALSE) %>%
  ungroup() 
  
respondent_father <- merge(
    respondent_father, 
    section01[, c("uniq_id", "q01_02", "q01_03", "q01_05")],
    by = "uniq_id"
)

respondent_mother <- section01 %>%
  filter(!is.na(q01_14)) %>%
  mutate(
    hhid = paste0(psu_number, "-", hh_number),
    mother_id = paste0(psu_number, "-", hh_number, "-", q01_14), 
    uniq_id = mother_id
  ) %>%
  select(
    hhid, mother_id, uniq_id, q01_03, q01_15
  ) %>%
  rename(
    firstchild_age = q01_03,
    mother_education = q01_15
  ) %>%
  group_by(mother_id) %>%
  slice_max(firstchild_age, n = 1, with_ties = FALSE) %>%
  ungroup()

respondent_mother <- merge(
    respondent_mother, 
    section01[, c("uniq_id", "q01_02", "q01_03", "q01_05")],
    by = "uniq_id"
)

#SECTION 7 - EDUCATION

section07 <- read_dta("NLSS/NLSS IV 2022_23/data/stata format/S07.dta")

section07 <- section07 %>%
  mutate(
    uniq_id = paste0(psu_number, "-", hh_number, "-", idcode)
  )

respondent_father <- merge(
  respondent_father, 
  section07[, c("uniq_id", "q07_12")],
  by = "uniq_id"
)

respondent_mother <- merge(
  respondent_mother, 
  section07[, c("uniq_id", "q07_12")],
  by = "uniq_id"
)

#CLEANING VARIABLES  

respondent_father <- respondent_father %>%
  rename(
    father_sex = q01_02,
    father_age = q01_03,
    father_maritalstatus = q01_05
  ) %>%
  mutate(
    father_education = case_when(
        q07_12 %in% c(0, 16, 17, 98, NA) ~ 1,   #PRE-PRIMARY
        q07_12 %in% c(1:8) ~ 2,                 #PRIMARY
        q07_12 %in% c(9:12, 15) ~ 3,            #SECONDARY
        q07_12 %in% c(13, 14) ~ 4,              #POST SECONDARY 
    )
  ) %>%
  select(-uniq_id) %>%
  select(hhid, father_id, everything())

respondent_mother <- respondent_mother %>%
  rename(
    mother_sex = q01_02,
    mother_age = q01_03,
    mother_maritalstatus = q01_05
  ) %>%
  mutate(
    mother_education = case_when(
        q07_12 %in% c(0, 16, 17, 98, NA) ~ 1,   #PRE-PRIMARY
        q07_12 %in% c(1:8) ~ 2,                 #PRIMARY
        q07_12 %in% c(9:12, 15) ~ 3,            #SECONDARY
        q07_12 %in% c(13, 14) ~ 4,              #POST SECONDARY 
    )
  ) %>%
  select(-uniq_id) %>%
  select(hhid, mother_id, everything())


  
    