## code to prepare `data` dataset goes here
library(tidyverse)

pat <- read_delim("data-raw/Random_PatientLevelInfo_2020.tsv", delim = "\t")
lab <- read_delim("data-raw/Random_LabValuesInfo_2020.tsv", delim = "\t")

# Patient level data
pat <- pat %>% 
  mutate(
    SEX = fct_collapse(SEX, U = c("UNDIFFERENTIATED", "U")
    )
  )

# Lab measurements
lab <- lab %>% 
  mutate(
    BMRKR2 = fct_relevel(BMRKR2, c("LOW", "MEDIUM", "HIGH")),
    AVISIT = fct_relevel(AVISIT, "SCREENING", after = 0)
  ) %>% 
  group_by(
    USUBJID, LBTEST
  ) %>% 
  arrange(
    AVISIT # is already the default ordering. Just to be sure.
  ) %>% 
  mutate(
    AVAL_CHANGE = AVAL - nth(AVAL, n = 2) # change relative to baseline (2nd measurement)
  ) %>% 
  ungroup()


usethis::use_data(pat, lab)
