###################################################################
## Author: Marco del Olmo Linares
## Date of last edit: 21/05/2025
##
## Description: Code to take the .tsv file from
## https://www.cbioportal.org/study/clinicalData?id=brca_metabric
## And reformat it when necessary.
###################################################################

library(tidyverse)

t <- read_tsv("./data/brca_metabric_clinical_data.tsv") |> 
  drop_na(-'Overall Survival Status')

## Identify and exclude all columns which are equal accross the 
## entire table (sex, cancer type, study id...)

tlog <- t |> select(where(~ n_distinct(.) > 1)) 

# The number of variables in this base is still too big, we need to do research.

tbib <- tlog |> select(`Patient ID`, `Age at Diagnosis`, `Tumor Stage`, `Radio Therapy`,
                  `Overall Survival (Months)`, `Overall Survival Status`) |> 
  mutate(Age60 = `Age at Diagnosis` > 60)

# A NA in Overall Survival Status can be treated as censored. But we do need to
# have some kind of time of survival data.

## Tang et al. (it's a retracted paper, but we are not evaluating the clinical
## implications of this work) identifies Age (<60 years/>60 years), Stage and
## HER2 status as the three variables which could have predictive value over
## 3-year or 5-year survival. ** Put this in the Quarto **

# We prepare the tibbles we are going to use in the analysis.

tprop1 <- tbib |> mutate(
  time = ifelse(`Overall Survival (Months)` > 60, 60, `Overall Survival (Months)`),
  status = ifelse(`Overall Survival Status` == "1:DECEASED" & `Overall Survival (Months)` < 60,
                  1, 0),
  truestatus = ifelse(`Overall Survival Status` == "1:DECEASED", 1, 0)
)

tprop2 <- tlog |> mutate(
  time = ifelse(`Overall Survival (Months)` > 60, 60, `Overall Survival (Months)`),
  status = ifelse(`Overall Survival Status` == "1:DECEASED" & `Overall Survival (Months)` < 60,
                  1, 0),
  truestatus = ifelse(`Overall Survival Status` == "1:DECEASED", 1, 0)
)

set.seed(2479)

# I will split the data in a training and a test subset to compare the methods
# in the test subset.

ttrain_3var <- tprop1 |> slice_sample(prop = 0.8)
ttest_3var  <- anti_join(tprop1, ttrain_3var, by = "Patient ID")

# For analysis with all variables
ttrain_nvar <- tprop2 |> slice_sample(prop = 0.8)
ttest_nvar  <- anti_join(tprop2, ttrain_nvar, by = "Patient ID")

