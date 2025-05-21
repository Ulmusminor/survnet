###################################################################
## Author: Marco del Olmo Linares
## Date of last edit: 21/05/2025
##
## Description: Code to take the .tsv file from
## https://www.cbioportal.org/study/clinicalData?id=brca_metabric
## And reformat it when necessary.
###################################################################

library(tidyverse)

t <- read_tsv("./data/brca_metabric_clinical_data.tsv")

## Identify and exclude all columns which are equal accross the 
## entire table (sex, cancer type, study id...)

tlog <- t |> select(where(~ n_distinct(.) > 1))

# The number of variables in this base is still too big, we need to do research.

tbib <- tlog |> select(`Patient ID`, `Age at Diagnosis`, `Tumor Stage`, `Radio Therapy`,
                  `Overall Survival (Months)`, `Overall Survival Status`) |> 
  drop_na(-'Overall Survival Status')

# A NA in Overall Survival Status can be treated as censored. But we do need to
# have some kind of time of survival data.

## Tang et al. (it's a retracted paper, but we are not evaluating the clinical
## implications of this work) identifies Age (<60 years/>60 years), Stage and
## HER2 status as the three variables which could have predictive value over
## 3-year or 5-year survival. ** Put this in the Quarto **

# Now we create the important variables for us.

tref <- tbib |> mutate(
  Age60 = `Age at Diagnosis` > 60
)
