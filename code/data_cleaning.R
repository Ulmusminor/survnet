###################################################################
## Author: Marco del Olmo Linares
## Date of last edit: 12/05/2025
##
## Description: Code to take the .tsv file from
## https://www.cbioportal.org/study/clinicalData?id=brca_metabric
## And reformat it when necessary.
###################################################################

library(tidyverse)

t <- read_tsv("./data/brca_metabric_clinical_data.tsv")

## Identify and exclude all columns which are equal accross the 
## entire table (sex, cancer type, study id...)

tc <- t |> select(where(~ n_distinct(.) > 1))

# The number of variables in this base is still too big, so for proper
# analysis we still need to do some research.

## Some of the most important variables according to studies is the type
## of receptor present in the cancer cell (ER Status, HER2 Status, 
## PR Status)
