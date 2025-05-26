###################################################################
## Author: Marco del Olmo Linares
## Date of last edit: 26/05/2025
##
## Description: Code to handle the survival analysis file from
## data_cleaning.R and make a neural network regression model.
###################################################################

library(keras)
library(tensorflow)
library(tidyverse)
library(survival)
library(pROC)

tprop <- tref |> mutate(
  time = ifelse(`Overall Survival (Months)` > 60, 60, `Overall Survival (Months)`),
  status = ifelse(`Overall Survival Status` == "1:DECEASED" & `Overall Survival (Months)` < 60,
                  1, 0),
  truestatus = ifelse(`Overall Survival Status` == "1:DECEASED", 1, 0)
)

set.seed(2479)

# I will split the data in a training and a test subset to compare the methods
# in the test subset.

ttrain <- tprop |> slice_sample(prop = 0.8)
ttest  <- anti_join(tprop, ttrain, by = "Patient ID")

# Here keras expects numeric matrices.

# Training matrices - x variables and y variables.

x_train <- ttrain |> mutate(
  `Radio Therapy` = fct(`Radio Therapy`),
  Age60 = as.numeric(Age60)
) |> 
  select("Radio Therapy", "Tumor Stage", "Age60") |> 
  dummy_cols(remove_first_dummy = FALSE,
             remove_selected_columns = TRUE) |> 
  as.matrix() |> 
  scale()

y_train <- ttrain |> 
  select("time", "status") |>   
  as.matrix()

# Testing matrices.

x_test <- ttest |> mutate(
  `Radio Therapy` = fct(`Radio Therapy`),
  Age60 = as.numeric(Age60)
) |> 
  select("Radio Therapy", "Tumor Stage", "Age60") |> 
  dummy_cols(remove_first_dummy = FALSE,
             remove_selected_columns = TRUE) |> 
  as.matrix() |> 
  scale()

y_test <- ttest |> 
  select("time", "status") |>   
  as.matrix()

# For a survival neural network in tensorflow we need to define the survival loss function.

surv_loss <- function(y_true, y_pred){
  times <- y_true[, 1]
  event <- y_true[, 2]
}
