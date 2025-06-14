###################################################################
## Author: Marco del Olmo Linares
## Date of last edit: 21/05/2025
##
## Description: Code to handle the survival analysis file from
## data_cleaning.R and make a Cox regression model to fit it.
###################################################################

library(tidyverse)
library(pROC)

# We adjust with coxph

m1cox <- ttrain_3var |> coxph(formula = Surv(time, status) ~ 
                       `Radio Therapy` + `Tumor Stage` + `Age60`)
summary(m1cox)

pred <- survfit(m1cox, newdata = ttest)
pred_60 <- summary(pred, times = 60)$surv

# get survival scores (we were getting event scores)

pred_roc <- 1 - pred_60 |> as.numeric()
r1cox <- roc(response = ttest |> pull(status), predictor = pred_roc)

# compare if the survival scores match the observed status.

## Repeat for all variables:

# We adjust with coxph

m2cox <- ttrain_nvar |> coxph(formula = Surv(time, status) ~ .)
summary(m2cox)

pred <- survfit(m1cox, newdata = ttest)
pred_60 <- summary(pred, times = 60)$surv

# get survival scores (we were getting event scores)

pred_roc <- 1 - pred_60 |> as.numeric()
r1cox <- roc(response = ttest |> pull(status), predictor = pred_roc)

# compare if the survival scores match the observed status.

r1cox |> ggroc() +
  theme_bw()
