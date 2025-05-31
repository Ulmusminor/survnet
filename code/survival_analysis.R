###################################################################
## Author: Marco del Olmo Linares
## Date of last edit: 21/05/2025
##
## Description: Code to handle the survival analysis file from
## data_cleaning.R and make a Cox regression model to fit it.
###################################################################

library(tidyverse)
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

# We adjust with coxph

m1cox <- ttrain |> coxph(formula = Surv(time, status) ~ 
                       `Radio Therapy` + `Tumor Stage` + `Age60`)
summary(m1cox)

# And now we test with the ttest dataset:

pred <- survfit(m1cox, newdata = ttest)
pred_60 <- summary(pred, times = 60)$surv

# Since survfit calculates probability of survival to 60 months now we
# need the probability of the event, which is 1 - this. Also we want the
# numbers from pred_60, not the indices.

pred_roc <- 1 - pred_60 |> as.numeric()
r1cox <- roc(response = ttest |> pull(status), predictor = pred_roc)

r1cox |> ggroc() +
  theme_bw()

# And there we have it. A ROC curve with the test. Next we'll use a network.
