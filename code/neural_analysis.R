###################################################################
## Author: Marco del Olmo Linares
## Date of last edit: 26/05/2025
##
## Description: Code to handle the survival analysis file from
## data_cleaning.R and make a neural network regression model.
###################################################################

library(dnn)
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

# We need to design the model for the neural network:

model = dNNmodel(units = c(4, 3, 1),
                 activation = c("relu", "relu", "relu"),
                 input_shape = 3)

# Now we can run the function deepSurv:

set.seed(6700)
m1nn <- deepSurv(formula = Surv(time, status) ~ 
                          `Radio Therapy` + `Tumor Stage` + `Age60`,
                 model = model,
                 data = ttrain)
summary(m1nn)

###### another model with more neurons

model2 = dNNmodel(units = c(16, 16, 1),
                 activation = c("relu", "relu", "linear"),
                 input_shape = 3)

# Now we can run the function deepSurv:

set.seed(6700)
m2nn <- deepSurv(formula = Surv(time, status) ~ 
                   `Radio Therapy` + `Tumor Stage` + `Age60`,
                 model = model2,
                 lr_rate = 5e-5,
                 data = ttrain)
summary(m2nn)

######

# With this we can check if we did a good adjustment.

prepare_test_data <- function(t) {
  t$Radio_Therapy_num <- ifelse(t$`Radio Therapy` == "YES", 1, 0)
  t$Tumor_Stage_num <- as.numeric(t$`Tumor Stage`)
  t$Age60_num <- as.numeric(t$Age60)
  t[, c("Radio_Therapy_num", "Tumor_Stage_num", "Age60_num")] |>
    as.matrix()
}

ttest_mat <- prepare_test_data(ttest)
risk_scores <- predict(m2nn, newdata = ttest_mat)

roc_obj <- roc(response = ttest$status, predictor = risk_scores$risk |> as.numeric())

## This shows the DeepSurv model ROC. 
## We will try to put each one side to side now.

df_cox <- data.frame(
  specificity = rev(r1cox$specificities),
  sensitivity = rev(r1cox$sensitivities),
  model = "Cox Model"
)

df_dnn <- data.frame(
  specificity = rev(roc_obj$specificities),
  sensitivity = rev(roc_obj$sensitivities),
  model = "DeepSurv"
)

# Combine both data frames
df_all <- rbind(df_cox, df_dnn)

# Plot with different colors for each model
ggplot(df_all, aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_line(size = 1) +
  labs(x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)",
       title = "ROC Curve Comparison") +
  theme_bw() +
  theme(legend.position = "bottom")

