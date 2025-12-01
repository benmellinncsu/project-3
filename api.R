library(plumber)
library(tidymodels)
library(readr)
library(dplyr)
library(ranger)


#Read in the data

data <- read_csv("../diabetes_binary_health_indicators_BRFSS2015.csv")

# subset only the relevant predictors
data_subset <- data |>
  select(Diabetes_binary, Smoker, PhysActivity, Fruits, Veggies,
         HvyAlcoholConsump, Sex, Education, Income)

#convert all features to factors
data_subset <- data_subset |>
  mutate(
    Diabetes_binary   = factor(Diabetes_binary, levels = c(0,1), labels = c("No", "Yes")),
    Smoker            = factor(Smoker, levels = c(0,1), labels = c("No", "Yes")),
    PhysActivity      = factor(PhysActivity, levels = c(0,1), labels = c("No", "Yes")),
    Fruits            = factor(Fruits, levels = c(0,1), labels = c("No", "Yes")),
    Veggies           = factor(Veggies, levels = c(0,1), labels = c("No", "Yes")),
    HvyAlcoholConsump = factor(HvyAlcoholConsump, levels = c(0,1), labels = c("No", "Yes")),
    Sex               = factor(Sex, levels = c(0,1), labels = c("Male", "Female")),
    Education         = as.factor(Education),
    Income            = as.factor(Income)
  )

## Fit the random forest model
rf_spec <- rand_forest(
  mtry = 2,
  trees = 50,
  min_n = 5
) |>
  set_engine("ranger") |>
  set_mode("classification")

# Create workflow
rf_wf <- workflow() |>
  add_model(rf_spec) |>
  add_formula(Diabetes_binary ~ .)

# Fit on the full dataset
best_model <- rf_wf |> fit(data = data_subset)

