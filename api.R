library(plumber)
library(tidymodels)
library(readr)
library(dplyr)
library(ranger)
library(yardstick)


#Read in the data

data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")

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
dt_spec <- decision_tree(
  cost_complexity = 0,  
  tree_depth = NULL          
) |>
  set_engine("rpart") %>%
  set_mode("classification")

# Create workflow
dt_wf <- workflow() |>
  add_model(dt_spec) |>
  add_formula(Diabetes_binary ~ .)

# Fit on the full dataset
best_model <- dt_wf |> fit(data = data_subset)



# Helper function to get mode
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#* @apiTitle Diabetes Prediction API

#* Predict diabetes from input features
#* @param Smoker Default is most common class
#* @param PhysActivity Default is most common class
#* @param Fruits Default is most common class
#* @param Veggies Default is most common class
#* @param HvyAlcoholConsump Default is most common class
#* @param Sex Default is most common class
#* @param Education Default is most common class
#* @param Income Default is most common class
#* @get /predict
function(Smoker = get_mode(data_subset$Smoker),
         PhysActivity = get_mode(data_subset$PhysActivity),
         Fruits = get_mode(data_subset$Fruits),
         Veggies = get_mode(data_subset$Veggies),
         HvyAlcoholConsump = get_mode(data_subset$HvyAlcoholConsump),
         Sex = get_mode(data_subset$Sex),
         Education = get_mode(data_subset$Education),
         Income = get_mode(data_subset$Income)) {
  
  # Create a new data frame for prediction
  newdata <- tibble(
    Smoker = factor(Smoker, levels = levels(data_subset$Smoker)),
    PhysActivity = factor(PhysActivity, levels = levels(data_subset$PhysActivity)),
    Fruits = factor(Fruits, levels = levels(data_subset$Fruits)),
    Veggies = factor(Veggies, levels = levels(data_subset$Veggies)),
    HvyAlcoholConsump = factor(HvyAlcoholConsump, levels = levels(data_subset$HvyAlcoholConsump)),
    Sex = factor(Sex, levels = levels(data_subset$Sex)),
    Education = factor(Education, levels = levels(data_subset$Education)),
    Income = factor(Income, levels = levels(data_subset$Income))
  )
  
  # Predict
  pred <- predict(best_model, new_data = newdata, type = "class")
  pred
}

# Example 1: Use all default values
# http://localhost:8000/predict

# Example 2: Specify a few predictors
# http://localhost:8000/predict?Smoker=Yes&PhysActivity=No&Sex=Male

# Example 3: Specify all predictors
# http://localhost:8000/predict?Smoker=No&PhysActivity=No&Fruits=No&Veggies=No&HvyAlcoholConsump=No&Sex=Female&Education=6&Income=2


#* API info endpoint
#* @get /info
function() {
  list(
    name = "Ben Mellin",  
    site_url = "https://benmellinncsu.github.io/project-3/EDA.html" 
  )}

#http://localhost:8000/info

#* Confusion matrix heatmap
#* @serializer png
#* @get /confusion
function() {
  
  # Make predictions on the full dataset
  preds <- predict(best_model, new_data = data_subset, type = "class") %>%
    bind_cols(data_subset %>% select(Diabetes_binary))  # keep true labels
  
  # Compute confusion matrix
  cm <- yardstick::conf_mat(preds, truth = Diabetes_binary, estimate = .pred_class)
  
  # Plot confusion matrix heatmap
  p <- autoplot(cm, type = "heatmap") +
    ggtitle("Confusion Matrix Heatmap")
  
  print(p)
}

# http://localhost:8000/confusion
