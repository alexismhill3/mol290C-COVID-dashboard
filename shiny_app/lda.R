library(tidyverse)
library(MASS)
library(caret)

subset_data_by_state <- function(county_data, state_choice) {
  choice <- c(state_choice)
  subset_by_state <- county_data %>% filter(State == choice)
  
  # create new column for deaths per capita
  subset_by_state <- subset_by_state %>% 
    mutate(`Deaths per capita` = deaths / `Population`)
  
  # drop rows with NA values, since these can't be used for training or testing
  subset_by_state <- subset_by_state %>% drop_na()
  
  # correct the positive skew by applying a square-root transformation
  subset_by_state$`Deaths per capita` <- sqrt(subset_by_state$`Deaths per capita`)
  
  # calculate mean and std deviation for deaths per capita for the chosen state
  std_dev_deaths_cap <- sd(subset_by_state$`Deaths per capita`)
  mean_deaths_cap <- mean(subset_by_state$`Deaths per capita`)
  
  # Creates a Severity label (low, med, high) based on the deaths per capita in each county
  subset_by_state <- subset_by_state %>% mutate(Severity = case_when(`Deaths per capita` <= (mean_deaths_cap - std_dev_deaths_cap) ~ "low",
                                                                     `Deaths per capita` > (mean_deaths_cap - std_dev_deaths_cap) &
                                                                       `Deaths per capita` <= (mean_deaths_cap + std_dev_deaths_cap) ~ "med",
                                                                     `Deaths per capita` > (mean_deaths_cap + std_dev_deaths_cap) ~ "high"))
  subset_by_state
}


get_train_and_test_sets <- function(state_specific_data) {
  # drop columns that won't be used to create the model
  state_specific_data <- state_specific_data %>% 
    dplyr::select(-c("County", "Code", "State", "Abbrev", "Population", "state", "fips", "cases", "deaths", `Deaths per capita`)) %>%
    drop_na()
   
  # partition data into a test set and a training set
  set.seed(123)
  training_samples <- state_specific_data$Severity %>%
    caret::createDataPartition(p = 0.8, list = FALSE)
  train_data <- state_specific_data[training_samples, ]
  test_data <- state_specific_data[-training_samples, ]
  
  preproc.param <- train_data %>% 
    preProcess(method = c("center", "scale"))
  # Transform the data using the estimated parameters
  train_transformed <- preproc.param %>% predict(train_data)
  test_transformed <- preproc.param %>% predict(test_data)
  
  list("train" = train_transformed, "test" = test_transformed)
}

create_model <- function (train_data) {
  model <- lda(formula = Severity ~ ., data = train_data)
  model
}