library(tidyverse)
library(brms)

data <- read_csv("data/Turnout_1980_2022_v1.1.csv") %>%
    rename_with(tolower) %>%
    drop_na(state_abv, total_ballots_counted)  %>%
    mutate(
           y = log(total_ballots_counted),
           l_vep = log(vep)
    )

log_model <- brm(total_ballots_counted ~ t2(year) + (vep|state_abv), data = data, cores = 5,  family = lognormal())

pred_data <- read_csv("data/eligible-voters-by-state-2024.csv") %>%
    rename(state_abv = AB, vep = votingEligiblePop) %>%
    mutate(year = 2024) %>%
    mutate(vep = ifelse(is.na(vep), over18Pop, vep))

pred_data$predictions <- predict(log_model, pred_data, allow_new_levels=T)[,1]

pred_data %>%
    select(state_abv, predictions) %>%
    mutate(predictions = round(predictions)) %>%
    arrange() %>%
    print(n=51)



