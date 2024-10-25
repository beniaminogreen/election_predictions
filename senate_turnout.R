library(tidyverse)
library(haven)
library(brms)


presidential_data <- read_csv("data/mit_election_data/president_1970_2020/1976-2020-president.csv")
presidential_data_for_joining <- presidential_data %>%
    select(year, state_po, party_simplified, candidatevotes) %>%
    rename(presidential_votes = candidatevotes) %>%
    summarize(presidential_votes = sum(presidential_votes), .by = c(year, state_po))

senate_data <- read_csv("data/mit_election_data/senate_1970_2020/1976-2020-senate.csv") %>%
    filter(stage == "gen") %>%
    mutate(n_candidates = n(), .by = c(state_po, year))

senate_data_for_joining <- senate_data %>%
    rename(senate_votes = candidatevotes) %>%
    summarize(senate_votes = sum(senate_votes), .by = c(n_candidates, special, year, state_po)) %>%
    filter(senate_votes > 5000)

data <- inner_join(presidential_data_for_joining, senate_data_for_joining)
data <- data %>%
    mutate(frac = senate_votes / presidential_votes) %>%
    mutate(year_sq = (year-mean(year))^2, year_qb = (year -mean(year))^3)

year_mean = mean(data$year, na.rm=T)
year_sq_sd = sd(data$year_sq)
year_qb_sd = sd(data$year_qb)

data <- data %>%
    mutate(
           year_sq = year_sq / year_sq_sd, year_qb=year_qb / year_qb_sd) %>%
    filter(n_candidates != 1)

lb <- quantile(data$frac, .02)

brm_model <- brm(frac ~ year + year_sq + (1|n_candidates) + (1|state_po), data = data, cores = 5)

pred_data <- expand_grid(
            state_po = unique(data$state_po),
            year = 2024,
            year_sq = ((2024 - year_mean)^2)/year_sq_sd,
            year_qb = ((2024 - year_mean)^3)/year_qb_sd,
            n_candidates = 2
)

pred_data$Prediction <- predict(brm_model, pred_data)[,1]
pred_data$Category = "Senate turnout"
pred_data <- pred_data %>%
    rename(State = state_po)

pred_data %>%
    select(State, Category, Prediction) %>%
    arrange(State) %>%
    print(n = 50)

