##########################################################################
# HOMEWORK 4
# Shabnam Shahrezaei
# Spring 2025
##########################################################################

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(MASS)

# Load dataset
fruitfly <- read.csv("C:/Users/13095/Downloads/fruitfly(in).csv")
# Compute means and standard deviations for sleep
data_summary <- fruitfly %>%
  group_by(type) %>%
  summarise(
    mean_sleep = mean(sleep, na.rm = TRUE),
    sd_sleep = sd(sleep, na.rm = TRUE)
  )

# Extract values for 8 Newly Pregnant and 8 Virgin Females
mean_sleep_pregnant <- data_summary$mean_sleep[data_summary$type == "3"]
sd_sleep_pregnant <- data_summary$sd_sleep[data_summary$type == "3"]

mean_sleep_virgin <- data_summary$mean_sleep[data_summary$type == "5"]
sd_sleep_virgin <- data_summary$sd_sleep[data_summary$type == "5"]

# Compute probabilities using the normal distribution
prob_pregnant <- c(
  pnorm(10, mean_sleep_pregnant, sd_sleep_pregnant),
  pnorm(25, mean_sleep_pregnant, sd_sleep_pregnant) - pnorm(10, mean_sleep_pregnant, sd_sleep_pregnant),
  pnorm(29, mean_sleep_pregnant, sd_sleep_pregnant) - pnorm(25, mean_sleep_pregnant, sd_sleep_pregnant),
  1 - pnorm(29, mean_sleep_pregnant, sd_sleep_pregnant)
)

prob_virgin <- c(
  pnorm(10, mean_sleep_virgin, sd_sleep_virgin),
  pnorm(25, mean_sleep_virgin, sd_sleep_virgin) - pnorm(10, mean_sleep_virgin, sd_sleep_virgin),
  pnorm(29, mean_sleep_virgin, sd_sleep_virgin) - pnorm(25, mean_sleep_virgin, sd_sleep_virgin),
  1 - pnorm(29, mean_sleep_virgin, sd_sleep_virgin)
)
# Create table with results
prob_table <- data.frame(
  Sleep_Hours = c("<= 10", "10 - 25", "25 - 29", "> 29"),
  Pregnant_Prob = round(prob_pregnant, 2),
  Virgin_Prob = round(prob_virgin, 2)
)
print(prob_table)

# 1. Compare the distribution of sleep among the five experimental groups
## (a) Create a density plot
fruitfly %>% 
  ggplot(aes(x = sleep, fill = factor(type))) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Distribution of Sleep Duration Across Experimental Groups", 
       x = "Sleep Duration (hours)", y = "Density", fill = "Group") +
  theme_minimal()

## (b) Identify the group with shortest average sleep duration
shortest_group <- fruitfly %>% 
  group_by(type) %>% 
  summarise(mean_sleep = mean(sleep, na.rm = TRUE), 
            sd_sleep = sd(sleep, na.rm = TRUE)) %>% 
  arrange(mean_sleep) %>% 
  slice(1)
print(shortest_group)

# 2. Compare sleep distribution using normal distribution
mean_sleep_virgin <- mean(fruitfly$sleep[fruitfly$type == "5"], na.rm = TRUE)
sd_sleep_virgin <- sd(fruitfly$sleep[fruitfly$type == "5"], na.rm = TRUE)

mean_sleep_pregnant <- mean(fruitfly$sleep[fruitfly$type == "3"], na.rm = TRUE)
sd_sleep_pregnant <- sd(fruitfly$sleep[fruitfly$type == "3"], na.rm = TRUE)

# 3. Quantile comparison
quantile_table <- data.frame(
  Percentile = c(10, 25, 50, 75, 90),
  Pregnant_Theoretical = qnorm(c(0.1, 0.25, 0.5, 0.75, 0.9), mean_sleep_pregnant, sd_sleep_pregnant),
  Pregnant_Observed = quantile(fruitfly$sleep[fruitfly$type == "3"], probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE),
  Virgin_Theoretical = qnorm(c(0.1, 0.25, 0.5, 0.75, 0.9), mean_sleep_virgin, sd_sleep_virgin),
  Virgin_Observed = quantile(fruitfly$sleep[fruitfly$type == "5"], probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
)
print(quantile_table)

# 4. Binomial distribution approximation
## (a) Estimate survival probability
p_virgin <- mean(fruitfly$sleep[fruitfly$type == "5"] >= 30, na.rm = TRUE)
p_pregnant <- mean(fruitfly$sleep[fruitfly$type == "3"] >= 30, na.rm = TRUE)

## (b) Binomial probabilities
binom_table <- data.frame(
  Surviving = 0:10,
  Pregnant_Prob = dbinom(0:10, size = 10, prob = p_pregnant),
  Virgin_Prob = dbinom(0:10, size = 10, prob = p_virgin)
)
print(binom_table)

## (c) Probability of exactly 6 fruitflies surviving
prob_6_pregnant <- dbinom(6, size = 10, prob = p_pregnant)
prob_6_virgin <- dbinom(6, size = 10, prob = p_virgin)
print(c(prob_6_pregnant, prob_6_virgin))

## (d) Most likely number of fruitflies surviving
likely_pregnant <- which.max(dbinom(0:10, size = 10, prob = p_pregnant)) - 1
likely_virgin <- which.max(dbinom(0:10, size = 10, prob = p_virgin)) - 1
print(c(likely_pregnant, likely_virgin))

## (e) Probability of at least 5 surviving
prob_5plus_pregnant <- sum(dbinom(5:10, size = 10, prob = p_pregnant))
prob_5plus_virgin <- sum(dbinom(5:10, size = 10, prob = p_virgin))
print(c(prob_5plus_pregnant, prob_5plus_virgin))
