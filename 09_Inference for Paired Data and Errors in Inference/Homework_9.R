# Lab 9 - Inference on weight_kg

# Set working directory (edit the path)
setwd("C:/Users/13095/Desktop/Emory/Emory2025/QTM100/LAB9/")

# Load the dataset
yrbss <- read.csv("yrbss2013 (2).csv", header = TRUE)

# Source the custom functions
source("TestingFunctions_forLab9.R")

# (a) Population Distribution of weight_kg
hist(yrbss$weight_kg, main = "Population Distribution of weight_kg", xlab = "weight_kg")
mean(yrbss$weight_kg)  # True population mean
sd(yrbss$weight_kg)    # True population standard deviation

# (b) Check sampling distribution assumptions
# Sample size of 300 → CLT likely satisfied, if shape not extreme

# (c) Fill in:
# H0: mu = true_mean vs Ha: mu ≠ true_mean
# Risk of committing Type I error if H0 is actually true
# Targeted Type I error rate = 0.05
# Targeted CI coverage = 95%

# (d) Perform inference for 100 samples of size 20
true_mean <- mean(yrbss$weight_kg)
sim <- inference.means(variable = yrbss$weight_kg,
                       sample.size = 20,
                       alpha = 0.05,
                       num.reps = 100)

# Sample means
hist(sim$samp.est, main = "Sample Means", xlab = "Sample Mean of weight_kg")

# t-test statistics
hist(sim$test.stat, main = "t Test Statistics", xlab = "t-stat")

# p-values
hist(sim$p.val, main = "p-values", xlab = "p-value")

# Type I Error Rate
table(sim$decision)  # Proportion of 'reject' = error rate

# (e) Plot confidence intervals
plot.ci(results = sim, true.val = true_mean)

# Numeric summary of CI coverage
table(sim$capture)

# Contingency table between hypothesis decision and CI coverage
table(sim$capture, sim$decision)
