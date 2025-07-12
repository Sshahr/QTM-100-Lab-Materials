##########################################################################
# HOMEWORK 3
# Shabnam Shahrezaei
# Spring 2025
##########################################################################
# Import the dataset
CDC <- read.csv("C:/Users/13095/Desktop/Emory/Emory2025/QTM100/LAB3/HW/cdc.csv", header = TRUE)

# Examine dataset structure
str(CDC)
summary(CDC)

# Question 2: Compute birth year
CDC$year_birth <- 2013 - CDC$age

# Question 3: Classify BMI category
CDC$BMI_category <- factor(NA, levels = c("Underweight", "Healthy weight", "Overweight", "Obese"))
CDC$BMI_category[CDC$BMIPCT < 5] <- "Underweight"
CDC$BMI_category[CDC$BMIPCT >= 5 & CDC$BMIPCT < 85] <- "Healthy weight"
CDC$BMI_category[CDC$BMIPCT >= 85 & CDC$BMIPCT < 95] <- "Overweight"
CDC$BMI_category[CDC$BMIPCT >= 95] <- "Obese"

# Verify BMI category
table(CDC$BMI_category, CDC$BMIPCT)

# Question 4: Recode gender variable
CDC$gender_label <- factor(CDC$gender, levels = c(0, 1), labels = c("Female", "Male"))

# Verify gender variable
table(CDC$gender, CDC$gender_label)

# Question 5a: Scatter plot of BMI vs Age
install.packages("ggplot2")  # Only run this once
library(ggplot2)
ggplot(CDC, aes(x = age, y = bmi)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of BMI vs Age", x = "Age", y = "BMI") +
  theme_minimal()

# Question 5b: Boxplot of BMI by Gender
ggplot(CDC, aes(x = gender_label, y = bmi, fill = gender_label)) +
  geom_boxplot() +
  labs(title = "Boxplot of BMI by Gender", x = "Gender", y = "BMI") +
  theme_minimal()

# Question 6a: Compute average BMI for male respondents
mean(CDC$bmi[CDC$gender_label == "Male"], na.rm = TRUE)

# Question 6b: Compute percentage of respondents classified as "Obese"
obese_percentage <- sum(CDC$BMI_category == "Obese", na.rm = TRUE) / nrow(CDC) * 100
obese_percentage

# Question 6c: Count number of individuals classified as "Healthy weight"
healthy_count <- sum(CDC$BMI_category == "Healthy weight", na.rm = TRUE)
healthy_count
