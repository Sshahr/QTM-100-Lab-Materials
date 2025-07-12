# Import the dataset
hw2_data <- read.csv("C:/Users/13095/Desktop/Emory/Emory2025/QTM100/LAB3/HW/cdc.csv", header = TRUE)

# Examine dataset structure
str(hw2_data)
summary(hw2_data)

# Question 2: Compute birth year
hw2_data$year_birth <- 2013 - hw2_data$age

# Question 3: Classify BMI category
hw2_data$BMI_category <- factor(NA, levels = c("Underweight", "Healthy weight", "Overweight", "Obese"))
hw2_data$BMI_category[hw2_data$BMIPCT < 5] <- "Underweight"
hw2_data$BMI_category[hw2_data$BMIPCT >= 5 & hw2_data$BMIPCT < 85] <- "Healthy weight"
hw2_data$BMI_category[hw2_data$BMIPCT >= 85 & hw2_data$BMIPCT < 95] <- "Overweight"
hw2_data$BMI_category[hw2_data$BMIPCT >= 95] <- "Obese"

# Verify BMI category
table(hw2_data$BMI_category,hw2_data$BMIPCT)

# Question 4: Recode gender variable
hw2_data$gender_label <- factor(hw2_data$gender, levels = c(0, 1), labels = c("Female", "Male"))

# Verify gender variable
table(hw2_data$gender, hw2_data$gender_label)

# Question 5a: Scatter plot of BMI vs Age
install.packages("ggplot2")
library(ggplot2)
ggplot(hw2_data, aes(x = age, y = bmi)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot of BMI vs Age", x = "Age", y = "BMI") +
  theme_minimal()

# Question 5b: Boxplot of BMI by Gender
ggplot(hw2_data, aes(x = gender_label, y = bmi, fill = gender_label)) +
  geom_boxplot() +
  labs(title = "Boxplot of BMI by Gender", x = "Gender", y = "BMI") +
  theme_minimal()

# Question 6a: Compute average BMI for male respondents
mean(hw2_data$bmi[hw2_data$gender_label == "Male"], na.rm = TRUE)

# Question 6b: Compute percentage of respondents classified as "Obese"
obese_percentage <- sum(hw2_data$BMI_category == "Obese", na.rm = TRUE) / nrow(hw2_data) * 100
obese_percentage

# Question 6c: Count number of individuals classified as "Healthy weight"
healthy_count <- sum(hw2_data$BMI_category == "Healthy weight", na.rm = TRUE)
healthy_count