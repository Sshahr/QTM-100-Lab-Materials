yrbss <- read.csv("C:/Users/13095/Desktop/Emory/Emory2025/QTM100/LAB8/yrbss2013 (2).csv", header = TRUE)
unique(yrbss$drive_text)

yrbss$drive_text_num <- NA
# Assign numeric values
yrbss$drive_text_num[yrbss$drive_text == "not drive"] <- NA
yrbss$drive_text_num[yrbss$drive_text == "0 days"] <- 0
yrbss$drive_text_num[yrbss$drive_text == "1 or 2 days"] <- mean(c(1,2))
yrbss$drive_text_num[yrbss$drive_text == "3 to 5 days"] <- mean(c(3,5))
yrbss$drive_text_num[yrbss$drive_text == "6 to 9 days"] <- mean(c(6,9))
yrbss$drive_text_num[yrbss$drive_text == "10 to 19 days"] <- mean(c(10,19))
yrbss$drive_text_num[yrbss$drive_text == "20 to 29 days"] <- mean(c(20,29))
yrbss$drive_text_num[yrbss$drive_text == "all 30 days"] <- 30

### Question 1a: Summary and Histogram
summary(yrbss$drive_text_num)
sd(yrbss$drive_text_num, na.rm = TRUE)
hist(yrbss$drive_text_num, main = "Distribution of drive_text", xlab = "Days Texted While Driving")

### Question 2a: 100 samples of size 10
set.seed(123)
sample_means_100_10 <- rep(NA, 100)
for (i in 1:100) {
  samp <- sample(drive_text_num, 10, replace = TRUE)
  sample_means_100_10[i] <- mean(samp)
}
mean(sample_means_100_10)
sd(sample_means_100_10)
hist(sample_means_100_10, main = "100 Samples of Size 10", xlab = "Sample Mean")

### Question 2b: 5000 samples of size 10
set.seed(123)
sample_means_5000_10 <- rep(NA, 5000)
for (i in 1:5000) {
  samp <- sample(drive_text, 10, replace = TRUE)
  sample_means_5000_10[i] <- mean(samp)
}
mean(sample_means_5000_10)
sd(sample_means_5000_10)
hist(sample_means_5000_10, main = "5000 Samples of Size 10", xlab = "Sample Mean")

### Question 2c: 200 samples of size 20
set.seed(123)
sample_means_200_20 <- rep(NA, 200)
for (i in 1:200) {
  samp <- sample(drive_text, 20, replace = TRUE)
  sample_means_200_20[i] <- mean(samp)
}
mean(sample_means_200_20)
sd(sample_means_200_20)
hist(sample_means_200_20, main = "200 Samples of Size 20", xlab = "Sample Mean")

### Question 2d: 200 samples of size 5000
set.seed(123)
sample_means_200_5000 <- rep(NA, 200)
for (i in 1:200) {
  samp <- sample(drive_text, 5000, replace = TRUE)
  sample_means_200_5000[i] <- mean(samp)
}
mean(sample_means_200_5000)
sd(sample_means_200_5000)
hist(sample_means_200_5000, main = "200 Samples of Size 5000", xlab = "Sample Mean")

############################################################################################3
# part two
lead <- read.csv("lead.csv", header = TRUE)

### Question 1: Explore IQ
# a. Mean and standard deviation
mean(lead$Iqf, na.rm = TRUE)
sd(lead$Iqf, na.rm = TRUE)

# b. Check normality
hist(lead$Iqf, main = "Histogram of IQ", xlab = "Full Scale IQ")
qqnorm(lead$Iqf); qqline(lead$Iqf)

### Question 2: Test if IQ is different from 85
# a. One-sample t-test
# H0: mean IQ = 85
# Ha: mean IQ ≠ 85
t.test(lead$Iqf, mu = 85)

# b. Same t-test but with 99% confidence interval
t.test(lead$Iqf, mu = 85, conf.level = 0.99)

# One-sided test (e.g., Ha: mean IQ < 85)
t.test(lead$Iqf, mu = 85, alternative = "less")

### Question 3: Test if 1972 blood lead levels differ from 36
# a. One-sample t-test
# H0: mean Ld72 = 36
# Ha: mean Ld72 ≠ 36
t.test(lead$Ld72, mu = 36)

# b. View histogram and normality for context
hist(lead$Ld72, main = "Histogram of Ld72", xlab = "Blood Lead Level (1972)")
qqnorm(lead$Ld72); qqline(lead$Ld72)