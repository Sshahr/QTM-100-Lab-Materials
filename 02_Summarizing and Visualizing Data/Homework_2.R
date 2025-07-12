##########################################################################
# HOMEWORK 2
# Shabnam Shahrezaei
# Spring 2025
##########################################################################

# Setting the working directory

setwd("C:/Users/13095/Desktop/Emory/Emory2025/QTM100/LAB2/data")

# Loading the Dataset
psid <- read.csv("PSID.csv", header = TRUE)

# View dataset structure
str(psid)

# Preview data
head(psid)
#There are 200 observations of 6 variables.
PSID <- read.table("PSID.csv", header = TRUE)
#variable: Person ID, categorical, integer
#variable: Year, numerical, integer
#variable: Sex, categorical, numerical
#variable: Race, categorical, numerical
#variable: Age, numerical, numerical
#variable: Log_wage, numerical, numerical
str(PSID)
#114 Male, 86 Female,
summary(PSID)
PSID$sexm <-factor(PSID$sex, labels=c("male","female"))
str(PSID)
summary(PSID)
#A histogram is the most appropriate for describing the distribution of the variable of
age
hist(PSID$age)
#the distribution of age is bimodal, with peaks at the 20-30 ages and the 50-60 ages.
#Two boxplots is most appropriate to compare the variable of log wage across the 2 groups in variable of race
boxplot(PSID$log_wage ~ PSID$race)
#Many outliers in log_wage, particularly in the race of white(1). Outliers in white race
at:
#3.456346 3.459436 1.266353 4.015878 3.823557 1.322680 1.208390 1.307433 3.418597
boxplot.stats(PSID$log_wage)
#
tapply(X = PSID$age, INDEX = PSID$race, FUN = mean)
tapply(X = PSID$age, INDEX = PSID$sex, FUN = mean)
tapply(X = PSID$age, INDEX = PSID$race, FUN = sd)
tapply(X = PSID$age, INDEX = PSID$sex, FUN = sd)
tapply(X = PSID$log_wage, INDEX = PSID$race, FUN = mean, na.rm = TRUE)
tapply(X = PSID$log_wage, INDEX = PSID$sex, FUN = mean, na.rm = TRUE)
tapply(X = PSID$log_wage, INDEX = PSID$race, FUN = sd, na.rm = TRUE)
tapply(X = PSID$log_wage, INDEX = PSID$sex, FUN = sd, na.rm = TRUE)
mean(PSID$age)
sd(PSID$age)
mean(PSID$log_wage, na.rm = TRUE)
sd(PSID$log_wage, na.rm = TRUE)
summary(PSID)
PSID$racew <-factor(PSID$race, labels=c("white","black"))
str(PSID)
summary(PSID)
