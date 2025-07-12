##########################################################################
# HOMEWORK 1
# Shabnam Shahrezaei
##########################################################################

# Setting the working directory

setwd("C:/Users/13095/Desktop/Emory/Emory2025/QTM100/LAB1/data")

# Loading the Dataset
LB <- read.csv("labor_force.csv", header = TRUE)

# Column names
View(LB)
# We have year, men, women

# Dimensions of the data frame
dim(LB)
# 51 rows and 4 columns 

# What years in the dataset?
summary(LB)
# We have 1970 to 2020

# On propotions 
LB$men > LB$women
sum(LB$men > LB$women)
# Yes, true as well 

# Plot on the proportion of men over time 
LB$propmen <- LB$men/(LB$men + LB$women)
plot(x = LB$year, y = LB$propmen)
# Declining proportion 

# Add a title
plot(x = LB$year, y = LB$propmen, main="Laborforce participation rate of men (1970-2020)")
#title(main = "Laborforce participation rate of men (1970-2020)")

# How many years did the proportion exceed 0.56? 
sum(LB$propmen > 0.56)
# 22 times 

# What year was the highest number of participation 
LB$tot_partic <- LB$men + LB$women
max(LB$tot_partic)
# Year was 2020

LB$propwomen <- LB$women/(LB$men + LB$women)
plot(x = LB$year, y = LB$propwomen)
# Declining proportion 

# Add a title
plot(x = LB$year, y = LB$propwomen, main="Laborforce participation rate of women (1970-2020)")