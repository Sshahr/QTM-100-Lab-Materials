#################################################
# Lab Manual 1
# Shabnam Shahrezaei
# Spring 2025
#################################################

# Setting the working directory

setwd("C:/Users/13095/Desktop/Emory/Emory2025/QTM100/LAB1/data")

# Naming the dataset 

arbuthnot <- read.csv("arbuthnot.csv", header = TRUE)

# We can look at the data by typing its name in the console

arbuthnot

# Using head and tail

head(arbuthnot)
tail(arbuthnot)

# View the dimensions of the data frame

dim(arbuthnot)

# Get a quick overview of the values in the dataset

summary(arbuthnot)

# Selecting a specific column

arbuthnot$boys
arbuthnot$girls

# Adding the first entry (which is year 1629)

5218 + 4683

# We can also calculate the proportion of the boys in 1629 

5218/ (5218 + 4683)

# We can compute for the proportion of boys for all years by assigning a new variable

arbuthnot$propBoys <- arbuthnot$boys /(arbuthnot$boys + arbuthnot$girls)

# View the new variable
arbuthnot

# Making comparisons 

arbuthnot$boys > arbuthnot$girls
sum(arbuthnot$boys > arbuthnot$girls)

# What is the proportion of male baptisms, and does it vary by year?

plot(x = arbuthnot$year, y = arbuthnot$propBoys)

# Connecting with lines

plot(x = arbuthnot$year, y = arbuthnot$propBoys, type = "l")
?plot
?par

# Changing colors
plot(x = arbuthnot$year, y = arbuthnot$propBoys, type = "l", col = 2)
plot(x = arbuthnot$year, y = arbuthnot$propBoys, type = "l", col = "plum")

# All available colors 
colors()

