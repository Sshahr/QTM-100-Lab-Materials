#################################################
# Lab Manual 2
# Shabnam Shahrezaei
# Spring 2025
#################################################

# Setting the working directory
setwd("C:/Users/13095/Desktop/Emory/Emory2025/QTM100/LAB2/data")

# Loading the .txt file
babies <- read.table("babies.txt", header = TRUE)
babies

# Finding out the variable type
str(babies)

# Summarizing the dataset
summary(babies)

# Recoding parity and smoke to factor/categorical variables
babies$parityf <- factor(babies$parity,labels=c("first born","otherwise"))
babies$smokef <- factor(babies$smoke,labels=c("not now","yes now"))

# Summarizing numeric variables
summary(babies$bwt)
mean(babies$bwt)
sd(babies$bwt)
min(babies$bwt)
max(babies$bwt)
median(babies$bwt)
range(babies$bwt)
IQR(babies$bwt)

# Comparing numeric by factor variables
tapply(X = babies$bwt, INDEX = babies$smokef, FUN = sd)
tapply(X = babies$bwt, INDEX = babies$parityf, FUN = mean)

# Visualizing numeric variables
hist(babies$bwt)
boxplot(babies$bwt)

boxplot(babies$bwt ~ babies$smokef)
plot(x = babies$gestation, y = babies$bwt)

# Summarizing a categorical variable
smk.tab <- table(babies$smokef)
smk.tab
addmargins(smk.tab)

# Summarizing two categorical variables
smk.par.tab <- table(babies$smokef,babies$parityf)
addmargins(smk.par.tab)
prop.table(smk.par.tab)

# Row and Column Proportions
prop.table(smk.par.tab, margin = 1)
prop.table(smk.par.tab, margin = 2)


# Creating a bar chart
barplot(smk.par.tab)
barplot(smk.par.tab, beside = T, legend.text = T)
barplot(prop.table(smk.par.tab, margin=2), beside = F, legend.text = T)