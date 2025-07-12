# data is .txt thus use read.table
setwd("C:/Users/13095/Desktop/Emory/Emory2025/QTM100/LAB6")
dt<-read.table("gardasil.txt", header = T, stringsAsFactors = T)

# explor the data
str(dt)
summary(dt)

# look at practicetype
practice_count<-table(dt$PracticeType)
pediatric<-practice_count[3]
total_count<-sum(practice_count)

# run one sample z test on the proportion
prop.test(pediatric, total_count, p = 0.5, correct = F)

# z statistics will be sqrt xhi squared
z_stat<-sqrt(103.81)

# age group
age_count<-table(dt$AgeGroup)
age_prop<-prop.table(age_count)

# z test for age group
prop.test(age_count, p = 0.53, correct = F)

# p value
z_age<-sqrt(6.51)
prob_age<-2*(1-pnorm(z_age))