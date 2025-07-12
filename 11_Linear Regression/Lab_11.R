# import the data set and look into the variables 
setwd("C:/Users/13095/Desktop/Emory/Emory2025/QTM100/LAB11/")
dt<-read.csv("mariokart(in).csv", header = T)
# examine the structure of the data
str(dt)
#examine the varaiables
summary(dt)

# create a new dataset tha only includes packages with price less than 100
dt_clean<-subset(dt, dt$total_pr<100)
#view historgram
hist(dt_clean$total_pr)

# examine the correlation/relationship between the selling price and the number of bids
plot(dt_clean$n_bids, dt_clean$total_pr)
# difficult to see any trend (random scatter)
cor(dt_clean$n_bids, dt_clean$total_pr )
cor.test(dt_clean$n_bids, dt_clean$total_pr )

# linear regression
test1<-lm(dt_clean$total_pr~dt_clean$n_bids)
summary(test1)

# add the lr line to your scatter plot
abline(test1)

# get confidence interval for B0 and B1
confint(test1)

# regular residual for each observations: It’s the difference between the observed value and the predicted value 
#It tells you how far off your model is for each observation.
test1$residuals
resid(test1)
# standardized residual = residuals/std of residuals: puts allr residuals ona copmmon scale: helps detect outliers
rstandard(test1)
predict(test1)

# interpret residuals with plots
hist(rstandard(test1))
qqnorm(rstandard(test1))
qqline(rstandard(test1))