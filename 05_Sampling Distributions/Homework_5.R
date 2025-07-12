# HW5
# FREQUENCY
setwd("C:/Users/13095/Desktop/Emory/Emory2025/QTM100/LAB5/")
dt<-read.csv("yrbss2013.csv", header = T)

bullied<-factor(dt$bullied)
table(bullied)
prop.table(table(bullied))
barplot(prop.table(table(bullied)), beside = T)

sad<-factor(dt$sad)
table(sad)
prop.table(table(sad))
barplot(prop.table(table(sad)), beside = T)


# for loops
# craete a sample of 100 
samp1<-sample(x=sad, size=400)
prop.table(table(samp1))

# We want 1000 sample of size 100
sample_prop100<-matrix(rep(NA, 1000), nrow = 1000, ncol = 2)
colnames(sample_prop100)<- c("not sad", "sad")
for(i in 1:1000){
  print(paste0('i =', i))
  sample100<-sample(sad, 100)
  sample_prop100[i,]<-prop.table(table(sample100))
}

barplot(colMeans(sample_prop100), names.arg = c("no", "yes"), ylim = c(0,1))

# create a function

hw5<-function(variable, sample_size, number){
  proportion<- matrix(rep(NA,number), nrow = number, ncol = 2)
  var_value<-variable
  
  for(i in seq_len(number)){
    sample<-sample(var_value, sample_size)
    proportion[i,]<-prop.table(table(sample))
    
  }
  print(colMeans(proportion))
}

hw5_a<-hw5(sad, 5000, 200)

sample_prop100<-matrix(rep(NA, 200), nrow = 200, ncol = 2)
colnames(sample_prop100)<- c("not sad", "sad")
for(i in 1:200){
  sample100<-sample(x =sad, 5000)
  sample_prop100[i,]<-prop.table(table(sample100))
}
print(colMeans(sample_prop100))