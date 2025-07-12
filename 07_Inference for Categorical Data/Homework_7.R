# set directory
setwd("C:/Users/13095/Desktop/Emory/Emory2025/QTM100/LAB7/")
dt<-read.csv(("pharynx(in).csv"), header = TRUE)

# QUESTION1: 
dt$survived_500<-factor(dt$TIME>=500, levels = c(FALSE, TRUE), labels = c("0", "1"))
Survival_Sex_Table <- table(dt$SEX, dt$survived_500)
# Display table and proportions
print(addmargins(Survival_Sex_Table))
print(prop.table(Survival_Sex_Table, margin=1))
#chisqrt test
chi_test <- chisq.test(Survival_Sex_Table, correct=FALSE)
print(chi_test)
surv
print(chi_test$expected)

fisher_test <- fisher.test(Survival_Sex_Table)
print(fisher_test)

#tumur level
Survival_TStage_Table <- table(dt$T_STAGE, dt$survived_500)
chi_test_tstage <- chisq.test(Survival_TStage_Table, correct=FALSE)
print(chi_test_tstage$expected)

#fisher
fisher_test_tstage <- fisher.test(Survival_TStage_Table)
print(fisher_test_tstage)

p_values <- 1-pchisq(c(1,3,5,1,3,5), df=c(1,1,1,2,2,2))
print(p_values)