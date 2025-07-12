# Load dataset
survey <- read.csv("C:/Users/13095/Desktop/Emory/Emory2025/QTM100/LAB10/SurveySp13.csv", header = TRUE)

# Inspect structure
str(survey)
summary(survey$hrs_facebook)
hist(survey$hrs_facebook)
boxplot(survey$hrs_facebook)
# Step 1: Recode hrs_facebook into 3 categories
# 0-1 hrs = "Low", 2-3 hrs = "Medium", 4+ hrs = "High"
# Q1.B 
survey$fb_group<-factor(NA, levels = c("low", "medium", "high"))
survey$fb_group[survey$hrs_facebook>=0 & survey$hrs_facebook<=1 ] <-"low"
survey$fb_group[survey$hrs_facebook>=2 & survey$hrs_facebook<=3 ] <-"medium"
survey$fb_group[survey$hrs_facebook>=4 & survey$hrs_facebook<=23 ] <-"high"
table(survey$fb_group, survey$hrs_facebook)
#Q1.C
boxplot(survey$GPA ~ survey$fb_group, 
        main = 'GPA by hrs of facebook',
        xlab = 'facebook group',
        ylab = 'GPA')


# Check GPA by fb_group
tapply(survey$GPA, survey$fb_group, mean, na.rm = TRUE)



# Histogram per group to check normality (just vibes, not rigorous)
hist(survey$GPA[survey$fb_group == "low"], main = "GPA: Low FB Use")
hist(survey$GPA[survey$fb_group == "medium"], main = "GPA: Medium FB Use")
hist(survey$GPA[survey$fb_group == "high"], main = "GPA: High FB Use")

# Step 3: Conduct ANOVA
anova.fb <- aov(GPA ~ fb_group, data = survey)
summary(anova.fb)

# Step 4: Tukey pairwise comparison
TukeyHSD(anova.fb)
plot(TukeyHSD(anova.fb))



# Step 6: Gender-based GPA test
female <- survey$GPA[survey$gender == "Female"]
male <- survey$GPA[survey$gender == "Male"]
t.test(female, male, var.equal = TRUE)

# Optional sanity check
boxplot(GPA ~ gender, data = survey,
        main = "GPA by Gender",
        xlab = "Gender",
        ylab = "GPA")
