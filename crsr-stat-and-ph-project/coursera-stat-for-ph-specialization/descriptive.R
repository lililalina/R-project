# Coursera Statistics for Public Health - Imperial College

getwd()

#import the dataset
g <- read.csv(file ="C:/Users/linan/Documents/GitHub/project/R-project/crsr-stat-and-ph-project/cancer data.csv",
              header = TRUE, sep = ',')

# checking the dataset, datatype, shape, and summary
head(g)
str(g)
summary(g)

# inspect age
g$age
table(g$gender)
table(g$smoking)
table(g$exercise)
table(g$fruit)
table(g$veg)
table(g$cancer)

# Create new variable to sum the daily consumption of fruit and veggies of each patient
g$fruitveg <- g$fruit + g$veg
table(g$fruitveg)

# Create a new variable 'five-a-day' to indicate whether the patient meets the 5-a-day recommendation
g$five_a_day <- ifelse(g$fruitveg >= 5, 1,0)
table(g$five_a_day)

# Display a histogram of the daily fruit and veg consumption of patients
hist(g$fruitveg, main = "Histogram of Daily Fruit and Vegetable Consumption",
     xlab = "Number of Portions", ylab = "Frequency", axes = F)
axis(side = 1, at = seq(0,11,1))
axis(side = 2, at = seq(0, 60, 10))
box()

#Create a new binary of "healthy_BMI"
g$healthy_BMI <- ifelse(g$bmi >= 18.5 & g$bmi <= 24.9, 1, 0)

table(g$healthy_BMI)

# Run a chi-square test to see if there is an association between eating fruit and cancer diagnosis
chisq.test(x = g$five_a_day, y = g$cancer)

# Run a two-tailed t-test to see if there is a difference in mean BMI between those diagnosed with cancer and those not diagnosed with cancere
t.test(bmi ~ cancer, data = g, var.equal = TRUE)

# Run a two-tailed t-test to see if mean BMI of those with cancer is different with one without cancer
t.test(bmi ~ cancer, data = g, var.equal = TRUE, alternative = "two.sided")

# Create binary variable for 'overweight'
g$overweight <- ifelse(g$bmi > 24.9, 1, 0)

table(g$overweight)

# Run a chi-square test to see if there is an association between being overweight and cancer diagnos
chisq.test(x = g$overweight, y = g$cancer)