cat("\n #### Descriptive Statistics and Linear Regression #### \n")

library(EwR)
library(moments)
library(lmtest)
library(forecast)
library(Hmisc)

# Reading ready data from package in R
data(REcoData)

sue=REcoData$SUE
dk=REcoData$DK
io=REcoData$IO
enf=REcoData$ENF
ihr=REcoData$IHR
ith=REcoData$ITH

str(sue)
str(dk)
str(io)
str(enf)
str(ihr)
str(ith)

cat("\n Descriptive Statistics will be performed and tabulated for each dataset for the Project Assignment. \n")
cat("Descriptive Statistics for each dataset: Number of Observations, Mean, Median, Min, Max, Variance, Skewness: \n")
#DK = Exchange Rate
length(dk)
mean(dk, na.rm=TRUE)
median(dk, na.rm=TRUE)
min(dk)
max(dk)
var(dk)
moments::skewness(dk)

#SUE = Industrial Production Index
length(sue)
mean(sue, na.rm=TRUE)
median(sue, na.rm=TRUE)
min(sue)
max(sue)
var(sue)
moments::skewness(sue)

#IO = Unemployment Rate
length(io)
mean(io, na.rm=TRUE)
median(io, na.rm=TRUE)
min(io)
max(io)
var(io)
moments::skewness(io)

#ENF = Inflation Rate 
length(enf)
mean(enf, na.rm=TRUE)
median(enf, na.rm=TRUE)
min(enf)
max(enf)
var(enf)
moments::skewness(enf)

#IHR = Export 
length(ihr)
mean(ihr, na.rm=TRUE)
median(ihr, na.rm=TRUE)
min(ihr)
max(ihr)
var(ihr)
moments::skewness(ihr)

#ITH = Import 
length(ith)
mean(ith, na.rm=TRUE)
median(ith, na.rm=TRUE)
min(ith)
max(ith)
var(ith)
moments::skewness(ith)

cat("\n Gives descriptive statistics and graph of variables: \n")
summary(REcoData)
describe(REcoData)

plot(sue, main = "Industrial Production Index")
hist(sue, main = "Industrial Production Index")
plot(dk, main = "Exchange Rate")
hist(dk, main = "Exchange Rate")
plot(io, main = "Unemployment Rate")
hist(io, main = "Unemployment Rate")
plot(enf, main = "Scatter plot of Inflation Rate")
hist(enf, main = "Histogram of Inflation Rate")
plot(ihr, main = "Scatter plot of Export")
hist(ihr, main= "Histogram of Export")
plot(ith, main = "Scatter plot of Import")
hist(ith, main = "Histogram of Import")

#Dependent variable : Inflation Rate(inf)
#Independent variable : Export(ihr), Import(ith)

#Correlation check for variables
enf_cor<-REcoData[,c("ENF","IHR","ITH")]
cor_matrix <- cor(enf_cor, use = "complete.obs")
cor_matrix
round(cor_matrix,4)


pairs(~ENF+IHR+ITH, data=REcoData, pch=16,cex=1)


cat("\n Simple regression and Multivariate linear regression model. \n") 
#Simple Regression ENF~IHR
# Y = B0 + B1*X1 + e
model3 = lm(enf~ihr)
summary(model3)
# ENF = 871.531 - 6.083*IHR
# Rquared = 0.491
# p-value < 0.001

#Simple Regression ENF~ITH
# Y = B0 + B1*X1 + e
model4 = lm(enf~ith)
summary(model4)
# ENF = 617.147 -3.633*ITH
# Rsquared = 0.425
# p-value < 0.001

#Multiple Regression, ENF~IHT+ITH
# Y = B0 + B1*X1 + B2*X2 + B3*X3 + e
# ENF = C + B1*IHR + B2*ITH + e
# ENF^ = C + B1*IHR + B2*ITH
# ENF - ENF^  =  e 

model5 = lm(enf~ihr+ith)
summary(model5)
confint(model5)
# ENF = 1021.81  -10.38*IHR + 2.86*ITH + e
# Rsquared = 0.503
# p-value < 0.001


# For coefficient significance, the p-values of the t-statistic test should be less than 0.05
# ENF = 1021.81  -10.38*IHR + 2.86*ITH + e


cat("\n #### Reading Data  #### \n")
# excel file is saved as".csv" and uploaded from menu.
# data have to consist at least 2 independent variables and 1 dependent variable.
# Get time series, monthly series recommended, at least 60 observations for all variables.
# Load the dataset

tablo.oku <- REcoData[, c("ENF", "IHR", "ITH")]

head(tablo.oku)
str(tablo.oku)


cat("\n #### Regresyon assumptions  #### \n ") 
## investigating model is spurious regression or not?

ENF=tablo.oku$ENF
IHR=tablo.oku$IHR
ITH=tablo.oku$ITH

model6 = lm(ENF~IHR+ITH)
summary(model6)
# ENF = 1021.81  -10.38*IHR + 2.86*ITH + e
# Rsquared = 0.503
# p-value < 0.001

# i. The White test: If p < 0.05, there is heteroscedasticity(variance is not constant)
cat("\n >>> White Test, check variance constant or not? \n")
bptest(model6, ~ ihr*ith+I(ihr^2)+I(ith^2))
#BP 55.255, p-value<0.001 -->reject null hypothesis --> heteroscedasticity

# ii. The Durbin-Watson(DW) test checks for first-order autocorrelation.
# The null hypothesis (H0): There is no autocorrelation; if p < 0.05, H0 is rejected.
cat("\n >>> DW test, check autocorrelation exists or not? \n")
library(lmtest)
lmtest::dwtest(model6)
#DW = 0.055, p-value < 0.001 --> reject the null hypothesis --> autocorrelation

#Indication that the model is a spurious regression

cat("\n #### spurious regression #### \n")
# if coefficients of model, F-test model gives significant results (p<0.05) and R-Square is high but DW test results gives residuals have autocorrelation then this is a spurious regression.


cat("\n #### Stationary Series #### \n")
# to avoid spurious regression, we need to check whether the series is stationary or not. 
# stationary means that the series is not changing over time: mean and variance
# to make series series stationary, we need to remove trend and seasonal component.
# taking diffeerence of series, we can see that the series is stationary.

# if there is heteroscedasticity, to make series more stationary get log of them.
cat("\n >>> getting log of series: \n")
logged.enf <-log(enf)
logged.ihr  <-log(ihr)
logged.ith  <-log(ith)

# plot graphs
plot(logged.enf, main="logged enf")
plot(logged.ihr, main= "logged ihr")
plot(logged.ith, main="logged ith")

#Use log10
log10.enf <-log10(enf)
log10.ihr  <-log10(ihr)
log10.ith  <-log10(ith)

# plot graphs
plot(log10.enf, main="log10 enf")
plot(log10.ihr, main= "log10 ihr")
plot(log10.ith, main="log10 ith")


# To eliminate autocorellation, first differenciating the series 
cat("\n >>> getting first diff of the logged series: \n")
dif.logged.enf <-diff(logged.enf)
dif.logged.ihr  <-diff(logged.ihr)
dif.logged.ith  <-diff(logged.ith)

# plot graphs
plot(dif.logged.enf, main="diff.logged.enf")
plot(dif.logged.ihr, main="diff.logged.ihr")
plot(dif.logged.ith, main="diff.logged.ith")

#dif log10
dif.log10.enf <-diff(log10.enf)
dif.log10.ihr  <-diff(log10.ihr)
dif.log10.ith  <-diff(log10.ith)

# plot graphs
plot(dif.log10.enf, main="diff.log10.enf")
plot(dif.log10.ihr, main="diff.log10.ihr")
plot(dif.log10.ith, main="diff.log10.ith")

cat("\n Employ model again with differenciated series and DW test \n")
# Employ regression again with differenciated series and check DW test for autocorrelation (whether spurios regresion or not) 
dif.model2 = lm(dif.logged.enf~dif.logged.ihr+dif.logged.ith)
summary(dif.model2)

dif.model3 = lm(dif.log10.enf~dif.log10.ihr+dif.log10.ith)
summary(dif.model3)

library(lmtest)
lmtest::dwtest(dif.model2)

cat("\n #### Component of Time series #### \n")
# Y = Trend + Seasonal + Cylical fluctuations + Residual

# first, convert the orijinal series to time series form from vector form.
ts.enf<-ts(enf,start=c(1990,1),frequency=12)
ts.ihr<-ts(ihr,start=c(1990,1),frequency=12)
ts.ith<-ts(ith,start=c(1990,1),frequency=12)

# plot them and see trend, seasonal and residual part
plot(decompose(ts.enf))
plot(decompose(ts.ihr))
plot(decompose(ts.ith))


cat("\n #### ARIMA Models #### \n")
# AR - I - MA

# AR
# Yt = a+bY(t-1) + e  AR(1)
# Yt = a+bY(t-1) + bY(t-2) + e  AR(2)

# MA
# Yt = b*e(t-1) + e(t-2)    MA(2)

# I
# I: Integrated part(differenced number)

# dYt = a+b*dY(t-1) + b*e(t-1) + e  ARIMA(1,1,1)
# ARIMA(p,d,q) for p:AR, d:I, q:MA

# Arima reading:
#https://otexts.com/fpp2/arima-r.html
#https://www.rdocumentation.org/packages/forecast/versions/8.22.0/topics/auto.arima
#https://people.cs.pitt.edu/~milos/courses/cs3750/lectures/class16.pdf

# select one series and lets estimate a model with arima
# ENF as model with arima
enf=tablo.oku$ENF
log10.enf=log10(enf)
diff_log10_enf=diff(log10.enf)

# for best model we need to put right lagged values. Later, check coefficient whether significant and low AIC value is the better.
enf1.2.1.1<-arima(enf, order=c(2,1,1)) # AR(2), I(1), MA(1)
enf1.2.1.1
coeftest(enf1.2.1.1)

enf1.2.0.1<-arima(enf, order=c(2,0,1)) # AR(2), I(1), MA(1)
enf1.2.0.1
coeftest(enf1.2.0.1)


# check Rplots.pdf file
acf(enf)
pacf(enf)

#enf2.2.1.1<-arima(log10.enf, order=c(2,1,1)) # AR(2), I(1), MA(1)
#enf2.2.1.1
#coeftest(enf2.2.1.1)

#enf2.2.0.1<-arima(log10.enf, order=c(2,0,1)) # AR(2), I(1), MA(1)
#enf2.2.0.1
#coeftest(enf2.2.0.1)

#error in log10.enf shows that the AR non-stationary, so using the diff format of log10_enf

# for best model we need to put right lagged values. Later, check coefficient whether significant and low AIC value is the better.
enf3.2.1.1<-arima(diff_log10_enf, order=c(2,1,1)) # AR(2), I(1), MA(1)
enf3.2.1.1
coeftest(enf3.2.1.1)

enf3.2.0.1<-arima(diff_log10_enf, order=c(2,0,1)) # AR(2), I(1), MA(1)
enf3.2.0.1
coeftest(enf3.2.0.1)

#arima(2,0,1) shows smaller aic 

cat("\n #### ACF(autocorrelation function) and PACF(partial autocorrellation function  #### \n") 
# to determine rigth AR and MA lag, we need to check ACF and PACF 

# Example of interpretting ACF and PACF graphs:
# We have an AR(p) model when the ACF is exponentially decaying or sinusoidal there is a significant spike at lag p in PACF, but none beyond p.
# we have an MA(1) model when the PACF is exponentially decaying and there is a single significant spike in ACF

# check Rplots.pdf file
acf(diff_log10_enf)
pacf(diff_log10_enf)

cat("\n #### auto Arima and Forecast #### \n")
### !!! ###
### to run auto.arima command and make forecast in "replit",
# a little bit work here about package installing..:

# First;
## 1. open menu three point (right near of the "Files") and show hidden files 
## 2. find "replit.nix" file and open it.
## 3. The file should contain as following "exactly": (do not forget clear "#" sign for eachline when you are copying below text to file ):

# { pkgs }: {
# deps = [
#  pkgs.R
#  pkgs.rPackages.curl
#  pkgs.rPackages.magrittr
#  pkgs.rPackages.forecast
# ];
# }

# turn back "Packages" tab,
# find "curl" package and install it
# find "magrittr" package and install it
# find "forecast" package and install it
# be sure all these packages installed and shown on your package tab.
# if you able too see your package in your "package tab" with "remove" button, it already was installed.. 
# then you can run below program with your data to forecast.


library(forecast)

# Using ts.enf
ts.enf<-ts(enf,start=c(1990,1),frequency=12)
# auto.arima
enf.arima<-forecast::auto.arima(ts.enf,seasonal=FALSE, trace=TRUE)
enf.arima
coeftest(enf.arima)
forecast::forecast(enf.arima,h=12)
plot(forecast::forecast(enf.arima,h=12))


cat(" \n Seasonality modellin for ARIMA \n")
# ARIMA also has seasonal part: SARIMA(p,d,q)(P,D,Q)[F]
# P: seasonal AR, D:seasonal diffencing, Q:seasonal MA
enf.arima<-forecast::auto.arima(ts.enf,seasonal=TRUE, trace=TRUE)
enf.arima
coeftest(enf.arima)
forecast::forecast(enf.arima,h=12)
plot(forecast::forecast(enf.arima,h=12))

#check Rplots for forecasted series
acf(ts.enf)
pacf(ts.enf)


#Using diff_log10_enf
ts.diff_log10_enf<-ts(diff_log10_enf,start=c(1990,1),frequency=12)
# auto.arima
enf.arima<-forecast::auto.arima(ts.diff_log10_enf,seasonal=FALSE, trace=TRUE)
enf.arima
coeftest(enf.arima)
forecast::forecast(enf.arima,h=12)
plot(forecast::forecast(enf.arima,h=12))


cat(" \n Seasonality modellin for ARIMA \n")
# ARIMA also has seasonal part: SARIMA(p,d,q)(P,D,Q)[F]
# P: seasonal AR, D:seasonal diffencing, Q:seasonal MA
enf.arima<-forecast::auto.arima(ts.diff_log10_enf,seasonal=TRUE, trace=TRUE)
enf.arima
coeftest(enf.arima)
forecast::forecast(enf.arima,h=12)
plot(forecast::forecast(enf.arima,h=12))

#check Rplots for forecasted series
acf(ts.diff_log10_enf)
pacf(ts.diff_log10_enf)



