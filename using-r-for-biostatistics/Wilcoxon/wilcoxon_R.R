


datawx <- read.csv(file = "Wilcoxon_rank.csv")
View(datawx)

datawx$Diff_6Months_Baseline <-
  datawx$Hb_6Months - datawx$Hb_Baseline
hist(datawx$Diff_6Months_Baseline)


shapiro.test(datawx$Hb_6Months - datawx$Hb_Baseline)
##
## Shapiro-Wilk normality test
##
## data: datawx$Hb_6Months - datawx$Hb_Baseline
## W = 0.70555, p-value = 4.559e-05
#hist(datawx$Diff_6Months_Baseline, breaks = 5)
#datawx datawx can be visualised for confirmation
attach(what = datawx)

median(x = Hb_Baseline)
## [1] 11.55
median(Hb_6Months)
## [1] 13.45
median(Hb_6Months-Hb_Baseline)
## [1] 2.1
quantile(Hb_6Months-Hb_Baseline)
## 0% 25% 50% 75% 100%
## 0.500 1.975 2.100 2.200 2.500


wilcox.test(x = Hb_6Months, y = Hb_Baseline, paired = TRUE)
## Warning in wilcox.test.default(x = Hb_6Months, y = Hb_Baseline, paired =
TRUE):
  ## cannot compute exact p-value with ties
  ##
  ## Wilcoxon signed rank test with continuity correction
  ##
  ## data: Hb_6Months and Hb_Baseline
  ## V = 210, p-value = 9.251e-05
  ## alternative hypothesis: true location shift is not equal to 0