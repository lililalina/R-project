##Wilcoxon test for paired samples

####################

df<-read.csv("M:/wilcoxonLF.csv")
View(df)
attach(df)

#label group
df$Time<-factor(df$Time,
                levels=c(1,2),
                labels=c("Before", "After"))


###the test
wilcox.test(Despairing~Time, paired=T)
wilcox.test(Despairing~Time, paired=T, exact = FALSE)

#The effect size r
install.packages("rstatix")
library(rstatix)
wilcox_effsize(df, Despairing~Time, paired = T)



