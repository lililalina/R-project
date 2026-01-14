###############
####between subjects anova in r Dr Paul Christiansen

install.packages("readxl")      ### reads excel
install.packages("emmeans")     ### estimated marginal means and p values
install.packages("car")         ### Levene's test and ncv test
install.packages("sjstats")     ### eta squared and cohens f effect size 

###############pull packages out of the library

library(readxl)
library(emmeans)
library(car)
library(sjstats)

###############turn off scientfic notation

options(scipen = 999)

##############read in the data set

alcohol <- read_excel("C:/Users/prc/Desktop/Covid-19/BOOK/ANOVA btw/ANOVA_btw.xlsx")
View(alcohol)        
attach(alcohol)

############################Tell it you have factors, and what they are called

alcohol$Drinking_motive <- factor(alcohol$Drinking_motive,
                    levels = c(1,2,3),
                    labels = c("Enhancement", "Cope Depression", "Cope Anxiety"))

alcohol$Stress_condition <- factor(alcohol$Stress_condition,
                                  levels = c(1,2),
                                  labels = c("Stress", "Control"))

######################R now knows your factors, the below command breaks down your variables 

summary(alcohol)

######################One way anova, the effect of drinking motive type on ml of beer consumed 

beer1_lm <-lm(ml_of_beer ~ as.factor(Drinking_motive))  ### create a linear model (we have called beer1_lm), DV=ml of beer, IV drinking motive "as.factor"
anova(beer1_lm)                                         ### show the anova results for the linear model
eta_sq(beer1_lm)                                        ### effect size
effectsize::cohens_f(beer1_lm)                          ### effect size
emmeans(beer1_lm, pairwise~Drinking_motive)             ### Post hoc tests, corrections examples below
emmeans(beer1_lm, pairwise~Drinking_motive, adjust="bonferroni")
emmeans(beer1_lm, pairwise~Drinking_motive, adjust="holm")


################test the assumption of homegenity of variance 

leveneTest(beer1_lm)

##########lack of homegeity of variance: Welch's ANOVA although its only a one way (single IV) test

oneway.test(ml_of_beer ~ as.factor(Drinking_motive),
            var.equal=FALSE)

#########################multifactorial ANOVA, we add * between the factors so it computes the interaction term 

beer2_lm <-lm(ml_of_beer ~ as.factor(Drinking_motive)*as.factor(Stress_condition))
anova(beer2_lm)
eta_sq(beer2_lm)
effectsize::cohens_f(beer2_lm)       

####################Post hoc testing as before. We can add corrections with the adjust="_____" command
#########if you ask for the main effects individually it will give a warning
######### "Results may be misleading due to involvement in interactions" so its better to just use the interation command

emmeans(beer2_lm, pairwise~Drinking_motive)
emmeans(beer2_lm, pairwise~Stress_condition)

#############explains the interaction - also gives tests for main effects, controling for the presence of the other variable
############## I have asked for the tests to be between the two stress condtions separtely for the three drinking motive conditions

emmeans(beer2_lm, pairwise~Stress_condition | Drinking_motive)

#########################Test for homogeneity of variance- note theres no Welch test for multifactorial ANOVA

leveneTest(beer2_lm)

##############################################


###################Non-sig = meet the assumption, if significant the White-adjusted ANOVA can be done

ncvTest(beer2_lm)

######### White-adjusted anova for heteroscedasticity

##### same command as above no need to re run.

beer2_lm <-lm(ml_of_beer ~ as.factor(Drinking_motive)*as.factor(Stress_condition))

#####this computes the White adjusted ANOVA - note the capital A in ANOVA in the command 

Anova(beer2_lm, Type="II",
      white.adjust=TRUE)


