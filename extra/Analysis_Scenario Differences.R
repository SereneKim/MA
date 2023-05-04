library(dplyr)
library(car)
library(tibble)
library(MASS)
library(lme4)
require(GGally)
# install.packages("brms")
library(brms)
library(purrr)
# library(nlme)
library(lmerTest)

rm(list=ls())

setwd("/Users/serenekim/Desktop/Thesis_SeorinKim/MasterThesis_Seorin_Kim/data")

abs1 <- read.csv('absolute_measure1.csv', header = T)
abs2 <- read.csv('absolute_measure2.csv', header = T)
head(abs2)

abs1$Cohort <- as.factor(abs1$Cohort)
abs2$Cohort <- as.factor(abs2$Cohort)
abs1$Scenario <- as.factor(abs1$Scenario)
abs2$Scenario <- as.factor(abs2$Scenario)
hist(abs1$Probability)
hist(abs2$Probability)



abs1b <- read.csv('absolute_measure1_sum.csv', header = T)
abs2b <- read.csv('absolute_measure2_sum.csv', header = T)
abs1b
hist(abs1b$Probability_mean)
hist(abs2b$Probability_mean)



# ANOVA --------------------------------------------------------------------

test <- abs1 %>% filter(Cohort == 3)
test

model0 <- lm(Probability ~ 1 + Scenario, data=test)
a1 <- aov(model0)
a11 <- summary(a1)
as.matrix(a11)

length(test$Replication)

abs1 <- na.omit(abs1)
abs2 <- na.omit(abs2)

# Compare 3 scenarios
compare3 <-  function(abs1){
        output <- list()
        for (i in unique(abs1$Cohort)) {
          df <- abs1 %>% filter(Cohort == i) 
          # Check if there are at least two levels in the Scenario variable for the current cohort
          if (length(unique(df$Scenario)) > 2 & length(unique(df$Replication)) > 7) {
            # Fit the linear model
            mod <- lm(Probability ~ 1 + Scenario, data=df)
            # Perform ANOVA on the linear model
            anova1 <- aov(mod)
            anova1_summary <- summary(anova1)
            # Store the summary in the output list with the cohort as the key
            output[[as.character(i)]] <- anova1_summary
          } else {
            message(paste("Cohort", i, "has less than two levels in the Scenario variable and not enough replications. Skipping."))
          }
        }
        return(output)
        }

sum_abs1 <- compare3(abs1)
sum_abs2 <- compare3(abs2)


# Compare 2 scenarios

compare2 <- function(abs1){
          output <- list()
          for (i in unique(abs1$Cohort)) {
            df <- abs1 %>% filter(Cohort == i & Scenario != 1) 
            # Check if there are at least two levels in the Scenario variable for the current cohort
            if (length(unique(df$Scenario)) >= 2 & length(unique(df$Replication)) > 7) {
              # Fit the linear model
              mod <- lm(Probability ~ 1 + Scenario, data=df)
              # Perform ANOVA on the linear model
              anova1 <- aov(mod)
              anova1_summary <- summary(anova1)
              # Store the summary in the output list with the cohort as the key
              output[[as.character(i)]] <- anova1_summary
            } else {
              message(paste("Cohort", i, "has less than two levels in the Scenario variable and not enough replications. Skipping."))
            }
          }
          return(output)
          }

sum2_abs1 <- compare2(abs1)
sum2_abs2 <- compare2(abs2)


# GLMM --------------------------------------------------------------------


model1 <- lmer(formula = Probability ~ 1 + Cohort + (1 | Scenario), data=abs1, REML = TRUE)
anova(model1)
summary(model1)
ranova(model1)


model2 <- lmer(formula = Probability ~ 1 + Cohort + (1 | Scenario), data=abs2, REML = TRUE)
anova(model2)
summary(model2)
ranova(model2)

# Singular
model3 <- lmer(formula = Probability_mean ~ 1  + Cohort + (1| Scenario), data=abs1b, REML = TRUE)
anova(model3)
summary(model3)
ranova(model3)


model4 <- lmer(formula = Probability_mean ~ 1 + Cohort + (1 | Scenario), data=abs2b, REML = TRUE)
anova(model4)
summary(model4)
ranova(model4)


