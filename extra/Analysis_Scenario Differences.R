rm(list=ls())

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

setwd("/Users/serenekim/Desktop/Thesis_SeorinKim/MasterThesis_Seorin_Kim/data")

abs1 <- read.csv('absolute_measure1.csv', header = T)
abs2 <- read.csv('absolute_measure2.csv', header = T)

abs1_ex <- read.csv('absolute_measure1_all.csv', header = T)  %>% filter(Scenario >=4)
abs2_ex <- read.csv('absolute_measure2_all.csv', header = T)  %>% filter(Scenario >=4)


hist(abs1$Probability)
hist(abs2$Probability)


abs1b <- read.csv('absolute_measure1_sum.csv', header = T)
abs2b <- read.csv('absolute_measure2_sum.csv', header = T)

abs1b_ex <- read.csv('absolute_measure1_sum_all.csv', header = T) %>% filter(Scenario >=4)
abs2b_ex <- read.csv('absolute_measure2_sum_all.csv', header = T) %>% filter(Scenario >=4)



hist(abs1b$Probability_mean)
hist(abs2b$Probability_mean)


s4 <- abs1b_ex %>%  filter(abs1b_ex$Scenario == 4)
s5 <- abs1b_ex %>%  filter(abs1b_ex$Scenario == 5)
leveneTest(abs1b_ex$Probability_mean, group = abs1b_ex$Scenario)
t.test(s4$Probability_mean, s5$Probability_mean) #p-value = 0.0004035 different


# ANOVA --------------------------------------------------------------------

# Compare 3 scenarios
compare3 <-  function(abs1){
        output <- list()
        for (i in unique(abs1$Cohort)) {
          df <- abs1 %>% filter(Cohort == i) 
          df$Scenario <- as.factor(df$Scenario)
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


abs1 <- na.omit(abs1)
abs2 <- na.omit(abs2)

sum_abs1 <- compare3(abs1)
sum_abs2 <- compare3(abs2)


# Compare 2 scenarios

compare2 <- function(abs1){
          output <- list()
          for (i in unique(abs1$Cohort)) {
            df <- abs1 %>% filter(Cohort == i & Scenario != 1)
            df$Scenario <- as.factor(df$Scenario)
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

# Between Extreme Cases: More distinct! More significantly!
abs1_ex <- na.omit(abs1_ex)
abs2_ex <- na.omit(abs2_ex)
sum_abs1_ex <- compare2(abs1_ex)
sum_abs2_ex <- compare2(abs2_ex)

# GLMM --------------------------------------------------------------------

abs1b_ex$Scenario <- as.factor(abs1b_ex$Scenario)
# Extreme cases
model1 <- lmer(formula = Probability_mean ~ 1 + Cohort + (1 | Scenario), data=abs1b_ex, REML = TRUE)
anova(model1)
summary(model1)
ranova(model1)

abs2b_ex$Scenario <- as.factor(abs2b_ex$Scenario)
model2 <- lmer(formula = Probability_mean ~ 1 + Cohort + (1 | Scenario), data=abs2b_ex, REML = TRUE)
anova(model2)
summary(model2)
ranova(model2)


# Between 3 Scenarios
# Singular
abs1b$Scenario <- as.factor(abs1b$Scenario)
model3 <- lmer(formula = Probability_mean ~ 1 + Cohort + (1 | Scenario), data=abs1b, REML = TRUE)
anova(model3)
summary(model3)
ranova(model3)

abs2b$Scenario <- as.factor(abs2b$Scenario)
model4 <- lmer(formula = Probability_mean ~ 1 + Cohort + (1 | Scenario), data=abs2b, REML = TRUE)
anova(model4)
summary(model4)
ranova(model4)



