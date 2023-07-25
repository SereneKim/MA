library(dplyr)
library(car)
library(tibble)
library(MASS)
library(lme4)
require(GGally)
# install.packages("brms")
library(brms)

rm(list=ls())
setwd("/Users/serenekim/Desktop/Thesis_SeorinKim/MasterThesis_Seorin_Kim/data")

df<- read.csv("s1r1_analysis.csv")
df

gen1 <- df %>% filter(df$Generation == 1)
gen1

cor(gen1$Edu_level, gen1$Child1_Edu, use = "pairwise.complete.obs")

plot(gen1$Edu_level, gen1$Child1_Edu)

# lm <- lm(gen1$Child1_Edu~gen1$Edu_level+gen1$Income)
# plot(lm)
# summary(lm)
# 
# gen1$Edu_level.t <- gen1$Edu_level + 1
# qqp(gen1$Edu_level.t, "norm")

ggpairs(df[, c("Cohort", "Edu_level", "Generation", "Child1_Edu", "Child2_Edu")])

ggpairs(gen1[, c("Cohort", "Edu_level", "Child1_Edu", "Child2_Edu")])


