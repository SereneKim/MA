library(dplyr)
library(car)
library(tibble)
library(MASS)
library(lme4)
require(GGally)
# install.packages("brms")
library(brms)
library(purrr)
library(Hmisc)
library(foreign)
library(ggplot2)
library(ggforce)


# Data -------------------------------------------------------------------

rm(list=ls())

setwd("/Users/serenekim/Desktop/Thesis_SeorinKim/MasterThesis_Seorin_Kim/data")
 
folder_path = "/Users/serenekim/Desktop/Thesis_SeorinKim/MasterThesis_Seorin_Kim/data"
file_list <- list.files(path = folder_path, pattern = "^s\\d+r\\d+_analysis\\.csv$", full.names = TRUE)
data_all <- lapply(file_list, read.csv)

data_all[50]

# file_names <- Sys.glob(paths = "s*r*_analysis.csv")
# print(file_names)

# Combine all data frames into a single list
df_all <- reduce(data_all[1:30], rbind)
df_extreme <- reduce(data_all[31:50], rbind)

names(df_all)
names(df_extreme) #spouse_edu variable added additionally (newer version of the function used in python)

# do.call("rbind", data_all)

s1 <- df_all %>% filter(Scenario == 1 & Generation != 1)
s2 <- df_all %>% filter(Scenario == 2 & Generation != 1)
s3 <- df_all %>% filter(Scenario == 3 & Generation != 1)
s4 <- df_extreme %>% filter(Scenario == 4 & Generation != 1)
s5 <- df_extreme %>% filter(Scenario == 5 & Generation != 1)

# Correlation
ggpairs(s1[, c("Cohort", "Edu_level", "Generation", "Mother_Edu", "Father_Edu")])
ggpairs(s2[, c("Cohort", "Edu_level", "Generation", "Mother_Edu", "Father_Edu")])
ggpairs(s3[, c("Cohort", "Edu_level", "Generation", "Mother_Edu", "Father_Edu")])


n1 <- s1 %>% group_by(Replication, Cohort) %>% summarise(n=n())
n2 <- s2 %>% group_by(Replication, Cohort) %>% summarise(n=n())
n3 <- s3 %>% group_by(Replication,Cohort) %>% summarise(n=n())


# Get Maximum Education of Parents
max_parents <- function(data){
  for (i in 1:length(data$X)){
  data$Max_Parents[i] <- max(data$Father_Edu[i], data$Mother_Edu[i])
  data$Max_Cult_P[i] <- max(data$Father_Cultural[i], data$Mother_Cultural[i])
  }
  return(data)
}

s1 <- max_parents(s1)
s2 <- max_parents(s2)
s3 <- max_parents(s3)
s4 <- max_parents(s4)
s5 <- max_parents(s5)

s1$sMax_Cult_P <- scale(s1$Max_Cult_P, center = TRUE, scale = TRUE)
s2$sMax_Cult_P <- scale(s2$Max_Cult_P, center = TRUE, scale = TRUE)
s3$sMax_Cult_P <- scale(s3$Max_Cult_P, center = TRUE, scale = TRUE)
s4$sMax_Cult_P <- scale(s4$Max_Cult_P, center = TRUE, scale = TRUE)
s5$sMax_Cult_P <- scale(s5$Max_Cult_P, center = TRUE, scale = TRUE)

s1$sCultural <- scale(s1$Cultural, center = TRUE, scale = TRUE)
s2$sCultural <- scale(s2$Cultural, center = TRUE, scale = TRUE)
s3$sCultural <- scale(s3$Cultural, center = TRUE, scale = TRUE)
s4$sCultural <- scale(s4$Cultural, center = TRUE, scale = TRUE)
s5$sCultural <- scale(s5$Cultural, center = TRUE, scale = TRUE)
# scaling is possible but then interpretation becomes harder and it deosn't seem necessary given the same scales. 
# Also, there's not much difference after plotting the coefficient regression (because it anyway scales a bit).
unique(s1$sCultural)
unique(s1$Cultural)

# Per Cohort --------------------------------------------------------------

# Test for Max_Parents ----------------------------------------------------
"it does not seem to be a good idea to use Max_Parents when
we use the categorical education variable. Coefficients for Max_Parents = NA
because e.g., maximum education levels within a cohort are mostly same"

test = s1 %>% filter(Replication == 1)
unique(test$Cohort)

test2 = test %>% filter(Cohort == 3)


mod1= lm(test2$Edu_level~test2$Mother_Edu*test2$Father_Edu)
mod2 = lm(test2$Cultural~test2$Mother_Cultural*test2$Father_Cultural)
summary(mod2)

summary(mod1) # NA 
coef_m1 <- data.frame(t(data.frame(mod1$coefficients)))
extra <- data.frame(coef(summary(mod1)))
colnames(extra) <- c("Estimate", "SE", "t_stat", "p_value")
extra.t <- data.frame(Intercept_SE = extra$SE[1], Mother_SE = extra$SE[2], Father_SE = extra$SE[3], Interaction_SE = extra$SE[4],
                            Intercept_pr = extra$p_value[1], Mother_pr =extra$p_value[2], Father_pr = extra$p_value[3], Interaction_pr = extra$p_value[4])
cbind(coef_m1, extra.t)
unique(test2$Max_Parents) # Only 4

"Ordinary Logistic Regression because we're considering categorical data.
Then, we can use Max_Parents."
test2$Edu_level <- as.factor(test2$Edu_level)
test2$Mean_EduP <- (test2$Mother_Edu+test2$Father_Edu)*0.5

test2$Mother_Edu <- as.factor(test2$Mother_Edu)
test2$Father_Edu <- as.factor(test2$Father_Edu)
test2$Max_Parents <- as.factor(test2$Max_Parents)

m2 <- polr(test2$Edu_level~ test2$Mean_EduP, Hess = TRUE, method = "logistic")
summary(m2)
ctable <- coef(summary(m2))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p_value" = p)
ctable <- data.frame(ctable)
colnames(ctable) <- c("Values", "SE", "t_value", "p_value")
ctable
ctable$Replication <- 1
ctable$Cohort <- 3  
ctable
output_df <- cbind(output_df, ctable)


# Regression Coefficients per Cohort + Rep -------------------------------------
## Linear With Cultural -----------------------------------------------------------
#  With Interaction --> Hard to interpret!! 
# calculate_coefficients <- function(input_data) {
#   output_df <- data.frame(matrix(ncol = 6, nrow = 0))
#   colnames(output_df) <- c("Intercept", "Mother_Edu", "Father_Edu", "Interaction", 
#                            "Replication", "Cohort")
#   
#   for (i in 1:10){
#     df = input_data %>% filter(Replication == i)
#     for (j in unique(df$Cohort)){
#       df2 = df %>% filter(Cohort == j)
#       lm = lm(df2$Cultural~df2$Mother_Cultural*df2$Father_Cultural)
#       coef_df <- data.frame(t(data.frame(lm$coefficients)))  # Transpose the coefficients dataframe
#       extra <- data.frame(coef(summary(lm)))
#       colnames(extra) <- c("Estimate", "SE", "t_stat", "p_value")
#       extra.t <- data.frame(Intercept_SE = extra$SE[1], Mother_SE = extra$SE[2], Father_SE = extra$SE[3], Interaction_SE = extra$SE[4],
#                             Intercept_pr = extra$p_value[1], Mother_pr =extra$p_value[2], Father_pr = extra$p_value[3], Interaction_pr = extra$p_value[4])
#       colnames(coef_df) <- c("Intercept", "Mother_Edu", "Father_Edu", "Interaction")
#       coef_df$Replication <- i  
#       coef_df$Cohort <- j  
#       binding <- cbind(coef_df, extra.t)
#       output_df <- rbind(output_df, binding)
#     }
#   }
#   return(output_df)
# }


## Linear With Max_Cult_P ---------------------------------------------------------

calculate_coefficients <- function(input_data) {
  output_df <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(output_df) <- c("Intercept", "Parents_Edu", 
                           "Replication", "Cohort")
  for (i in 1:10){
    df = input_data %>% filter(Replication == i)
    for (j in unique(df$Cohort)){
      df2 = df %>% filter(Cohort == j)
      lm = lm(df2$Cultural~df2$Max_Cult_P)
      coef_df <- data.frame(t(data.frame(lm$coefficients)))  # Transpose the coefficients dataframe
      extra <- data.frame(coef(summary(lm)))
      colnames(extra) <- c("Estimate", "SE", "t_stat", "p_value")
      extra.t <- data.frame(Intercept_SE = extra$SE[1], Parents_SE = extra$SE[2],
                            Intercept_pr = extra$p_value[1], Parents_pr =extra$p_value[2])
      colnames(coef_df) <- c("Intercept", "Parents_Edu")
      coef_df$Replication <- i  
      coef_df$Cohort <- j  
      binding <- cbind(coef_df, extra.t)
      output_df <- rbind(output_df, binding)
    }
  }
  return(output_df)
}

# cfr. Ordinal Logistic Regression With Edu (Not working well) --------------------------

calculate_coefficients2 <- function(input_data) {
  output_df <- data.frame()
  input_data$Edu_level <- as.factor(input_data$Edu_level)
  # input_data$Mother_Edu <- as.factor(input_data$Mother_Edu)
  # input_data$Father_Edu <- as.factor(input_data$Father_Edu)
  input_data$Mean_EduP <- (input_data$Mother_Edu+input_data$Father_Edu)*0.5
  for (i in 1:10){
    df = input_data %>% filter(Replication == i)
    for (j in unique(df$Cohort)){
      df2 = df %>% filter(Cohort == j)
      model = polr(df2$Edu_level ~ df2$Mean_EduP, Hess = TRUE, method = "logistic") 
      ctable <- coef(summary(model))
      p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
      ctable <- cbind(ctable, "p_value" = p)
      ctable <- data.frame(ctable)
      colnames(ctable) <- c("Values", "SE", "t_value", "p_value")
      ctable$Replication <- i  
      ctable$Cohort <- j  
      output_df <- rbind(output_df, ctable)
    }
  }
  return(output_df)
}

"
Errors when using anything....
* Max_Parents (singularity issues)
* Both Mother_Edu and Father_Edu
* One of Mother_Edu and Father_Edu
* Mean(Mother_Edu, Father_Edu)
"
result2_s1 <- calculate_coefficients2(s1) 
result2_s2 <- calculate_coefficients2(s2)
result2_s3 <- calculate_coefficients2(s3)


# Apply the function  --------------------------------------------------------------------

# Use the function with different data frames
result_s1 <- calculate_coefficients(s1)
result_s2 <- calculate_coefficients(s2)
result_s3 <- calculate_coefficients(s3)
result_s4 <- calculate_coefficients(s4)
result_s5 <- calculate_coefficients(s5)
head(result_s1)


result_s1$Scenario = 1
result_s2$Scenario = 2
result_s3$Scenario = 3
df <- rbind(result_s1, result_s2, result_s3)
df$label <- ifelse(df$Scenario == 1, "Scenario 1", ifelse(df$Scenario == 2, "Scenario 2", "Scenario 3"))

result_s4$Scenario = 4
result_s5$Scenario = 5
df_ex <- rbind(result_s4, result_s5)
df_ex$label <- ifelse(df_ex$Scenario == 4, "True Homogamy", "True Heterogamy")

plot(result_s4$Parents_pr ~ result_s4$Cohort)
plot(result_s5$Parents_pr ~ result_s5$Cohort)
plot(result_s4$Parents_SE ~ result_s4$Cohort)
plot(result_s5$Parents_SE ~ result_s5$Cohort)


setwd("/Users/serenekim/Desktop/Thesis_SeorinKim/MasterThesis_Seorin_Kim/images")
png(filename = "coef_scens.png", width = 800, res = 100)
ggplot(df, mapping = aes(x = Cohort, color = label)) +
  geom_smooth(aes(y = Parents_Edu), method ="loess") +
  geom_point(aes(y = Parents_Edu, alpha = 0.1)) +
  facet_wrap(.~ label, nrow = 3) +
  labs(x = "Cohort", y = "Parents") +
  theme_light() + theme(legend.position="none", strip.text.x = element_text(size = 10, colour = "black"))+
  theme(strip.background =element_rect(fill="#e6e5e3"))+ 
  scale_color_manual(values=c('#636EFA', '#EF553B', '#00CC96'))
dev.off()

png(filename = "coef_scens_ex.png", width = 800, res = 100)
ggplot(df_ex, aes(x = Cohort, color = label)) +
  geom_smooth(aes(y = Parents_Edu), method ="loess") +
  geom_point(aes(y = Parents_Edu, alpha = 0.1)) +
  facet_wrap(~ label, nrow = 3) +
  labs(x = "Cohort", y = "Parents") +
  theme_light() + theme(legend.position="none", strip.text.x = element_text(size = 10, colour = "black"))+
  theme(strip.background =element_rect(fill="#e6e5e3"))+ 
  scale_color_manual(values=c('#636EFA', '#EF553B', '#00CC96'))
dev.off()


png(filename = "SE_scens_ex.png", width = 800, res = 100)
ggplot(df_ex, aes(x = Cohort, color = label)) +
  geom_smooth(aes(y = Parents_SE), method ="loess") +
  geom_point(aes(y = Parents_SE, alpha = 0.1)) +
  facet_wrap(~ label, ncol = 2) +
  labs(x = "Cohort", y = "Standard Error") +
  theme_light() + theme(legend.position="none", strip.text.x = element_text(size = 10, colour = "black"))+
  theme(strip.background =element_rect(fill="#e6e5e3"))+ 
  scale_color_manual(values=c('#636EFA', '#EF553B', '#00CC96'))
dev.off()

# Zoomed Plots (Reg Coeff) ------------------------------------------------------------
ggplot(result_s1, aes(x = Cohort)) +
  geom_smooth(aes(y = Parents_Edu), method = "loess") +
  geom_point(aes(y = Parents_Edu, alpha=0.5)) +
  labs(x = "Cohort", y = "Parents") +
  facet_zoom( ylim = c(-1, 1)) +
  theme(legend.position="none")

ggplot(result_s2, aes(x = Cohort)) +
  geom_smooth(aes(y = Parents_Edu), method = "loess") +
  geom_point(aes(y = Parents_Edu, alpha=0.5)) +
  labs(x = "Cohort", y = "Parents") +
  facet_zoom( ylim = c(-1, 1)) +
  theme(legend.position="none")

ggplot(result_s3, aes(x = Cohort)) +
  geom_smooth(aes(y = Parents_Edu), method = "loess") +
  geom_point(aes(y = Parents_Edu, alpha=0.5)) +
  labs(x = "Cohort", y = "Parents") +
  facet_zoom( ylim = c(-1, 1)) +
  theme(legend.position="none")


## Extreme Cases
png(filename = "Zcoef_homo.png", width = 800, res = 100)
ggplot(result_s4, aes(x = Cohort)) +
  geom_smooth(aes(y = Parents_Edu), method = "loess", color = "#EF553B") +
  geom_point(aes(y = Parents_Edu, alpha=0.5), color = "#EF553B") +
  labs(x = "Cohort", y = "Parents", title = "True Homogamy") +
  facet_zoom( ylim = c(-1, 1)) + 
  theme_light() + theme(legend.position="none") 
dev.off()

png(filename = "Zcoef_hetero.png", width = 800, res = 100)
ggplot(result_s5, aes(x = Cohort)) +
  geom_smooth(aes(y = Parents_Edu), method = "loess", color = '#636EFA') +
  geom_point(aes(y = Parents_Edu, alpha=0.5), color = '#636EFA') +
  labs(x = "Cohort", y = "Parents", title = "True Heterogamy") +
  facet_zoom( ylim = c(-1, 1)) +
  theme_light() + theme(legend.position="none") 
dev.off()



# Ranking per Cohort + Rep -----------------------------------------------------------------

# Higher ranking = higher value

calculate_meanrank <- function(input_data) {
  output_df <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(output_df) <- c("Mean Rank", "Var Rank", "Total", "Replication", "Cohort")
  
  for (i in 1:10){
    df = input_data %>% filter(Replication == i)
    for (j in unique(df$Cohort)){
      df2 = df %>% filter(Cohort == j) %>% filter(rank(Max_Cult_P) < median(rank(Max_Cult_P)))
      count = df2 %>% summarise(n = n())
      rk = rank(df2$Cultural)
      rank_df <- data.frame(data.frame(mean(rk), var(rk), count))  
      rank_df$Replication <- i  # Add the replication number 'i'
      rank_df$Cohort <- j  # Add the cohort number 'j'
      output_df <- rbind(output_df, rank_df)  # Append the row to the output dataframe
    }}
  # sample <- output_df %>% filter(Cohort <= 55)
  # tb <- sample %>% group_by(Cohort) %>% summarise(
  #   MM_Rank = mean(mean.rk.),
  #   MV_Rank  = mean(var.rk.),
  #   Mean_Total = mean(n),
  #   VM_Rank = var(mean.rk.),
  #   Var_Total = var(n),
  #   Mean_Perc = mean(mean.rk. / n)
  # )
  return(output_df)
}


# Use the function with different data frames
result2_s1 <- calculate_meanrank(s1)
result2_s2 <- calculate_meanrank(s2)
result2_s3 <- calculate_meanrank(s3)
result2_s4 <- calculate_meanrank(s4)
result2_s5 <- calculate_meanrank(s5)

# plot(result2_s1$Mean_Perc ~ result_s1$Cohort) 
# plot(result2_s2$Mean_Perc ~ result_s2$Cohort) 
# plot(result2_s3$Mean_Perc ~ result_s3$Cohort) 
# 
# min(result2_s1$Mean_Perc, na.rm = T)
# min(result2_s2$Mean_Perc, na.rm = T)
# min(result2_s3$Mean_Perc, na.rm = T)
# 
# median(result2_s1$Mean_Perc, na.rm = T)
# median(result2_s2$Mean_Perc, na.rm = T)
# median(result2_s3$Mean_Perc, na.rm = T)



# Combine the data into one dataframe
# df2 <- rbind(
#   data.frame(Scenario = 1, Cohort = result2_s1$Cohort, Mean_Perc = result2_s1$Mean_Perc, VM_Rank = result2_s1$VM_Rank, Var_Total = result2_s1$Var_Total),
#   data.frame(Scenario = 2, Cohort = result2_s2$Cohort, Mean_Perc = result2_s2$Mean_Perc, VM_Rank = result2_s2$VM_Rank, Var_Total = result2_s2$Var_Total),
#   data.frame(Scenario = 3, Cohort = result2_s3$Cohort, Mean_Perc = result2_s3$Mean_Perc, VM_Rank = result2_s3$VM_Rank, Var_Total = result2_s3$Var_Total)
# )

df2 <- rbind(
  data.frame(Scenario = "Scenario 1", Cohort = result2_s1$Cohort, Mean_Rank = result2_s1$mean.rk., Var_Rank = result2_s1$var.rk., Total = result2_s1$n, Perc = result2_s1$mean.rk./result2_s1$n),
  data.frame(Scenario = "Scenario 2", Cohort = result2_s2$Cohort, Mean_Rank = result2_s2$mean.rk., Var_Rank = result2_s2$var.rk., Total = result2_s2$n, Perc = result2_s2$mean.rk./result2_s2$n),
  data.frame(Scenario = "Scenario 3", Cohort = result2_s3$Cohort, Mean_Rank = result2_s3$mean.rk., Var_Rank = result2_s3$var.rk., Total = result2_s3$n, Perc = result2_s3$mean.rk./result2_s3$n)
)

df2_ex <- rbind(
  data.frame(Scenario = "True Homogamy", Cohort = result2_s4$Cohort, Mean_Rank = result2_s4$mean.rk., Var_Rank = result2_s4$var.rk., Total = result2_s4$n, Perc = result2_s4$mean.rk./result2_s4$n),
  data.frame(Scenario = "True Heterogamy", Cohort = result2_s5$Cohort, Mean_Rank = result2_s5$mean.rk., Var_Rank = result2_s5$var.rk., Total = result2_s5$n, Perc = result2_s5$mean.rk./result2_s5$n)
)


# Create the plots (With Percentiles)
png(filename = "rank_scens.png", width = 800, res = 100)
ggplot(df2, mapping = aes(x = Cohort, color = Scenario)) +
  geom_smooth(aes(y = Perc), method = "loess", se = TRUE) +
  geom_point(aes(y = Perc, alpha = 0.1)) +
  facet_wrap(~ Scenario, ncol = 3) +
  labs(x = "Cohort", y = "Percentile of the child rankings") +
  theme_light() + theme(legend.position="none", strip.text.x = element_text(size = 10, colour = "black"))+
  theme(strip.background =element_rect(fill="#e6e5e3"))+ 
  scale_color_manual(values=c('#636EFA', '#EF553B', '#00CC96'))
dev.off()

png(filename = "rank_scens_ex.png", width = 800, res = 100)
ggplot(df2_ex, aes(x = Cohort, color = Scenario)) +
  geom_smooth(aes(y = Perc), method = "loess", se = TRUE) +
  geom_point(aes(y = Perc, alpha = 0.1)) +
  facet_wrap(~ Scenario, ncol = 3) +
  labs(x = "Cohort", y = "Percentile of the child rankings") +
  theme_light() + theme(legend.position="none", strip.text.x = element_text(size = 10, colour = "black"))+
  theme(strip.background =element_rect(fill="#e6e5e3"))+ 
  scale_color_manual(values=c('#636EFA', '#EF553B', '#00CC96'))
dev.off()

# Per Scenario (Zoomed Plots)
ggplot(result2_s1, aes(x = Cohort, y= mean.rk./n)) +
  geom_smooth( method = "loess") +
  geom_point(aes(alpha=0.5)) +
  labs(x = "Cohort", y = "Mean Rank") +
  facet_zoom( xlim = c(0, 40), ylim = c(0.5, 0.6)) +
  theme(legend.position="none")

ggplot(result2_s2, aes(x = Cohort, y= mean.rk./n)) +
  geom_smooth( method = "loess") +
  geom_point(aes(alpha=0.5)) +
  labs(x = "Cohort", y = "Mean Rank") +
  facet_zoom( xlim = c(0, 40), ylim = c(0.5, 0.6)) +
  theme(legend.position="none")

ggplot(result2_s3, aes(x = Cohort, y= mean.rk./n)) +
  geom_smooth( method = "loess") +
  geom_point(aes(alpha=0.5)) +
  labs(x = "Cohort", y = "Mean Rank") +
  facet_zoom( xlim = c(0, 40), ylim = c(0.5, 0.6)) +
  theme(legend.position="none")

ggplot(result2_s4, aes(x = Cohort, y= mean.rk./n)) +
  geom_smooth( method = "loess") +
  geom_point(aes(alpha=0.5)) +
  labs(x = "Cohort", y = "Mean Rank") +
  facet_zoom( xlim = c(0, 40), ylim = c(0.5, 0.6)) +
  theme(legend.position="none")

ggplot(result2_s5, aes(x = Cohort, y= mean.rk./n)) +
  geom_smooth( method = "loess") +
  geom_point(aes(alpha=0.5)) +
  labs(x = "Cohort", y = "Mean Rank") +
  facet_zoom( xlim = c(0, 40), ylim = c(0.5, 0.6)) +
  theme(legend.position="none")
