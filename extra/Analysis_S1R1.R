library(dplyr)
library(car)
library(tibble)
library(MASS)
library(lme4)
require(GGally)
# install.packages("brms")
library(brms)
library(purrr)

rm(list=ls())

setwd("/Users/serenekim/Desktop/Thesis_SeorinKim/MasterThesis_Seorin_Kim/data")

data_all <- lapply(Sys.glob(paths = "s*r*_analysis.csv"), read.csv)

# file_names <- Sys.glob(paths = "s*r*_analysis.csv")
# print(file_names)

# Combine all data frames into a single list
df_all <- reduce(data_all, rbind)
  
# do.call("rbind", data_all)

s1 <- df_all %>% filter(Scenario == 1 & Generation != 1)
s2 <- df_all %>% filter(Scenario == 2 & Generation != 1)
s3 <- df_all %>% filter(Scenario == 3 & Generation != 1)

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

s1$Mother_Cultural[69]
s1$Father_Cultural[69]
s1$Max_Cult_P[69]


s1$Max_Parents
head(s3)


# Per Cohort --------------------------------------------------------------


# Test for Max_Parents ----------------------------------------------------
"it does not seem to be a good idea to use Max_Parents when
we use the categorical education variable. Coefficients for Max_Parents = NA
because e.g., maximum education levels within a cohort are mostly same"

test = s1 %>% filter(Replication == 1)
unique(test$Cohort)

test2 = test %>% filter(Cohort == 3)


mod1= lm(test2$Edu_level~test2$Mother_Edu*test2$Father_Edu)
summary(mod1) # NA 
coef_m1 <- data.frame(t(data.frame(mod1$coefficients)))
extra <- data.frame(coef(summary(mod1)))
colnames(extra) <- c("Estimate", "SE", "t_stat", "p_value")
extra.t <- data.frame(Intercept_SE = extra$SE[1], Mother_SE = extra$SE[2], Father_SE = extra$SE[3], Interaction_SE = extra$SE[4],
                            Intercept_pr = extra$p_value[1], Mother_pr =extra$p_value[2], Father_pr = extra$p_value[3], Interaction_pr = extra$p_value[4])
cbind(coef_m1, extra.t)
unique(test2$Max_Parents) # Only 4





# Regression Coefficients per Cohort + Rep -------------------------------------

calculate_coefficients <- function(input_data) {
  output_df <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(output_df) <- c("Intercept", "Mother_Edu", "Father_Edu", "Interaction", 
                           "Replication", "Cohort")
  
  for (i in 1:10){
    df = input_data %>% filter(Replication == i)
    for (j in unique(df$Cohort)){
      df2 = df %>% filter(Cohort == j)
      lm = lm(df2$Edu_level~df2$Mother_Edu*df2$Father_Edu) # 2 main effects & 1 interaction effect
      coef_df <- data.frame(t(data.frame(lm$coefficients)))  # Transpose the coefficients dataframe
      extra <- data.frame(coef(summary(lm)))
      colnames(extra) <- c("Estimate", "SE", "t_stat", "p_value")
      extra.t <- data.frame(Intercept_SE = extra$SE[1], Mother_SE = extra$SE[2], Father_SE = extra$SE[3], Interaction_SE = extra$SE[4],
                            Intercept_pr = extra$p_value[1], Mother_pr =extra$p_value[2], Father_pr = extra$p_value[3], Interaction_pr = extra$p_value[4])
      colnames(coef_df) <- c("Intercept", "Mother_Edu", "Father_Edu", "Interaction")
      coef_df$Replication <- i  
      coef_df$Cohort <- j  
      binding <- cbind(coef_df, extra.t)
      output_df <- rbind(output_df, binding)
      # output_df <- rbind(output_df, coef_df)
      
    }
  }
  
  # sample <- output_df %>% filter(Cohort <= 55)
  # tb <- sample %>% group_by(Cohort) %>% summarise(
  #   Intercept = mean(X.Intercept.),
  #   Mother = mean(df2.Mother_Edu),
  #   Father = mean(df2.Father_Edu),
  #   Int_var = var(X.Intercept.),
  #   Mother_var = var(df2.Mother_Edu),
  #   Father_var = var(df2.Father_Edu)
  # )
  return(output_df)
}




# Use the function with different data frames
result_s1 <- calculate_coefficients(s1)
result_s2 <- calculate_coefficients(s2)
result_s3 <- calculate_coefficients(s3)
head(result_s1)


result_s1$Scenario = 1
result_s2$Scenario = 2
result_s3$Scenario = 3
df <- rbind(result_s1, result_s2, result_s3)
head(df)

# Create the plots
ggplot(df, aes(x = Cohort)) +
  geom_smooth(aes(y = Mother_Edu), method = "loess", color = "blue") +
  geom_point(aes(y = Mother_Edu)) +
  facet_wrap(~ Scenario, nrow = 3) +
  labs(x = "Cohort", y = "Mother") +
  theme_minimal()

ggplot(df, aes(x = Cohort)) +
  geom_smooth(aes(y = Father_Edu), method ="loess", color = "red") +
  geom_point(aes(y = Father_Edu)) +
  facet_wrap(~ Scenario, nrow = 3) +
  labs(x = "Cohort", y = "Father") +
  theme_minimal()

ggplot(df, aes(x = Cohort)) +
  geom_smooth(aes(y = Interaction), method = "loess", color = "green") +
  geom_point(aes(y = Interaction)) +
  facet_wrap(~ Scenario, nrow = 3) +
  labs(x = "Cohort", y = "Mother*Father") +
  theme_minimal()


# Ranking per Cohort + Rep -----------------------------------------------------------------


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
head(result2_s1)

plot(result2_s1$Mean_Perc ~ result_s1$Cohort) 
plot(result2_s2$Mean_Perc ~ result_s2$Cohort) 
plot(result2_s3$Mean_Perc ~ result_s3$Cohort) 

min(result2_s1$Mean_Perc, na.rm = T)
min(result2_s2$Mean_Perc, na.rm = T)
min(result2_s3$Mean_Perc, na.rm = T)

median(result2_s1$Mean_Perc, na.rm = T)
median(result2_s2$Mean_Perc, na.rm = T)
median(result2_s3$Mean_Perc, na.rm = T)



# Combine the data into one dataframe
# df2 <- rbind(
#   data.frame(Scenario = 1, Cohort = result2_s1$Cohort, Mean_Perc = result2_s1$Mean_Perc, VM_Rank = result2_s1$VM_Rank, Var_Total = result2_s1$Var_Total),
#   data.frame(Scenario = 2, Cohort = result2_s2$Cohort, Mean_Perc = result2_s2$Mean_Perc, VM_Rank = result2_s2$VM_Rank, Var_Total = result2_s2$Var_Total),
#   data.frame(Scenario = 3, Cohort = result2_s3$Cohort, Mean_Perc = result2_s3$Mean_Perc, VM_Rank = result2_s3$VM_Rank, Var_Total = result2_s3$Var_Total)
# )

df2 <- rbind(
  data.frame(Scenario = 1, Cohort = result2_s1$Cohort, Mean_Rank = result2_s1$mean.rk., Var_Rank = result2_s1$var.rk., Total = result2_s1$n, Perc = result2_s1$mean.rk./result2_s1$n),
  data.frame(Scenario = 2, Cohort = result2_s2$Cohort, Mean_Rank = result2_s2$mean.rk., Var_Rank = result2_s2$var.rk., Total = result2_s2$n, Perc = result2_s2$mean.rk./result2_s2$n),
  data.frame(Scenario = 3, Cohort = result2_s3$Cohort, Mean_Rank = result2_s3$mean.rk., Var_Rank = result2_s3$var.rk., Total = result2_s3$n, Perc = result2_s3$mean.rk./result2_s3$n)
)

# Create the plots
ggplot(df2, aes(x = Cohort)) +
  geom_smooth(aes(y = Perc), method = "lm", color = "blue", se = TRUE) +
  geom_point(aes(y = Perc)) +
  facet_wrap(~ Scenario, nrow = 3) +
  labs(x = "Cohort", y = "Percentile of the child rankings") +
  theme_minimal()
