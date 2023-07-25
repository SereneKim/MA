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
library(Hmisc)
library(foreign)
library(ggplot2)
library(ggforce)


# Data -------------------------------------------------------------------
setwd("/Users/serenekim/Desktop/Thesis_SeorinKim/MasterThesis_Seorin_Kim/data")
folder_path = "/Users/serenekim/Desktop/Thesis_SeorinKim/MasterThesis_Seorin_Kim/data"
file_list <- list.files(path = folder_path, pattern = "^s\\d+r\\d+_child3_analysis\\.csv$", full.names = TRUE)
data_all <- lapply(file_list, read.csv)

# Combine all data frames into a single list
df_all <- reduce(data_all[1:30], rbind)

s1 <- df_all %>% filter(Scenario == 1 & Generation != 1)
s2 <- df_all %>% filter(Scenario == 2 & Generation != 1)
s3 <- df_all %>% filter(Scenario == 3 & Generation != 1)

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

# Per Cohort --------------------------------------------------------------

# Regression Coefficients per Cohort + Rep -------------------------------------

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


# Apply the function  --------------------------------------------------------------------

# Use the function with different data frames
result_s1 <- calculate_coefficients(s1)
result_s2 <- calculate_coefficients(s2)
result_s3 <- calculate_coefficients(s3)

result_s1$Scenario = 1
result_s2$Scenario = 2
result_s3$Scenario = 3
df <- rbind(result_s1, result_s2, result_s3)
df$label <- ifelse(df$Scenario == 1, "Scenario 1", ifelse(df$Scenario == 2, "Scenario 2", "Scenario 3"))

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

# png(filename = "SE_scens.png", width = 800, res = 100)
ggplot(df, mapping = aes(x = Cohort, color = label)) +
  geom_smooth(aes(y = Parents_SE), method ="loess") +
  geom_point(aes(y = Parents_SE, alpha = 0.1)) +
  facet_wrap(.~ label, ncol = 3) +
  labs(x = "Cohort", y = "Standard Error") +
  theme_light() + theme(legend.position="none", strip.text.x = element_text(size = 10, colour = "black"))+
  theme(strip.background =element_rect(fill="#e6e5e3"))+ 
  scale_color_manual(values=c('#636EFA', '#EF553B', '#00CC96'))
# dev.off()


# Zoomed Plots (Reg Coeff) ------------------------------------------------------------


png(filename = "Zcoef_scen1_child3.png", width = 800, res = 100)
ggplot(result_s1, aes(x = Cohort)) +
  geom_smooth(aes(y = Parents_Edu), method = "loess", color = "#636EFA") +
  geom_point(aes(y = Parents_Edu, alpha=0.5), color = "#636EFA") +
  labs(x = "Cohort", y = "Regression Coefficient", title = "Scenario 1") +
  facet_zoom( ylim = c(-1, 1)) +
  theme_light() + theme(legend.position="none") 
dev.off()

png(filename = "Zcoef_scen2_child3.png", width = 800, res = 100)
ggplot(result_s2, aes(x = Cohort)) +
  geom_smooth(aes(y = Parents_Edu), method = "loess", color = '#EF553B') +
  geom_point(aes(y = Parents_Edu, alpha=0.5), color ='#EF553B') +
  labs(x = "Cohort", y = "Regression Coefficient", title = "Scenario 2") +
  facet_zoom( ylim = c(-1, 1)) +
  theme_light() + theme(legend.position="none") 
dev.off()

png(filename = "Zcoef_scen3_child3.png", width = 800, res = 100)
ggplot(result_s3, aes(x = Cohort)) +
  geom_smooth(aes(y = Parents_Edu), method = "loess", color = '#00CC96') +
  geom_point(aes(y = Parents_Edu, alpha=0.5), color ='#00CC96') +
  labs(x = "Cohort", y = "Regression Coefficient", title = "Scenario 3") +
  facet_zoom( ylim = c(-1, 1)) +
  theme_light() + theme(legend.position="none") 
dev.off()

var(result_s1$Parents_Edu, na.rm = TRUE)
var(result_s2$Parents_Edu, na.rm = TRUE)
var(result_s3$Parents_Edu, na.rm = TRUE)
result_s1 %>% group_by(Cohort) %>%  summarise(var = var(Parents_Edu))
result_s2 %>% group_by(Cohort) %>%  summarise(var = var(Parents_Edu))

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

df2 <- rbind(
  data.frame(Scenario = "Scenario 1", Cohort = result2_s1$Cohort, Mean_Rank = result2_s1$mean.rk., Var_Rank = result2_s1$var.rk., Total = result2_s1$n, Perc = result2_s1$mean.rk./result2_s1$n),
  data.frame(Scenario = "Scenario 2", Cohort = result2_s2$Cohort, Mean_Rank = result2_s2$mean.rk., Var_Rank = result2_s2$var.rk., Total = result2_s2$n, Perc = result2_s2$mean.rk./result2_s2$n),
  data.frame(Scenario = "Scenario 3", Cohort = result2_s3$Cohort, Mean_Rank = result2_s3$mean.rk., Var_Rank = result2_s3$var.rk., Total = result2_s3$n, Perc = result2_s3$mean.rk./result2_s3$n)
)


# Create the plots (With Percentiles)
png(filename = "rank_scens_child3.png", width = 800, res = 100)
ggplot(df2, mapping = aes(x = Cohort, color = Scenario)) +
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


v1.2 <- result2_s1 %>% group_by(Cohort) %>%  summarise(var = var(mean.rk.))
v2.2 <- result2_s2 %>% group_by(Cohort) %>%  summarise(var = var(mean.rk.))
v3.2 <- result2_s3 %>% group_by(Cohort) %>%  summarise(var = var(mean.rk.))

