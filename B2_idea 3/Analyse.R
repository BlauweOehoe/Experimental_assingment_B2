library(haven)
library(dplyr)
library(tidyr)

#Load data
df <- read_sav("Exp+Res+2022+Idea+3+Manipulate+&+loyalty_May+9,+2022_14.59.sav")

#Initial cleaning
df <- df %>%
  filter(Finished == 1) %>%
  filter_at(vars(StartDate:gender), all_vars(!is.na(.)))

#Add treatment variable + means loyalty and NFU
df_analyse <- df %>%
  mutate(treatment = case_when(loyalty_DO_higher == 1 ~ 1, loyalty_DO_control == 1 ~ 0)) %>%
  mutate(loyalty_mean = rowMeans(select(df_analyse, loyalty_1:loyalty_2))) %>%
  mutate(NFU_mean = rowMeans(select(df_analyse, NFU_similarity_2:NFU_similarity_4)))

#Effect of rank and NFU on loyalty
regression_1 <- lm(loyalty_mean ~ NFU_mean * relative_rank_1 + treatment, data = df_analyse) 
summary(regression_1)

#With age and gender
regression_2 <- lm(loyalty_mean ~ NFU_mean * relative_rank_1 + Age + gender, data = df_analyse)
summary(regression_2)

#with product_choice
regression_3 <- lm(loyalty_mean ~ NFU_mean * relative_rank_1 + product_choice, data = df_analyse)
summary(regression_3)

