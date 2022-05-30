library(haven)
library(dplyr)
library(tidyr)
library(effectsize)
library(emmeans)
library(car)
library(ggplot2)
library(ggfortify)
library(broom)

#Load data
df <- read_sav("Exp+Res+2022+Idea+3+Manipulate+&+loyalty_May+9,+2022_14.59.sav")

#Initial cleaning
df <- df %>%
  filter(Finished == 1) %>%
  filter_at(vars(StartDate:gender), all_vars(!is.na(.)))

#Add treatment variable + means loyalty and NFU
df_analyse <- df %>%
  mutate(treatment = case_when(loyalty_DO_higher == 1 ~ 1, loyalty_DO_control == 1 ~ 0)) %>%
  mutate(loyalty_mean = rowMeans(select(df, loyalty_1:loyalty_2))) %>%
  mutate(NFU_mean = rowMeans(select(df, NFU_similarity_1:NFU_similarity_4)))

# normality
shapiro.test(df_analyse$loyalty_mean)

#Effect of rank and NFU on loyalty
regression_1 <- lm(loyalty_mean ~ treatment + NFU_mean * relative_rank_1, data = df_analyse)
summary(regression_1)


#Evaluate model assumptions of the regression
autoplot(
  regression_1,
  which = 1:3,
  nrow = 1,
  ncol = 3
)


