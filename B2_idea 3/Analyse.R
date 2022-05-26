library(haven)
library(dplyr)
library(tidyr)
library(effectsize)
library(emmeans)
library(car)

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
  mutate(NFU_mean = rowMeans(select(df, NFU_similarity_2:NFU_similarity_4)))

# normality
shapiro.test(df_analyse$loyalty_mean)

#Effect of rank and NFU on loyalty
regression_1 <- lm(loyalty_mean ~ NFU_mean * relative_rank_1 + treatment, data = df_analyse) 
summary(regression_1)

#Anova treatment
anova_1 <- aov(loyalty_mean ~ NFU_mean * treatment, data = df_analyse)
summary(anova_1)


#Eta squared
eta_squared(anova_1)

#Differences in effect treatment
post_hoc <- emmeans(anova_1, pairwise ~ treatment, adjust="bonferroni")
summary(post_hoc)

