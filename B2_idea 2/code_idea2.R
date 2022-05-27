library(haven)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(haven)
library(tidyr)
library(effectsize)
library(emmeans)
library(car)
library(ggplot2)
library(ggfortify)
library(broom)

#loading survey results into R
survey_df<-read_sav("Exp+Res+2022+Idea+2+-+premium+vs+private+label_May+9,+2022_15.03.sav")

#cleaning the data, Private label is 1, Premium = 0
#survey_clean<-survey_df %>% 
#  mutate(private_v_premium = replace_na(Main_Block_DO_buy_private, 0)) %>% 
#  mutate(mean_wtp = (WTP_self + WTP_others)/2)

survey_clean<-survey_df %>% 
  mutate(private_v_premium = replace_na(Main_Block_DO_buy_private, 0)) %>% 
  mutate(mean_wtp = (WTP_others - WTP_self)) %>% 
  filter(mean_wtp >= -5)

  
#keeping only relevant data
survey_tidy<-survey_clean %>% 
  select(ResponseId,
         private_v_premium,
         relative_rank_1,
         mean_wtp,
         Age,
         gender
         )

#descriptive statistics
descriptive_stat <- survey_tidy %>% 
  group_by(private_v_premium) %>%
  summarize_all(mean, na.rm = TRUE)

##Testing assumptions

#Normality
ggplot(survey_tidy, aes(mean_wtp))+geom_histogram(bins = 50)
shapiro.test(survey_tidy$mean_wtp)

#effect of X on Mediator
regr_xm<-lm(relative_rank_1 ~ private_v_premium, survey_tidy)
summary(regr_xm)

# effect of X on Mediator adding Age
regr_xm1<-lm(relative_rank_1 ~ private_v_premium + Age, survey_tidy)
summary(regr_xm1)

#effect of Mediator on Y
regr_my<-lm(mean_wtp ~ relative_rank_1, survey_tidy)
summary(regr_my)

#total effect
regr_all<-lm(mean_wtp ~ private_v_premium, survey_tidy)
summary(regr_all)


#effect of X+M+ M*X on Y
regr_total<- lm(mean_wtp ~ private_v_premium + relative_rank_1 + Age + private_v_premium*relative_rank_1 , survey_tidy)
summary(regr_total)

#effect of M*X on Y
regr_med<- lm(mean_wtp ~ private_v_premium * relative_rank_1, survey_tidy)
summary(regr_med)

#total model 
regr_full<- lm(mean_wtp ~ private_v_premium + relative_rank_1, survey_tidy)
summary(regr_full)

#Evaluate model assumptions of the regression
autoplot(
  regr_full,
  which = 1:3,
  nrow = 1,
  ncol = 3
)

#Anova treatment
anova_1 <- aov(mean_wtp ~ private_v_premium + relative_rank_1 + Age, survey_tidy)
summary(anova_1)

#Eta squared
eta_squared(anova_1)

#Differences in effect treatment
post_hoc <- emmeans(anova_1, pairwise ~ private_v_premium, adjust="bonferroni")
summary(post_hoc)
