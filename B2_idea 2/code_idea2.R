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

#cleaning the data & Removing outliers, Private label is 1, Premium = 0
survey_clean<-survey_df %>% 
  mutate(private_v_premium = replace_na(Main_Block_DO_buy_private, 0)) %>% 
  mutate(mean_wtp = (WTP_others - WTP_self)) %>% 
  filter(mean_wtp >= -1) %>% 
  filter(mean_wtp <= 1)

  
#keeping only relevant data
survey_tidy<-survey_clean %>% 
  select(ResponseId,
         private_v_premium,
         relative_rank_1,
         mean_wtp,
         Age,
         gender,
         WTP_others,
         WTP_self
         )

#descriptive statistics
descriptive_stat <- survey_tidy %>% 
  group_by(private_v_premium) %>%
  summarize_all(mean, na.rm = TRUE)

##H1
#effect of X on Mediator
regr_xm<-lm(relative_rank_1 ~ private_v_premium, survey_tidy)
summary(regr_xm)

#effect of X on Mediator adding age
regr_xma<-lm(relative_rank_1 ~ private_v_premium + Age, survey_tidy)
summary(regr_xma)

##H2
#effect of Mediator on Y
regr_my<-lm(mean_wtp ~ relative_rank_1, survey_tidy)
summary(regr_my)

#total model 
regr_full<- lm(mean_wtp ~ private_v_premium + relative_rank_1 + Age, survey_tidy)
summary(regr_full)

##Testing assumptions

#Normality
ggplot(survey_tidy, aes(mean_wtp))+geom_histogram(bins = 50)
shapiro.test(survey_tidy$mean_wtp)

#Evaluate model assumptions of the Full_regr
autoplot(
  regr_full,
  which = 1:3,
  nrow = 1,
  ncol = 3
)

#Evaluate model assumptions of the H1 x-->m,
autoplot(
  regr_xm,
  which = 1:3,
  nrow = 1,
  ncol = 3
)

#evaluate model of X on M adding age
autoplot(
  regr_xma,
  which = 1:3,
  nrow = 1,
  ncol = 3
)

#Evaluate model assumptions of the H2
autoplot(
  regr_my,
  which = 1:3,
  nrow = 1,
  ncol = 3
)


