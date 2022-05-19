library(haven)
library(tidyverse)
library(dplyr)
library(ggplot2)

#loading survey results into R
survey_df<-read_sav("Exp+Res+2022+Idea+2+-+premium+vs+private+label_May+9,+2022_15.03.sav")

#cleaning the data, Private label is 1, Premium = 0
survey_clean<-survey_df %>% 
  mutate(private_v_premium = replace_na(Main_Block_DO_buy_private, 0)) %>% 
  mutate(mean_wtp = (WTP_self + WTP_others)/2)
  
#keeping only relevant data
survey_tidy<-survey_clean %>% 
  select(ResponseId,
         private_v_premium,
         relative_rank_1,
         mean_wtp
         )
  
##regression model
#effect of X on Mediator
regr_xm<-lm(relative_rank_1 ~ private_v_premium, survey_tidy)
summary(regr_xm)

#effect of Mediator on Y
regr_my<-lm(mean_wtp ~ relative_rank_1, survey_tidy)
summary(regr_my)

#total model 
regr_full<- lm(mean_wtp ~ private_v_premium + relative_rank_1, survey_tidy)
summary(regr)

