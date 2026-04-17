#==Hot Girl Analysis============================================================

# setting working directory
setwd("C:/Users/gabri/Documents/research_capstone")

# cleaning environment
rm(list = ls())

# loading packages
library(tidyverse)
library(sandwich)
library(lmtest)
library(car)



#==loading and cleaning df_bills================================================
df_bills <- read.csv('df_bills.csv')


## slimming df to only necessary variables
df_slim <- df_bills %>% select(-substitute_sponsors,
                               -subjects_changed,
                               -committees,
                               -money_appropriated,
                               -final_chamber,
                               -session_id,
                               -party,
                               -co_sponsors,
                               -floor_sponsor,
                               -sponsor,
                               -sponsor_id,
                               -last_action,
                               -last_action_owner,
                               -num_substitutes)

df_slim <- df_slim %>% as_tibble()


#==running logit models=========================================================

model_1 = glm(passed ~ econ + gov_ops + educ + cj_civ + 
                    health + env_nr + log_money_real + party1 + chamber1 +
                    committee1,
              family = binomial(link = "logit"), 
              data = df_slim)
summary(model_1)



model_2 = glm(passed ~ money1 + party1 + chamber1 + committee1,
                family = binomial(link='logit'),
                data = df_slim)
summary(model_2)



#==stage models=================================================================

model_3 = glm(stage_1 ~ econ + gov_ops + educ + cj_civ + 
                health + env_nr + log_money_real + party1 + chamber1 +
                committee1,
              family = binomial(link = "logit"), 
              data = df_slim)
summary(model_3) # RMK: this is if the bill *only* got to stage 1


model_4 = glm(stage_2 ~ econ + gov_ops + educ + cj_civ + 
               health + env_nr + log_money_real + party1 + chamber1 +
               committee1,
             family = binomial(link = "logit"), 
             data = df_slim)
summary(model_4)


model_5 = glm(stage_3 ~ econ + gov_ops + educ + cj_civ + 
                health + env_nr + log_money_real + party1 + chamber1 +
                committee1,
              family = binomial(link = "logit"), 
              data = df_slim)
summary(model_5)



#==economic model (if desired)==================================================

model_6 = glm(passed ~ econ + gov_ops + educ + cj_civ +
                health + env_nr + log_money_real + party1 +
                chamber1 + committee1 + unemp_rate + log(gfund_real),
              family = binomial(link = 'logit'),
              data = df_slim)

summary(model_6)


#==checking assumptions=========================================================

## model_chars
vif(model_1)  # woah no multicolin


## model_simple
vif(model_2) # also no multicolin


## model_stage1
vif(model_3)  # also no multicolin


## model_stage2
vif(model_4)  # also no multicolin


## model_stage3
vif(model_5)  # also no multicolin


## model_econ
vif(model_6) # also no multicolin, but gfund & committee are pretty large...
