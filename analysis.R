# Hot Girl Analysis

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

model_chars = glm(passed ~ cospon_num + econ + gov_ops + educ + cj_civ + 
                health + env_nr + log_money_real + party1 + chamber1,
              family = binomial(link = "logit"), 
              data = df_slim)
summary(model_chars)



model_simple = glm(passed ~ cospon1 + money1 + party1 + chamber1,
                family = binomial(link='logit'),
                data = df_slim)
summary(model_simple)


model_econ = glm(passed ~ cospon_num + econ + gov_ops + educ + cj_civ +
                health + env_nr + log_money_real + party1 +
                chamber1 + unemp_rate + log(gfund_real),
              family = binomial(link = 'logit'),
              data = df_slim)
summary(model_econ)

## clustered standard errors for each model
coeftest(model_chars, vcov = vcovCL(model_1, cluster = ~session_year))
coeftest(model_econ, vcov = vcovCL(model_2, cluster = ~session_year))


#==checking assumptions=========================================================

## model_chars
vif(model_chars)  # woah no multicolin


## model_simple
vif(model_simple) # also no multicolin


## model_econ
vif(model_econ)  # also no multicolin
