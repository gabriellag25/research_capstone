# time to explore and visualize data


# setting working directory
setwd("C:/Users/gabri/Documents/research_capstone")

# cleaning environment
rm(list = ls())

# loading packages
library(tidyverse)
library(ggplot2)
library(patchwork)

# loading in the df_bills
df_bills <- read.csv('df_bills.csv')

# dropping all bills without a floor sponsor
df_bills <- df_bills %>% filter(floorspon1 == 1) # dropped to 6730

# slimming df to only necessary variables
df_slim <- df_bills %>% select(-substitute_sponsors,
                               -subjects_changed,
                               -committees,
                               -money_appropriated,
                               -final_chamber,
                               -session_id,
                               -subject_group,
                               -party,
                               -co_sponsors,
                               -floor_sponsor,
                               -sponsor,
                               -sponsor_id,
                               -last_action,
                               -last_action_owner,
                               -num_substitutes,
                               -floorspon1)




# visualizing subjects and bills passed (I'll get data viz later)

subjs = c('econ', 'gov_ops',
          'educ','env_nr','cj_civ',
          'other','no_subject')

plt_econ = df_slim


# logit regression

model_1 = glm(passed ~ cospon_num + econ + gov_ops + educ + cj_civ + 
                health + env_nr + money_sum_real + party1 + 
                committee1,
              family = binomial(link = "logit"), 
              data = df_slim)
summary(model_1)


model_1.5 = glm(passed ~ cospon1 + money1 + committee1 + party1,
                family = binomial(link='logit'),
                data = df_slim)
summary(model_1.5)

# limit to 2024 due to missing unemp data
df_2024 = df_slim %>% filter(session_year <= 2024)
model_2 = glm(passed ~ cospon_num + econ + gov_ops + educ + cj_civ +
                health + env_nr + money_sum_real + party1 + committee1 +
                unemp_rate + log(gfund_real),
              family = binomial(link = 'logit'),
              data = df_2024)
summary(model_2)
