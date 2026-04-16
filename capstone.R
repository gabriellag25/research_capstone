# time to explore and visualize data


# setting working directory
setwd("C:/Users/gabri/Documents/research_capstone")

# cleaning environment
rm(list = ls())

# loading packages
library(tidyverse)
library(patchwork)

# loading in the df_bills
df_bills <- read.csv('df_bills.csv')


# slimming df to only necessary variables
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

# factorizing all binary variables

df_slim <- df_slim %>% 
  mutate(across(-c(bill_id, short_title, subjects, unemp_rate,
                 real_gdp_chg, real_gdp_pct, gfund_real,
                 money_sum_real, active_version, cospon_num), as.factor))

# visualizing

glimpse(df_slim)

## subjects pass rates & distributions

df_subjs = df_slim %>% select(subject_group, cj_civ, econ, educ, env_nr, 
                              gov_ops, health, other, passed)

df_slim %>%  ggplot(
  mapping = aes(x=subject_group, y=passed, color = passed)
) +
  geom_point()+
  geom_jitter()

ggplot(
  data = df_slim,
  mapping = aes(x=subject_group, fill = passed),
) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent)


## counts of binary variables and pass rate

df_slim %>% ggplot(
  mapping = aes(x=cospon1, y=count(passed))
) +
  geom_bar(stat = 'identity')


## boxplots of numerical variables (number of cosponsors, appropriations, 
## number of subjects, unemp, and real gdp % change)


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
model_2 = glm(passed ~ cospon_num + econ + gov_ops + educ + cj_civ +
                health + env_nr + money_sum_real + party1 + committee1 +
                unemp_rate + log(gfund_real),
              family = binomial(link = 'logit'),
              data = df_slim)
summary(model_2)
