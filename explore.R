# time to explore and visualize data


# setting working directory
setwd("C:/Users/gabri/Documents/research_capstone")

# cleaning environment
rm(list = ls())

# loading packages
library(tidyverse)
library(patchwork)
library(xtable)


# loading and cleaning df_bills
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



#==subjects pass rates & distributions==========================================

df_subjs = df_slim %>% select(subject_group, cj_civ, econ, educ, env_nr, 
                              gov_ops, health, other, passed)

df_slim %>%  ggplot(
  mapping = aes(x=subject_group, y=factor(passed))
) +
  geom_point()+
  geom_jitter()

ggplot(
  data = df_slim,
  mapping = aes(x=subject_group, fill = factor(passed)),
) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent)



#==table of all subject variables counts, pass counts, and pass rates===========
subj_vars <- c("econ", "gov_ops", "educ", 
                 "cj_civ", "health", "env_nr",
                 "other")

subject_table <- bind_rows(
  lapply(subj_vars, function(var) {
    df_slim %>%
      filter(.data[[var]] == 1) %>%
      summarise(
        Variable = var,
        N_coded_1 = n(),
        N_passed = sum(passed == 1, na.rm = TRUE),
        Pass_rate = round(mean(passed == 1, na.rm = TRUE) * 100, 1)
      )
  })
)

print(subject_table)

##print for LaTeX
x_subject_table <- xtable(subject_table, label = 'subject_table')
print(x_subject_table, type='latex')



#==table of other binary variables pass rates===================================
binary_vars = c('money1', 'cospon1', 'committee1','party1', 'chamber1')

summary_table <- bind_rows(
  lapply(binary_vars, function(var) {
    df_slim %>%
      summarise(
        Variable = var,
        N_1 = sum(.data[[var]] == 1, na.rm = TRUE),
        N_passed_1 = sum(.data[[var]] == 1 & passed == 1, na.rm = TRUE),
        Pass_rate_1 = round(N_passed_1 / N_1 * 100, 1),
        N_0 = sum(.data[[var]] == 0, na.rm = TRUE),
        N_passed_0 = sum(.data[[var]] == 0 & passed == 1, na.rm = TRUE),
        Pass_rate_0 = round(N_passed_0 / N_0 * 100, 1)
      )
  })
)
print(summary_table)

##print for LaTeX
x_summary_table <- xtable(summary_table)
print(x_summary_table, type='latex')



#==Linear plot of money_real, gfund, and unemp compared to passed rate over time
pass_rate_by_year <- function(data, year_var, outcome_var) {
  data %>%
    group_by({{ year_var }}) %>%
    summarise(
      total_bills = n(),
      bills_passed = sum({{ outcome_var }} == 1, na.rm = TRUE),
      pass_rate = round(bills_passed / total_bills * 100, 1)
    )
}

unemp_year = df_slim %>% 
  group_by(session_year) %>% 
  select(session_year, unemp_rate) %>% 
  distinct()

gfund_year = df_slim %>% 
  group_by(session_year) %>% 
  select(session_year, gfund_real) %>% 
  distinct()
gfund_year$gfund_real <- gfund_year$gfund_real/1000000

df_time = pass_rate_by_year(df_slim, session_year, passed)

df_time = merge(df_time, unemp_year, by = 'session_year')
df_time = merge(df_time, gfund_year, by = 'session_year')

pass_plt <- df_time %>% 
  ggplot(aes(x=session_year, y=pass_rate))+
  geom_line(color='steelblue', linewidth = 1)+
  labs(
    y= "Pass Rate (%)",
    x = "Session Year"
  )

unemp_plt <- df_time %>% 
  ggplot(aes(x=session_year, y=unemp_rate))+
  geom_line(color='firebrick2', linewidth = 1)+
  labs(
    y= "Unemployment Rate (%)",
    x= "Session Year"
  )

gfund_plt <- df_time %>% 
  ggplot(aes(x=session_year, y=gfund_real))+
  geom_line(color='lightgreen', linewidth = 1)+
  labs(
    y= "General Fund Revenue (Millions of $)",
    x = "Session Year"
  )

big_plt = pass_plt/unemp_plt/gfund_plt +
  plot_annotation(
    title = "Graphs of Pass Rate and Economic Indicators over Time"
  )
big_plt
