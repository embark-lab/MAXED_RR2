library(dplyr)
library(stringr)
library(sjmisc)
library(haven)
library(tidyr)
library(ggplot2)
library(patchwork)
library(haven)


load('data/RedCap/redcap_raw.RData')
redcap_raw <- data

# Identify unique ids that match the criteria
enrolled_ids <- redcap_raw %>%
  filter(redcap_event_name %in% c('questionnaires_arm_4', 'questionnaires_arm_5', 'questionnaires_arm_6')) %>%
  select(record_id) %>%
  distinct()


redcap_raw_enrolled <- redcap_raw |> 
  filter(record_id %in% enrolled_ids$record_id) |> 
  filter(str_detect(record_id, '^MAXED'))

px_group_table <-frq(redcap_raw_enrolled$participant_assignment.factor)[[1]] |> 
  select(val, frq, valid.prc, cum.prc) |> 
  filter(val != 'Ineligible', 
         !is.na(val))

redcap_raw_enrolled <- redcap_raw_enrolled %>%
  mutate(study_visit = recode(redcap_event_name, 
                                  'questionnaires_arm_3' = 'Intake',
                                  'questionnaires_arm_4' = 'Day_A',
                                  'questionnaires_arm_5' = 'Day_B',
                                  'questionnaires_arm_6' = 'Day_C',
                                  'questionnaires_arm_7' = 'Parents')) |> 
  select(-c(redcap_event_name)) |> 
  select(record_id, study_visit, everything())

redcap_raw_enrolled <- redcap_raw_enrolled %>% 
  mutate(participant_assignment = zap_labels(participant_assignment) %>% as.character()) %>% 
  mutate(group = recode(participant_assignment, 
                        "1"="0", 
                        "2"="1", 
                        "3"="1",
                        "4"="1")) %>% 
  mutate(group_factor = factor(group, levels = c("0", "1"), labels = c('Control', "ED")))
  
save(redcap_raw_enrolled, file = 'data/RedCap/redcap_raw_enrolled.RData')

