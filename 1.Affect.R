library(dplyr)
library(stringr)
library(sjmisc)

Day_A_affect <- readxl::read_excel('data/affect_data/PAAS_A.xlsx')
load('data/RedCap/redcap_raw_enrolled.RData')

redcap_raw_enrolled |> 
  select(record_id, participant_assignment) |> 
  mutate(group = recode(participant_assignment, 
                        1=0, 
                        2=1, 
                        3-1,
                        4=1))

