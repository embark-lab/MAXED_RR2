library(dplyr)
library(stringr)
library(sjmisc)
library(haven)
library(tidyr)
library(ggplot2)
library(patchwork)
library(haven)

load('data/RedCap/redcap_raw_enrolled.RData')
load('data/Survey_Data/MAXED_redcap_long.2023-08-01.RData')
load('data/RedCap/redcap_raw.RData')
redcap_raw <- data
# Identify unique ids that match the criteria
enrolled_ids <- as.list( redcap_raw %>%
  filter(redcap_event_name %in% c('questionnaires_arm_4', 'questionnaires_arm_5', 'questionnaires_arm_6')) %>%
  select(record_id) %>%
  distinct())
enrolled_ids <- enrolled_ids[[1]]

groups <- redcap_raw_enrolled %>% 
  select(record_id, participant_assignment) %>% 
  mutate(participant_assignment = zap_labels(participant_assignment) %>% as.character()) %>% 
  mutate(group = recode(participant_assignment, 
                        "1"="0", 
                        "2"="1", 
                        "3"="1",
                        "4"="1")) %>% 
  mutate(group_factor = factor(group, levels = c("0", "1"), labels = c('Control', "ED"))) |> 
  filter(!is.na(participant_assignment)) |> 
  rename (id = 'record_id')

groups$id = as.character((groups$id))

ex_data <-  redcap_raw_enrolled |> 
  select(record_id, starts_with('ex_hr'), starts_with('ex_hr_0'), starts_with('ex_watts'), starts_with('ex_dis'), studya_age, studya_max_hr) |> 
  rename(id = 'record_id')

ex_data$id = as.character(ex_data$id)

cet <- MAXED_redcap_long |> 
  filter(id %in% enrolled_ids,
         timepoint == 'Day_C') |> 
  select(id, cet_total_weighted_sum, cet_clinical)

# Pull out the a and b datasets
ex_data_a <- ex_data %>%
  select(id, !ends_with('_b')) %>%
  filter(rowSums(is.na(across(-id))) != (ncol(.) - 1))

names(ex_data_a)[-1] <- paste0(names(ex_data_a)[-1], "_a")

ex_data_b <- ex_data |> 
  select(id, ends_with('_b')) %>%
  filter(rowSums(is.na(select(., -id))) != (ncol(.) - 1))
ex_data <- full_join(ex_data_a, ex_data_b)


# pivot longer
# Convert labelled columns to factors
ex_numeric <- ex_data %>%
  mutate(across(-id, ~as.numeric(.)))   # Convert all except 'record_id' to numeric

ex_long <- ex_numeric %>%
  pivot_longer(
    cols = -c(id,studya_age_a, studya_max_hr_a) ,
    names_pattern = "(.*_)([0-9]+)_(.)", 
    names_to = c("variable", "time", "day"),
    values_to = "value"
  ) %>%
  mutate(variable = str_remove(variable, "_$"))  # Removes the trailing underscore

ex_data <- full_join(ex_long, cet)
ex_data <- full_join(ex_data, groups)

ex_data$time <- as.numeric(ex_data$time)

ex_data <- ex_data %>%
  mutate(
    condition = str_extract(variable, "^(ex)"),  # Extract prefix
    variable = str_replace(variable, "^(ex)_?", "")  # Remove prefix and optional underscore
  ) |> 
  mutate(variable = recode(variable, 'hr' = 'Heart Rate', 'watts' = 'Watts', 'dis' = 'Distance', 'calm' = 'Calm', 'borg' = 'Percieved Exertion'),
         day = recode(day, 'a'= 'Self-Paced', 'b' = 'Prescribed'))


custom_linetypes <- c("Self-Paced" = "dotted", "Prescribed" = "dashed")

# graph of HR over time
custom_colors <- c("#A89610", "#4d10bd")

ex_data_hr <- ex_data |>  
  filter(variable == 'Heart Rate') |> 
  mutate(`Percent Max HR` = value/studya_max_hr_a*100)

ggplot(ex_data_hr, aes(x = time, y = `Percent Max HR`, group = interaction(group_factor, day), color = group_factor, linetype = day)) +
  geom_smooth(aes(fill = group_factor), size = 2, alpha = 0.2) +
  labs(x = "Time (mins)",
       linetype = 'Ex Condition',
       color = 'Group',
  title = 'Heart Rate Over Exercise Session', hjust = 0.5) +
  theme_minimal() +  
  scale_linetype_manual(values = custom_linetypes, 
                        guide = guide_legend(override.aes = list(color = "black"))) +
  theme(text = element_text(size = 16), 
        axis.title  = element_text(size = 12), 
        strip.background = element_rect(fill = 'grey'),
        strip.text = element_text(color = 'white', face = 'bold'),
        plot.title = element_text(hjust = 0.5)
        )+
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors)  # Match fill color to group_factor

# Distance

ggplot(ex_data |> filter(variable == 'Distance'), aes(x = time, y = value, group = interaction(group_factor, day), color = group_factor, linetype = day)) +
  geom_smooth(aes(fill = group_factor), size = 2, alpha = 0.2) +
  labs(x = "Time (mins)",
       y = 'Distance',
       linetype = 'Ex Condition',
       color = 'Group',
       title = 'Distance Covered During Exercise Session', hjust = 0.5) +
  theme_minimal() +  
  scale_linetype_manual(values = custom_linetypes, 
                        guide = guide_legend(override.aes = list(color = "black"))) +
  theme(text = element_text(size = 16), 
        axis.title  = element_text(size = 12), 
        strip.background = element_rect(fill = 'grey'),
        strip.text = element_text(color = 'white', face = 'bold'),
        plot.title = element_text(hjust = 0.5)
  )+
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors)  # Match fill color to group_factor


# Distance

ggplot(ex_data |> filter(variable == 'Distance'), aes(x = time, y = value, group = interaction(group_factor, day), color = group_factor, linetype = day)) +
  geom_smooth(aes(fill = group_factor), size = 2, alpha = 0.2) +
  labs(x = "Time (mins)",
       y = 'Distance',
       linetype = 'Ex Condition',
       color = 'Group',
       title = 'Distance Covered During Exercise Session', hjust = 0.5) +
  theme_minimal() +  
  scale_linetype_manual(values = custom_linetypes, 
                        guide = guide_legend(override.aes = list(color = "black"))) +
  theme(text = element_text(size = 16), 
        axis.title  = element_text(size = 12), 
        strip.background = element_rect(fill = 'grey'),
        strip.text = element_text(color = 'white', face = 'bold'),
        plot.title = element_text(hjust = 0.5)
  )+
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors)  # Match fill color to group_factor

