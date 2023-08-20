library(dplyr)
library(stringr)
library(sjmisc)
library(haven)
library(tidyr)
library(ggplot2)
library(patchwork)
library(haven)
library(ggthemes)
library(cowplot)

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

hr_plot <- ggplot(ex_data_hr, 
       aes(x = time, y = `Percent Max HR`, 
           group = interaction(group_factor, day), 
           color = group_factor, 
           linetype = day)) +
  geom_smooth(aes(fill = group_factor), size = 2.5, alpha = 0.2) +
  labs(x = "Time (mins)",
       y = "% Max HR",
       linetype = 'Ex Condition',
       color = 'Group',
       title = 'Heart Rate Over Exercise Session') +
  theme_minimal(base_family = "Avenir") + 
  theme(
    text = element_text(size = 18, family = "Avenir"),
    axis.title  = element_text(size = 18, family = "Avenir", face = "bold"), 
    axis.text = element_text(size = 18),
    plot.title = element_text(hjust = 0.5, size = 20, family = "Avenir", face = "bold"),
    legend.position = "top",
    strip.background = element_rect(fill = 'grey80', color = "grey50"),
    strip.text = element_text(color = 'grey20', face = 'bold', size = 14, family = "Avenir"),
    panel.grid.minor = element_line(size = 0.5, linetype = 'dashed', color = "grey90"),
    legend.title = element_text(face = "bold")
  ) +
  scale_linetype_manual(values = custom_linetypes, 
                        guide = guide_legend(override.aes = list(color = "black"))) +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors)

hr_plot

ggsave(hr_plot, file = 'figs/hr_plot.png')

# Distance

distance_plot <- ggplot(ex_data |> filter(variable == 'Distance'), 
       aes(x = time, y = value, 
           group = interaction(group_factor, day), 
           color = group_factor, 
           linetype = day)) +
  geom_smooth(aes(fill = group_factor), size = 2.5, alpha = 0.2) +
  labs(x = "Time (mins)",
       y = 'Distance',
       linetype = 'Ex Condition',
       color = 'Group',
       title = 'Distance Covered During Exercise Session') +
  theme_minimal(base_family = "Avenir") + 
  theme(
    text = element_text(size = 18, family = "Avenir"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 18),
    plot.title = element_text(hjust = 0.5, size = 20, family = "Avenir", face = "bold"),
    legend.position = "top",
    strip.background = element_rect(fill = 'grey80', color = "grey50"),
    strip.text = element_text(color = 'grey20', face = 'bold', size = 14),
    panel.grid.minor = element_line(size = 0.5, linetype = 'dashed', color = "grey90"),
    legend.title = element_text(face = "bold")
  ) +
  scale_linetype_manual(values = custom_linetypes, 
                        guide = guide_legend(override.aes = list(color = "black"))) +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors)

distance_plot

ggsave(distance_plot, file = 'figs/distance_plot.png')

# Watts

watts_plot <- ggplot(ex_data |> filter(variable == 'Watts'), 
                        aes(x = time, y = value, 
                            group = interaction(group_factor, day), 
                            color = group_factor, 
                            linetype = day)) +
  geom_smooth(aes(fill = group_factor), size = 2.5, alpha = 0.2) +
  labs(x = "Time (mins)",
       y = 'Watts',
       linetype = 'Ex Condition',
       color = 'Group',
       title = 'Bike Resistance (Watts) During Exercise Session') +
  theme_minimal(base_family = "Avenir") + 
  theme(
    text = element_text(size = 18, family = "Avenir"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 18),
    plot.title = element_text(hjust = 0.5, size = 20, family = "Avenir", face = "bold"),
    legend.position = "top",
    strip.background = element_rect(fill = 'grey80', color = "grey50"),
    strip.text = element_text(color = 'grey20', face = 'bold', size = 14),
    panel.grid.minor = element_line(size = 0.5, linetype = 'dashed', color = "grey90"),
    legend.title = element_text(face = "bold")
  ) +
  scale_linetype_manual(values = custom_linetypes, 
                        guide = guide_legend(override.aes = list(color = "black"))) +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors)

watts_plot

ggsave(watts_plot, file = 'figs/watts_plot.png')

hr_plot_nolegend <- hr_plot + theme(legend.position = 'none', axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(title = '% Max Heart Rate')
distance_plot_nolegend <- distance_plot + theme(legend.position = 'none', axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(title = 'Distance')
watts_plot_nolegend <- watts_plot + theme(legend.position = 'none', axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(title = 'Watts')


legend<- get_legend(hr_plot)

# Combine the plots without individual titles
combined_plots <- hr_plot_nolegend / distance_plot_nolegend / watts_plot_nolegend 

# Lay out the plots
ex_params_plot <- combined_plots + plot_layout(ncol = 3)

# Create an annotation for the title
plot_annotation <- plot_annotation(
  title = 'Exercise Parameters Over Exercise Sessions',
  caption = 'Time',
  theme = theme(
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    plot.caption = element_text(size = 20, hjust = 0.5, face = "bold"))
  )
  
ex_params_plot <- ex_params_plot + plot_annotation 

# Create a ggplot object for the title
title_plot <- ggplot() + 
  theme_void() +
  labs(title = 'Exercise Parameters Over Exercise Sessions') +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold")) +
  coord_cartesian(clip = 'off') +
  theme(plot.margin = margin(t = 0, b = 0, l = 0, r = 0, "pt"))

x_label_plot <- ggplot() + 
  theme_void() +
  labs(caption = 'Time') +
  theme(plot.caption = element_text(size = 20, hjust = 0.5)) +
  coord_cartesian(clip = 'off') +
  theme(plot.margin = margin(t = 0, b = 0, l = 0, r = 0, "pt"))
x_label_plot <- x_label_plot + 
  theme(
    plot.caption = element_text(face = "bold", size = 20, hjust = 0.5)
  )

# Final assembly
ex_params_plot <- (title_plot / 
                 legend / 
                 ex_params_plot / 
                 x_label_plot) + 
  plot_layout(heights = c(0.02, 0.05, 1, 0.02))


ex_params_plot

ggsave(ex_params_plot, file = 'figs/ex_params_plot.png')

> 
