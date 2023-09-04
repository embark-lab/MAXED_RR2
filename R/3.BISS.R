library(dplyr)
library(stringr)
library(sjmisc)
library(haven)
library(tidyr)


load('data/RedCap/redcap_raw_enrolled.RData')
pilot_ids <- c('MAXED_1001', 'MAXED_1003', 'MAXED_1010', "MAXED_1011", "MAXED_1012")

redcap_raw_enrolled <- redcap_raw_enrolled |> 
  filter(!record_id %in% pilot_ids)

biss <-  redcap_raw_enrolled |> 
  select(record_id, starts_with('ex_phys'), starts_with ('ex_shap'), starts_with('ex_wt'), starts_with('ex_attract'), starts_with('ex_looks'))


groups <- redcap_raw_enrolled %>% 
  select(record_id, participant_assignment) %>% 
  mutate(participant_assignment = zap_labels(participant_assignment) %>% as.character()) %>% 
  mutate(group = recode(participant_assignment, 
                        "1"="0", 
                        "2"="1", 
                        "3"="1",
                        "4"="1")) %>% 
  mutate(group_factor = factor(group, levels = c("0", "1"), labels = c('Control', "ED"))) |> 
  filter(!is.na(participant_assignment)) 

biss_b <- biss %>%
  select(record_id, ends_with('_b')) %>%
  filter(rowSums(is.na(select(., -record_id))) != (ncol(.) - 1))

biss_a <- biss |> 
  select(record_id, !ends_with('_b'))%>%
  filter(rowSums(is.na(select(., -record_id))) != (ncol(.) - 1))
names(biss_a)[-1] <- paste0(names(biss_a)[-1], "_a")

biss <- full_join(biss_a, biss_b)

biss_numeric <- biss %>%
  mutate(across(-record_id, ~as.numeric(.)))  # Convert all except 'record_id' to numeric


biss_long <- biss_numeric %>%
  pivot_longer(
    cols = -record_id, 
    names_pattern = "(.*_)([0-9]+)_(.)", 
    names_to = c("variable", "time", "day"),
    values_to = "value"
  ) %>%
  mutate(variable = str_remove(variable, "_$"))  # Removes the trailing underscore


# Reverse code specific variables
biss_long <- biss_long %>%
  mutate(value = case_when(
    variable %in% c("ex_attract", "ex_shap") & value == 1 ~ 9,
    variable %in% c("ex_attract", "ex_shap") & value == 2 ~ 8,
    variable %in% c("ex_attract", "ex_shap") & value == 3 ~ 7,
    variable %in% c("ex_attract", "ex_shap") & value == 4 ~ 6,
    variable %in% c("ex_attract", "ex_shap") & value == 6 ~ 4,
    variable %in% c("ex_attract", "ex_shap") & value == 7 ~ 3,
    variable %in% c("ex_attract", "ex_shap") & value == 8 ~ 2,
    variable %in% c("ex_attract", "ex_shap") & value == 9 ~ 1,
    TRUE ~ value
  ))


biss <- left_join(biss_long, groups)
biss$time <- as.numeric(as.character(biss$time))

biss <- biss %>%
  mutate(
    condition = str_extract(variable, "^(ex_)"),  # Extract prefix
    variable = str_replace(variable, "^(ex)_?", "")  # Remove prefix and optional underscore
  )  |> 
  mutate(variable = recode(variable, 'phys' = 'Appearance', 'shap' = 'Shape', 'wt' = 'Weight', 'attract' = 'Phys Attract', 'looks' = 'Looks', 'avg' = 'BISS Average'),
         day = recode(day, 'a'= 'Self-Paced', 'b' = 'Prescribed')) |> 
  select(-condition)

averages <- biss %>%
  filter(variable %in% c('Weight', 'Shape', 'Phys Attract', 'Looks', 'Appearance')) %>%
  group_by(record_id, time, day, participant_assignment, group, group_factor) %>%
  summarise(value = mean(value, na.rm = TRUE),
            participant_assignment = first(participant_assignment),
            group = first(group),
            group_factor = first(group_factor)) %>%
  mutate(variable = 'Average') %>%
  ungroup()

# Append to the original data
biss<- bind_rows(biss, averages) %>%
  arrange(record_id, time, variable)

save(biss, file = 'data/BISS/biss_data.RData')

# Define custom line types
custom_linetypes <- c("Self-Paced" = "dotted", "Prescribed" = "dashed")

custom_colors <- c("#fc6d46","#1a4e66")
biss$variable <- factor(biss$variable, levels = c("Phys Attract", "Appearance", "Looks", "Shape", 'Weight', 'Average'))

# Define custom line types

# Plot for negative emotions
biss_plot <- ggplot(biss, aes(x = time, y = value, group = interaction(group_factor, day), color = group_factor, linetype = day)) +
  geom_smooth(size = 1, aes(fill = group_factor), alpha = 0.2) +
  facet_wrap(~variable) +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors) +
  scale_linetype_manual(values = custom_linetypes, 
                        guide = guide_legend(override.aes = list(color = "black"))) +
  labs(x = 'Time', 
       y = "Body Image Valence (1-9)", 
       linetype = 'Ex Condition') +
  theme_minimal() +
  theme(text = element_text(size = 16, family = "Avenir"),
        axis.title = element_text(size = 14, family = "Avenir"),
        axis.text = element_text(size = 14, family = "Avenir"),
        strip.background = element_rect(fill = 'darkgrey'),
        strip.text = element_text(color = 'white', face = 'bold', family = "Avenir"),
        legend.position = 'top',
        legend.title = element_text(face = "bold", family = "Avenir"),
        plot.title = element_text(hjust = 0.5, family = "Avenir")
  )

caption_text <- "Note: Responses rated every 5 minutes on 9-point Likert scale from \n 1 (extremely dissatisfied/negative) to 9 (extremely satisfied/positive)"
wrapped_caption <- str_wrap(caption_text, width = 100)  

biss_plot <- biss_plot +
  plot_annotation(title = "Body Image States Scale Scores Over Time During Exercise",
                  caption = wrapped_caption,
                  theme = theme(text = element_text(size = 14, family = "Avenir")) &
                    theme(plot.title.position = "plot",   # Position of title within plot area
                          plot.title = element_text(hjust = 0.5, family = "Avenir", face = "bold"),
                          plot.caption = element_text(hjust = 0, size = 14, family = "Avenir")) &   # Left alignment of caption
 theme(panel.background = element_rect(fill = 'transparent', colour = 'transparent'),
  plot.background = element_rect(fill = 'transparent', colour = 'transparent')) )

biss_plot


ggsave(file = 'figs/3.body_image/biss_plot.png')
