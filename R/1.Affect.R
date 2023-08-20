library(dplyr)
library(stringr)
library(sjmisc)
library(haven)
library(tidyr)
library(ggplot2)
library(patchwork)
library(haven)


Day_A_affect <- readxl::read_excel('data/affect_data/PAAS_A.xlsx')

load('data/RedCap/redcap_raw_enrolled.RData')

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


affect <- redcap_raw_enrolled |> 
  select(record_id, starts_with ('rest_affect'), starts_with('ex_borg'), starts_with('ex_enth'), starts_with('ex_crum'), starts_with('ex_fatig'), starts_with('ex_calm'))

# Pull out the a,b, c datasets
affect_b <- affect %>%
  select(record_id, ends_with('_b')) %>%
  filter(rowSums(is.na(select(., -record_id))) != (ncol(.) - 1))
affect_a <- affect |> 
  select(record_id, !ends_with('_b') & !ends_with('_c'))%>%
  filter(rowSums(is.na(select(., -record_id))) != (ncol(.) - 1))
names(affect_a)[-1] <- paste0(names(affect_a)[-1], "_a")
affect_c <- affect |> 
  select(record_id, ends_with('_c')) %>%
  filter(rowSums(is.na(select(., -record_id))) != (ncol(.) - 1))
affect_ab <- full_join(affect_a, affect_b)
affect <- full_join(affect_ab,affect_c)


# Convert labelled columns to factors
affect_numeric <- affect %>%
  mutate(across(-record_id, ~as.numeric(.)))  # Convert all except 'record_id' to numeric

affect_long <- affect_numeric %>%
  pivot_longer(
    cols = -record_id, 
    names_pattern = "(.*_)([0-9]+)_(.)", 
    names_to = c("variable", "time", "day"),
    values_to = "value"
  ) %>%
  mutate(variable = str_remove(variable, "_$"))  # Removes the trailing underscore


affect<- left_join(affect_long, groups)
affect$time <- as.numeric(as.character(affect$time))

affect <- affect %>%
  mutate(
    condition = str_extract(variable, "^(rest|ex)"),  # Extract prefix
    variable = str_replace(variable, "^(rest|ex)_?", "")  # Remove prefix and optional underscore
  ) |> 
  mutate(
    variable = str_replace(variable, "^(affect)_?", "")
  ) |> 
  mutate(variable = recode(variable, 'crum' = 'Crummy', 'enth' = 'Enthusiastic', 'fatig' = 'Fatigued', 'calm' = 'Calm', 'borg' = 'Percieved Exertion'),
         condition = recode(condition, 'ex' = 'Exercise', 'rest' = 'Rest'), 
         day = recode(day, 'a'= 'Self-Paced', 'b' = 'Prescribed'))

save(affect, file = 'data/affect_data/affect_data.RData')



combine_plots <- function(plot_neg, plot_pos, title_text) {
  caption_text <- "Note: Responses rated every 5 minutes on 5-point Likert scale from \n 0 (Do Not Feel) to 4 (Feel Very Strongly)"
  wrapped_caption <- str_wrap(caption_text, width = 100)  
  
  combined_plot <- plot_neg + plot_pos + plot_layout(ncol = 1) +
    plot_annotation(title = title_text,
                    caption = wrapped_caption,
                    theme = theme(text = element_text(size = 18, family = 'Avenir')) +
                      theme(plot.title.position = "plot",
                            plot.title = element_text(hjust = 0.5),
                            plot.caption = element_text(hjust = 0, size = 14)))
  return(combined_plot)
}


affect_plot_a_df <- affect %>% 
  filter(day == 'Self-Paced' & variable != 'Percieved Exertion' & condition == 'Exercise')

negative_emotions <- c("Crummy", "Fatigued")
positive_emotions <- c("Calm", "Enthusiastic")

positive_fill <-"darkblue"  # Color for negative emotions
negative_fill <- "darkgrey"  # Color for positive emotions

custom_colors <- c("#A89610", "#4d10bd")


# Plot for negative emotions
plot_neg_a <- ggplot(affect_plot_a_df %>% filter(variable %in% negative_emotions), 
                     aes(x = time, y = value, group = group_factor, color = group_factor, fill = group_factor)) +
  geom_smooth(method = "lm", size = 2, linetype = 'dashed', alpha = 0.2) +
  facet_wrap(~variable) +
  labs(color = 'Group') +
  theme_minimal() +
  theme(text = element_text(size = 18, family = "Avenir"), 
        axis.title.x = element_blank(),  
        axis.title.y = element_blank(),  
        axis.text.x = element_blank(),   
        axis.text.y = element_text(size = 16),
        strip.background = element_rect(fill = negative_fill),
        strip.text = element_text(color = 'white', face = 'bold'),
        legend.position = 'top',
        legend.title = element_text(face = "italic")) +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors) +
  ylim(-0.5, 3)

# Plot for positive emotions
plot_pos_a <- ggplot(affect_plot_a_df %>% filter(variable %in% positive_emotions), 
                     aes(x = time, y = value, group = group_factor, color = group_factor, fill = group_factor)) +
  geom_smooth(method = "lm", size = 2, linetype = 'dashed', alpha = 0.2) +
  facet_wrap(~variable) +
  labs(x = "Time (mins)") +
  theme_minimal() +
  theme(text = element_text(size = 18, family = "Avenir"), 
        axis.title  = element_text(size = 14), 
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16),
        strip.background = element_rect(fill = positive_fill),
        strip.text = element_text(color = 'white', face = 'bold'),
        legend.position = 'none') +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors) +
  ylim(-0.5,3)


# Combine the plots with patchwork

# Wrap the caption
caption_text <- "Note: Responses rated every 5 minutes on 5-point Likert scale from \n 0 (Do Not Feel) to 4 (Feel Very Strongly)"
wrapped_caption_a <- str_wrap(caption_text, width = 100)  

affect_plot_a <- combine_plots(plot_neg_a, plot_pos_a, "Affect Over Time During Self-Paced Exercise")

print(affect_plot_a)

ggsave(file = 'figs/Affect_DayA.png')



# Styling adjustments
# Plot for negative emotions
plot_neg_b <- ggplot(affect_plot_b_df %>% filter(variable %in% negative_emotions), 
                     aes(x = time, y = value, group = group_factor, color = group_factor, fill = group_factor)) +
  geom_smooth(method = "lm", size = 2, linetype = 'dashed', alpha = 0.2) +
  facet_wrap(~variable) +
  labs(color = 'Group') +
  theme_minimal() +
  theme(text = element_text(size = 18, family = "Avenir"), 
        axis.title.x = element_blank(),  
        axis.title.y = element_blank(),  
        axis.text.x = element_blank(),   
        axis.text.y = element_text(size = 16),
        strip.background = element_rect(fill = negative_fill),
        strip.text = element_text(color = 'white', face = 'bold'),
        legend.position = 'top',
        legend.title = element_text(face = "bold")) +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors) +
  ylim(-0.5, 3)

# Plot for positive emotions
plot_pos_b <- ggplot(affect_plot_b_df %>% filter(variable %in% positive_emotions), 
                     aes(x = time, y = value, group = group_factor, color = group_factor, fill = group_factor)) +
  geom_smooth(method = "lm", size = 2, linetype = 'dashed', alpha = 0.2) +
  facet_wrap(~variable) +
  labs(x = "Time (mins)") +
  theme_minimal() +
  theme(text = element_text(size = 18, family = "Avenir"), 
        axis.title  = element_text(size = 14), 
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 16),
        strip.background = element_rect(fill = positive_fill),
        strip.text = element_text(color = 'white', face = 'bold'),
        legend.position = 'none') +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors) +
  ylim(-0.5,3)

# Combine the plots with patchwork
affect_plot_b <- combine_plots(plot_neg_b, plot_pos_b, "Affect Over Time During Prescribed Exercise")

print(affect_plot_b)

# Combine the plots with patchwork
affect_plot_b <- combine_plots(plot_neg_b, plot_pos_b, "Affect Over Time During Prescribed Exercise")

print(affect_plot_b)


ggsave(file = 'figs/Affect_DayB.png')



affect_plot_c_df <- affect %>% 
  filter(day == 'Self-Paced', variable != 'Percieved Exertion' & condition == 'Rest')

# Plot for negative emotions
plot_neg_c <- ggplot(affect_plot_c_df %>% filter(variable %in% negative_emotions), 
                     aes(x = time, y = value, group = group_factor, color = group_factor)) +
  geom_smooth(method = "lm", size = 2) +
  facet_wrap(~variable) +
  labs(color = 'Group') +
  theme_minimal() +
  theme(text = element_text(size = 18), 
        axis.title.x = element_blank(),  # Hide x-axis title
        axis.title.y = element_blank(),  # Hide y-axis title for this plot
        axis.text.x = element_blank(),   # Hide x-axis text/ticks
        strip.background = element_rect(fill = negative_fill),
        strip.text = element_text(color = 'white', face = 'bold')) +
  scale_color_manual(values = custom_colors)+
  ylim(-0.5, 3)

# Plot for positive emotions
plot_pos_c <- ggplot(affect_plot_c_df %>% filter(variable %in% positive_emotions), 
                     aes(x = time, y = value, group = group_factor, color = group_factor)) +
  geom_smooth(method = "lm", size = 2) +
  facet_wrap(~variable) +
  labs(x = "Time (mins)") +   # Set y-axis title
  theme_minimal() +
  theme(text = element_text(size = 16), 
        axis.title  = element_text(size = 12), 
        axis.title.y = element_blank(),
        strip.background = element_rect(fill = positive_fill),
        strip.text = element_text(color = 'white', face = 'bold'),
        legend.position = 'none') +
  scale_color_manual(values = custom_colors) +
  ylim(-0.5,3)

# Combine the plots with patchwork
affect_plot_c <- combine_plots(plot_neg_c, plot_pos_c, "Affect Over Time During Rest")

ggsave(affect_plot_c, file = 'figs/Rest_Affect.png')

affect_plot_ex_df <- affect %>% 
  filter(day %in% c('Prescribed', 'Self-Paced') & variable != 'Percieved Exertion' & condition == 'Exercise')


custom_linetypes <- c("Self-Paced" = "dotted", "Prescribed" = "dashed")

library(ggplot2)
library(patchwork)

# Plot for negative emotions
plot_neg_ex <- ggplot(affect_plot_ex_df %>% filter(variable %in% negative_emotions), 
                      aes(x = time, y = value, group = interaction(group_factor, day), color = group_factor, linetype = day)) +
  geom_smooth(method = "lm", aes(fill = group_factor), size = 2, alpha = 0.2) +
  facet_wrap(~variable) +
  labs(color = 'Group',
       linetype = 'Ex Condition') +
  theme_minimal() +
  scale_linetype_manual(values = custom_linetypes, 
                        guide = guide_legend(override.aes = list(color = "black"))) +
  theme(
    text = element_text(size = 18, family = "Avenir"),
    axis.title = element_blank(),
    axis.text = element_text(size = 16, family = "Avenir"),
    strip.background = element_rect(fill = negative_fill),
    strip.text = element_text(color = 'white', face = 'bold', family = "Avenir"),
    legend.position = "top",
    legend.title = element_text(face = "bold", family = "Avenir")
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors) +
  ylim(-0.5, 3)

# Plot for positive emotions
plot_pos_ex <- ggplot(affect_plot_ex_df %>% filter(variable %in% positive_emotions), 
                      aes(x = time, y = value, group = interaction(group_factor, day), color = group_factor, linetype = day)) +
  geom_smooth(method = "lm", aes(fill = group_factor), size = 2, alpha = 0.2) +
  facet_wrap(~variable) +
  labs(x = "Time (mins)",
       linetype = 'Ex Condition', 
       y = "") +   # Set y-axis title
  theme_minimal() +  
  scale_linetype_manual(values = custom_linetypes, 
                        guide = guide_legend(override.aes = list(color = "black"))) +
  theme(
    text = element_text(size = 18, family = "Avenir"),
    axis.title = element_text(size = 14, family = "Avenir"),
    axis.text = element_text(size = 16, family = "Avenir"),
    strip.background = element_rect(fill = positive_fill),
    strip.text = element_text(color = 'white', face = 'bold', family = "Avenir"),
    legend.position = 'none'
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(name = 'Group', values = custom_colors) +
  ylim(-0.5, 3)

# Combine the plots with patchwork
affect_plot_ex <- combine_plots(plot_neg_ex, plot_pos_ex, "Affect Over Time During Exercise")


(affect_plot_ex)

ggsave(affect_plot_ex, file = 'figs/affect_plot_ex.png')
