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

theme_1 <- 
  theme_minimal() +
  theme(
  text = element_text(size = 18, family = "Avenir"),
  axis.title = element_text(size = 18, face = "bold"),
  axis.text = element_text(size = 18),
  plot.title = element_text(hjust = 0.5, size = 18, family = "Avenir", face = "bold"),
  legend.position = "top")

load('data/Exercise_Params/Exercise_Session_Data.RData')
custom_colors <- c("ED" = "#1a4e66", "Control" = "#fc6d46")

HR_data <- ex_data |> 
  filter(variable == 'Heart Rate') |> 
  group_by(id, day) |> 
  mutate(max_pct_hr = max(value/studya_max_hr_a *100)) |> 
  filter(time > 5) |> 
  mutate(avg_pct_hr = mean(value/studya_max_hr_a *100)) |> 
  select(id, day, group, group_factor, avg_pct_hr, max_pct_hr, cet_total_weighted_sum, cet_clinical) |> 
  distinct()


#Graph - Percent Heart Rate by CET Score During Self-Paced Exercise

ggplot(HR_data |> filter (day == 'Self-Paced'), aes(x = cet_total_weighted_sum, y = avg_pct_hr, color = group_factor, fill= group_factor)) +
  geom_point() + 
  geom_smooth(method = 'lm', aes(fill = group_factor), size = 2.5, alpha = 0.2) + 
  scale_color_manual(name = 'Group', values = custom_colors) + 
  scale_fill_manual(name = 'Group', values = custom_colors) +
  labs(x = 'CET Total', 
       y = 'Average HR (% Max)',
       title = 'Average HR During Self-Paced Exercise by CET Total')+
  theme_1



# Graph - Percent Max HR by CET  clinical

ggplot(HR_data |> filter (day == 'Self-Paced'), aes(x = as_factor(cet_clinical), y = avg_pct_hr, color = group_factor, fill = group_factor, alpha = 0.2)) +
  geom_point() +
  geom_boxplot() +
scale_color_manual(name = 'Group', values = custom_colors) +
scale_fill_manual(name = "Group", values = custom_colors) +
   labs(x = 'CET Clinical Criteria Met',
       y = 'Average HR (% Max)', 
       title = 'Heart Rate During Self-Paced \n Exercise based on CET Clinical Cutoff') +
  theme_1


  
