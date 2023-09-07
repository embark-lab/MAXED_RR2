theme_1 <- 
  theme_minimal() +
  theme(
    text = element_text(size = 18, family = "Avenir"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 18),
    plot.title = element_text(hjust = 0.5, size = 18, family = "Avenir", face = "bold"),
    legend.position = "top"
  )

load('data/Exercise_Params/Exercise_Session_Data.RData')
custom_colors <- c("ED" = "#1a4e66", "Control" = "#fc6d46")


pilot_ids <- c('MAXED_1001', 'MAXED_1003', 'MAXED_1010', "MAXED_1011", "MAXED_1012")

ex_data <- ex_data |> 
  filter(!id %in% pilot_ids)

HR_data_1 <- ex_data |> 
  filter(variable == 'Heart Rate', 
         day == 'Self-Paced') |> 
  group_by(id, day) |> 
  mutate(max_pct_hr = max(value/studya_max_hr_a *100)) |> 
  filter(time > 5) |> 
  mutate(avg_pct_hr = mean(value/studya_max_hr_a *100)) |> 
  select(id, day, group, group_factor, avg_pct_hr, max_pct_hr) |> 
  distinct()

library(effsize)
d_hr <- cohen.d(data = HR_data_1, max_pct_hr ~ group_factor)


ggplot(HR_data_1, aes(x = factor(1),y = max_pct_hr, color = group_factor, fill = group_factor, alpha = 0.2)) +
  geom_point() +
  geom_boxplot() +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  labs(y = 'Average HR (% Max)', 
       title = 'Average HR during Self-Paced Exercise',
       x = '') +
  theme_1 + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  guides(alpha = FALSE) +
  stat_summary(
    fun.y=median,
    geom="text",
    aes(label=sprintf("%.0f", ..y..)),
    vjust=-0.5,
    size=5,
    colour = 'black',
    position = position_dodge(width = 0.75)
  ) +
# Annotating Cohen's d val
  annotate("text", x = 1, y = 85, 
           label = paste("d =", round(d_hr$estimate*-1, 2)), vjust = -1, size = 5, fontface = 'bold', family = 'Avenir') +
  ylim(40,90)
  


watts_data_1 <- ex_data |> 
  filter(variable == 'Watts', 
         day == 'Self-Paced') |> 
  distinct() |> 
  group_by(id) |> 
  mutate(watts_max = max(value, na.rm = TRUE))  |> 
  select(id, day, group, group_factor, watts_max, cet_total_weighted_sum) |> 
  distinct() |> 
  filter(!is.na(cet_total_weighted_sum)) 

library(effsize)
d_watts <- cohen.d(data = watts_data_1, watts_max ~ group_factor)


ggplot(watts_data_1, aes(x = factor(1),y = watts_max, color = group_factor, fill = group_factor, alpha = 0.2)) +
  geom_point() +
  geom_boxplot() +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  labs(y = 'Watts', 
       title = 'Maximum Effort During Self-Paced Exercise',
       x = '') +
  theme_1 + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  guides(alpha = FALSE) +
  stat_summary(
    fun.y=median,
    geom="text",
    aes(label=sprintf("%.0f", ..y..)),
    vjust=-0.5,
    size=5,
    colour = 'black',
    position = position_dodge(width = 0.75)
  ) +
  # Annotating Cohen's d val
  annotate("text", x = 1, y = 100, 
           label = paste("d =", round(d_watts$estimate*-1, 2)), vjust = -1, size = 5, fontface = 'bold', family = 'Avenir') +
  ylim(0, 120)





