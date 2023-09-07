theme_1 <- 
  theme_minimal() +
  theme(
    text = element_text(size = 18, family = "Avenir"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 18),
    plot.title = element_text(hjust = 0.5, size = 18, family = "Avenir", face = "bold"),
    legend.position = "top",
    plot.background = element_rect(fill = 'transparent', colour = "#1a4e66", size = 3))

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


hr_plot <- ggplot(HR_data_1, aes(x = factor(1),y = max_pct_hr, color = group_factor, fill = group_factor, alpha = 0.2)) +
  geom_point() +
  geom_boxplot() +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  labs(y = 'HR (% Max)', 
       title = 'Mean HR',
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


w_plot <- ggplot(watts_data_1, aes(x = factor(1),y = watts_max, color = group_factor, fill = group_factor, alpha = 0.2)) +
  geom_point() +
  geom_boxplot() +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  labs(y = 'Watts', 
       title = 'Max Effort',
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
  annotate("text", x = 1, y = 120, 
           label = paste("d =", round(d_watts$estimate*-1, 2)), vjust = -1, size = 5, fontface = 'bold', family = 'Avenir') +
  ylim(0, 130)




distance_data_1 <- ex_data |> 
  filter(variable == 'Distance', 
         day == 'Self-Paced', 
         !is.na(value)) |> 
  distinct() |> 
  group_by(id) |> 
  mutate(distance_total = max(value, na.rm = TRUE))  |> 
  select(id, day, group, group_factor, distance_total, cet_total_weighted_sum) |>
  distinct() |> 
  filter(!is.na(cet_total_weighted_sum)) 

d_distance <- cohen.d(data = distance_data_1, distance_total ~ group_factor)


d_plot <- ggplot(distance_data_1, aes(x = factor(1),y = distance_total, color = group_factor, fill = group_factor, alpha = 0.2)) +
  geom_point() +
  geom_boxplot() +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  labs(y = 'Miles', 
       title = 'Distance',
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
  annotate("text", x = 1, y = 9, 
           label = paste("d =", round(d_distance$estimate*-1, 2)), vjust = -1, size = 5, fontface = 'bold', family = 'Avenir') +
  ylim(2, 10)

hr_plot_1 <- hr_plot + theme(legend.position = 'none', axis.title.x = element_blank()) +
  labs(title = 'Mean HR')
d_plot_1 <- d_plot + theme(legend.position = 'none', axis.title.x = element_blank()) +
  labs(title = 'Distance')
w_plot_1 <- w_plot + theme(legend.position = 'none', axis.title.x = element_blank()) +
  labs(title = 'Max Effort')

legend<- get_legend(hr_plot)


ex_plot_1 <- hr_plot_1 /d_plot_1 / w_plot_1 +
  plot_layout(ncol = 3)

ex_plot_1


# Create a ggplot object for the title
title_plot <- ggplot() + 
  theme_void() +
  labs(title = 'Exercise Parameters During Self-Paced Exercise') +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold")) +
  coord_cartesian(clip = 'off') +
  theme(plot.margin = margin(t = 0, b = 0, l = 0, r = 0, "pt"))



# Final assembly
ex_plot_1 <- (title_plot / 
                     legend / 
                     ex_plot_1) + 
  plot_layout(heights = c(0.02, 0.05, 1))

ex_plot_1

ex_plot_1 <- ex_plot_1 &   # Left alignment of caption
  theme(panel.background = element_rect(fill = 'transparent', colour = 'transparent'),
        plot.background = element_rect(fill = 'transparent', colour = 'transparent')) 

ggsave(ex_plot_1, file = 'figs/4.ex_params/ex_params_boxplots.png')

