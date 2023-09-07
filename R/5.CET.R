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


pilot_ids <- c('MAXED_1001', 'MAXED_1003', 'MAXED_1010', "MAXED_1011", "MAXED_1012")

ex_data <- ex_data |> 
  filter(!id %in% pilot_ids)

HR_data <- ex_data |> 
  filter(variable == 'Heart Rate', 
         day == 'Self-Paced') |> 
  group_by(id, day) |> 
  mutate(max_pct_hr = max(value/studya_max_hr_a *100)) |> 
  filter(time > 5) |> 
  mutate(avg_pct_hr = mean(value/studya_max_hr_a *100)) |> 
  select(id, day, group, group_factor, avg_pct_hr, max_pct_hr, cet_total_weighted_sum, cet_clinical) |> 
  distinct()

border_theme <- theme(plot.background = element_rect(colour = "#1a4e66", size = 3))



#Graph - Percent Heart Rate by CET Score During Self-Paced Exercise

CET_HR <- ggplot(HR_data, aes(x = cet_total_weighted_sum, y = avg_pct_hr, color = group_factor, fill= group_factor)) +
  geom_point() + 
  geom_smooth(method = 'lm', aes(fill = group_factor), size = 2.5, alpha = 0.2, linetype = 'dotted') + 
  scale_color_manual(name = 'Group', values = custom_colors) + 
  scale_fill_manual(name = 'Group', values = custom_colors) +
  labs(x = 'Compulsive Exercise Test Total Score', 
       y = 'Average HR (% Max)')+
  theme_1 + 
  xlim(5, 23) +
  ylim(40,80) + 
  border_theme

CET_HR


control_HR_df <-  HR_data |> filter(group_factor == 'Control') 
b1 <- glm(data = control_HR_df, avg_pct_hr ~ cet_total_weighted_sum)
c1 <- round (b1$coefficients[["cet_total_weighted_sum"]], 1)

ed_HR_df <- HR_data |> filter(group_factor == 'ED') 
b1 <- glm(data = ed_HR_df, avg_pct_hr ~ cet_total_weighted_sum)
e1 <- round (b1$coefficients[["cet_total_weighted_sum"]], 1)


coefs <- data.frame(
  group_factor = c('Control', 'ED'),
  coef = c(c1, e1),
  x_pos = c(7.5, 12.5), 
  y_pos = c(60, 72))


CET_HR <- CET_HR +
  geom_text(data = coefs, aes(x = x_pos, y = y_pos,
                                       label = paste("b = ", coef), 
                                       fontface = 'bold.italic')
  )

CET_HR

ggsave(CET_HR, file = 'figs/5.CET/CET_HR.png')

# Graph - Percent Max HR by CET  clinical

library(effsize)

HR_lo_CET <- HR_data |>  
  filter(cet_clinical == 0)

d_lo <- cohen.d(data = HR_lo_CET, avg_pct_hr ~ group_factor)

CET_D_HR <- ggplot(HR_data |> filter (day == 'Self-Paced', !is.na(cet_clinical)), aes(x = as_factor(cet_clinical), y = avg_pct_hr, color = group_factor, fill = group_factor, alpha = 0.2)) +
  geom_point() +
  geom_boxplot() +
scale_color_manual(name = 'Group', values = custom_colors) +
scale_fill_manual(name = "Group", values = custom_colors) +
   labs(x = 'CET Clinical Criteria Met',
       y = 'Average HR (% Max)') +
  theme_1 + 
  guides(alpha = FALSE) +
  ylim(40, 80) + 
  border_theme +
  # Annotating Cohen's d values
  annotate("text", x = 1, y = 75, 
           label = paste("d =", round(d_lo$estimate*-1, 2)), vjust = -1, size = 5, fontface = 'bold.italic', family = 'Avenir') +
  stat_summary(
    fun.y=median,
    geom="text",
    aes(label=sprintf("%.0f", ..y..)),
    vjust=-0.5,
    size=5,
    colour = 'black',
    position = position_dodge(width = 0.75)
  )

CET_D_HR

ggsave(CET_D_HR, file = 'figs/5.CET/CET_D_HR.png')

# Remove the y-axis from the second plot
CET_D_HR <- CET_D_HR + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) 

# Combine the plots


CET_HR_plot <- CET_HR + CET_D_HR

CET_HR_plot <- CET_HR_plot + 
  plot_annotation(title = 'Average Heart Rate (10-30 mins) During  Self-Paced Exercise by CET',
                  theme = theme(text = element_text(size = 14, family = 'Avenir', face = "bold")) &
                    theme(plot.title.position = "plot",
                          plot.title = element_text(hjust = 0.5))
  )
                  
CET_HR_plot
ggsave(CET_HR_plot, file = 'figs/5.CET/CET_HR_combined.png')


  
# Watts
watts_data <- ex_data |> 
  filter(variable == 'Watts') |> 
  group_by(id, day) |> 
  mutate(watts_max = max(value, na.rm = TRUE)) |>  
  select(id, day, group, group_factor, watts_max, cet_total_weighted_sum, cet_clinical) |> 
  distinct()

CET_watts <- ggplot(watts_data |> filter (day == 'Self-Paced'), aes(x = cet_total_weighted_sum, y = watts_max, color = group_factor, fill= group_factor)) +
  geom_point() + 
  geom_smooth(method = 'lm', aes(fill = group_factor), size = 2.5, alpha = 0.2, linetype = 'dotted') + 
  scale_color_manual(name = 'Group', values = custom_colors) + 
  scale_fill_manual(name = 'Group', values = custom_colors) +
  labs(x = 'CET Total', 
       y = 'Resistance (Watts)',
       title = element_blank())+
  theme_1 + 
  xlim(5,22)


control_watts_df <- watts_data |> filter(group_factor == 'Control') 
b1 <- glm(data = control_watts_df, watts_max ~ cet_total_weighted_sum)
c1 <- round (b1$coefficients[["cet_total_weighted_sum"]], 1)

ed_watts_df <- watts_data |> filter(group_factor == 'ED') 
b1 <- glm(data = ed_watts_df, watts_max ~ cet_total_weighted_sum)
e1 <- round (b1$coefficients[["cet_total_weighted_sum"]], 1)


coefs <- data.frame(
  group_factor = c('Control', 'ED'),
  coef = c(c1, e1),
  x_pos = c(7.5, 17.5), 
  y_pos = c(60, 72))


CET_watts <- CET_watts +
  geom_text(data = coefs, aes(x = x_pos, y = y_pos,
                              label = paste("b = ", coef),
                              fontface = 'bold.italic')
  ) +
  ylim(-5, 120)

CET_watts

ggsave(CET_watts, file = 'figs/5.CET/CET_Watts.png')

Watts_lo_CET <- watts_data |>  
  filter(cet_clinical == 0, 
         day == 'Self-Paced')

d_lo <- cohen.d(data = Watts_lo_CET, watts_max ~ group_factor)

CET_D_Watts <- ggplot(watts_data |> filter (day == 'Self-Paced', !is.na(cet_clinical)), aes(x = as_factor(cet_clinical), y = watts_max, color = group_factor, fill = group_factor, alpha = 0.2)) +
  geom_point() +
  geom_boxplot() +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  labs(x = 'CET Clinical Criteria Met',
       y = 'Resistance (Watts)', 
       title = element_blank()) +
  theme_1 + 
  guides(alpha = FALSE) +
  # Annotating Cohen's d values
  annotate("text", x = 1, y = 110, 
           label = paste("d =", round(d_lo$estimate*-1, 2)), vjust = -1, size = 5, fontface = 'bold.italic', family = 'Avenir') +
  stat_summary(
    fun.y=median,
    geom="text",
    aes(label=sprintf("%.0f", ..y..)),
    vjust=-0.5,
    size=5,
    colour = 'black',
    position = position_dodge(width = 0.75)
  ) +
  ylim(-5, 120)


CET_D_Watts
ggsave(CET_D_Watts, file = 'figs/5.CET/CET_D_Watts.png')


# Combine the plots

CET_D_Watts <- CET_D_Watts + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) 


CET_watts_plot <- CET_watts + CET_D_Watts

CET_watts_plot <- CET_watts_plot + 
  plot_annotation(title = 'Maximum Effort Chosen During  Self-Paced Exercise by CET',
                  theme = theme(text = element_text(size = 14, family = 'Avenir', face = "bold")) &
                    theme(plot.title.position = "plot",
                          plot.title = element_text(hjust = 0.5))
  )

CET_watts_plot
ggsave(CET_HR_plot, file = 'figs/5.CET/CET_HR_combined.png')


# BISS

load('data/BISS/biss_data.RData')

cet <- ex_data |> 
  select(id, cet_total_weighted_sum, cet_wtcontrol_subscale, cet_clinical) |> 
  distinct()

biss <- biss |> 
  rename(id = 'record_id') |> 
  mutate(id = as.character(id))

biss_wt_sp_0 <- biss |> 
  filter(time == 0, 
         variable == 'Weight', 
         day == 'Self-Paced') |> 
  rename(Wt_sp_0 = 'value') |> 
  select(id, Wt_sp_0)

biss_wt_p_0 <- biss |> 
  filter(time == 0, 
         variable == 'Weight', 
         day == 'Prescribed') |> 
  rename(Wt_p_0 = 'value') |> 
  select(id, Wt_p_0)

biss_avg_sp_0 <- biss |> 
  filter(time == 0, 
       variable == 'Average', 
       day == 'Self-Paced') |> 
  rename(avg_sp_0 = 'value') |> 
  select(id, avg_sp_0)
  
biss_avg_p_0 <- biss |> 
  filter(time == 0, 
         variable == 'Average', 
         day == 'Prescribed') |> 
    rename(avg_p_0 = 'value') |> 
    select(id, avg_p_0)


biss_2 <- full_join(biss, biss_wt_sp_0) 
biss_2 <- full_join(biss_2, biss_wt_p_0)
biss_2 <- full_join(biss_2, biss_avg_sp_0)
biss_2 <- full_join(biss_2, biss_avg_p_0)

biss_wt_sp_cet <- full_join(cet, biss_2) |> 
  filter(variable == 'Weight', day == 'Self-Paced') |> 
  group_by(id) |> 
  mutate(biss_wtchange = max(value, na.rm = TRUE) - Wt_sp_0)

biss_wt_p_cet <- full_join(cet, biss_2) |> 
  filter(variable == 'Weight', day == 'Prescribed') |> 
  group_by(id) |> 
  mutate(biss_wtchange = max(value, na.rm = TRUE) - Wt_sp_0)

biss_avg_sp_cet <- full_join(cet, biss_2) |> 
  filter(variable == 'Average', day == "Self-Paced") |> 
  group_by(id) |> 
  mutate(biss_avgchange = max(value, na.rm = TRUE) - avg_sp_0)

biss_avg_p_cet <- full_join(cet, biss_2) |> 
  filter(variable == 'Average', day == "Prescribed") |> 
  group_by(id) |> 
  mutate(biss_avgchange = max(value, na.rm = TRUE) - avg_p_0)

biss_avg <- full_join(biss_avg_sp_cet, biss_avg_p_cet)
biss_wt <- full_join(biss_wt_p_cet, biss_wt_sp_cet)


# BISS Avg Self-Paced
custom_linetypes <- c("Self-Paced" = "dotted", "Prescribed" = "dashed")

ggplot(biss_avg, aes(x = cet_total_weighted_sum, y = biss_avgchange, color = group_factor, group = interaction(group_factor, day), linetype = day)) +
  geom_smooth(method = 'lm', aes(fill = group_factor), size = 2.5, alpha = 0.2) + 
  scale_color_manual(name = 'Group', values = custom_colors) + 
  scale_fill_manual(name = 'Group', values = custom_colors) +
  labs(x = 'CET Total', 
       y = 'BISS Average Change',
       title = 'BISS Average Improvement during Exercise by CET Total')+
  theme_1 + 
  scale_linetype_manual(values = custom_linetypes, 
                        guide = guide_legend(override.aes = list(color = "black"))) +
  xlim(5,22)

ggsave(file = 'figs/5.CET/BISS_avgplot.png')

  
  # BISS WtControl Self-Paced

ggplot(biss_wt, aes(x = cet_wtcontrol_subscale, y = biss_wtchange, color = group_factor, group = interaction(group_factor, day), linetype = day)) +
    geom_smooth(method = 'lm', aes(fill = group_factor), size = 2.5, alpha = 0.2) + 
    scale_color_manual(name = 'Group', values = custom_colors) + 
    scale_fill_manual(name = 'Group', values = custom_colors) +
    labs(x = 'CET Weight Control Subscale Score', 
         y = 'BISS Weight Change',
         title = 'BISS Weight Improvement during Exercise by CET Weight Control Subscale')+
    theme_1 + 
    scale_linetype_manual(values = custom_linetypes, 
                          guide = guide_legend(override.aes = list(color = "black"))) 
  
ggsave(file = 'figs/5.CET/BISS_wtplot.png')
