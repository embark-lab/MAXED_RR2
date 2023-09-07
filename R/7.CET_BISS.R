
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

BISS_avgplot <- ggplot(biss_avg, aes(x = cet_total_weighted_sum, y = biss_avgchange, color = group_factor, group = interaction(group_factor, day), linetype = day)) +
  geom_smooth(method = 'lm', aes(fill = group_factor), size = 2.5, alpha = 0.2) + 
  scale_color_manual(name = 'Group', values = custom_colors) + 
  scale_fill_manual(name = 'Group', values = custom_colors) +
  labs(x = 'CET Total', 
       y = 'BISS Average Change',
       title = 'BISS (avg) change by CET Total')+
  theme_1 + 
  scale_linetype_manual(values = custom_linetypes, 
                        guide = guide_legend(override.aes = list(color = "black"))) +
  xlim(5,22)

control_biss_df <- biss_avg |> filter(group_factor == 'Control')

m <- glm(data = control_biss_df |> filter(day == "Prescribed"), biss_avgchange ~ cet_total_weighted_sum)
c_p <- round (m$coefficients[["cet_total_weighted_sum"]], 3)

m <- glm(data = control_biss_df |> filter(day == "Self-Paced"), biss_avgchange ~ cet_total_weighted_sum)
c_sp <- round (m$coefficients[["cet_total_weighted_sum"]], 3)

ed_biss_df <- biss_avg |> filter(group_factor == 'ED')

m <- glm(data = ed_biss_df |> filter(day == "Prescribed"), biss_avgchange ~ cet_total_weighted_sum)
ed_p <- round (m$coefficients[["cet_total_weighted_sum"]], 3)

m <- glm(data = ed_biss_df |> filter(day == "Self-Paced"), biss_avgchange ~ cet_total_weighted_sum)
ed_sp <- round (m$coefficients[["cet_total_weighted_sum"]], 3)


coefs <- data.frame(
  group_factor = c('Control', 'Control', 'ED', 'ED'),
  day = c("Prescribed", "Self-Paced", "Prescribed", "Self-Paced"),
  coef = c(c_p, c_sp, ed_p, ed_sp),
  x_pos = c(7,7,18,18), 
  y_pos = c(0.5, 0.2, 0.1, 0.6))


BISS_avgplot <- BISS_avgplot +
  geom_text(data = coefs, aes(x = x_pos, y = y_pos,
                              label = paste("b = ", coef),
                              fontface = 'bold.italic'), 
            size = 5
  )  +
  labs(linetype = 'Ex Condition')



ggsave(BISS_avgplot, file = 'figs/5.CET/BISS_avgplot.png')


# BISS WtControl Self-Paced

BISS_wtplot <- ggplot(biss_wt, aes(x = cet_wtcontrol_subscale, y = biss_wtchange, color = group_factor, group = interaction(group_factor, day), linetype = day)) +
  geom_smooth(method = 'lm', aes(fill = group_factor), size = 2.5, alpha = 0.2) + 
  scale_color_manual(name = 'Group', values = custom_colors) + 
  scale_fill_manual(name = 'Group', values = custom_colors) +
  labs(x = 'CET Weight Control Subscale', 
       y = 'BISS Weight Change',
       title = 'BISS Weight Change by \n CET Weight Control Subscale')+
  theme_1 + 
  scale_linetype_manual(values = custom_linetypes, 
                        guide = guide_legend(override.aes = list(color = "black"))) +
  labs(linetype = 'Ex Condition')


control_bisswt_df <- biss_wt |> filter(group_factor == 'Control')

m <- glm(data = control_bisswt_df |> filter(day == "Prescribed"), biss_wtchange ~ cet_total_weighted_sum)
c_p <- round (m$coefficients[["cet_total_weighted_sum"]], 3)

m <- glm(data = control_bisswt_df |> filter(day == "Self-Paced"), biss_wtchange ~ cet_total_weighted_sum)
c_sp <- round (m$coefficients[["cet_total_weighted_sum"]], 3)

ed_bisswt_df <- biss_wt |> filter(group_factor == 'ED')

m <- glm(data = ed_bisswt_df |> filter(day == "Prescribed"), biss_wtchange ~ cet_total_weighted_sum)
ed_p <- round (m$coefficients[["cet_total_weighted_sum"]], 3)

m <- glm(data = ed_bisswt_df |> filter(day == "Self-Paced"), biss_wtchange ~ cet_total_weighted_sum)
ed_sp <- round (m$coefficients[["cet_total_weighted_sum"]], 3)


coefs <- data.frame(
  group_factor = c('Control', 'Control', 'ED', 'ED'),
  day = c("Prescribed", "Self-Paced", "Prescribed", "Self-Paced"),
  coef = c(c_p, c_sp, ed_p, ed_sp),
  x_pos = c(1,1,4,4), 
  y_pos = c(-0.3, 0.2, 0.1, 0.8))


BISS_wtplot <- BISS_wtplot +
  geom_text(data = coefs, aes(x = x_pos, y = y_pos,
                              label = paste("b = ", coef),
                              fontface = 'bold.italic'), 
            size = 5
  ) 

BISS_wtplot
BISS_wtplot_nolegend <- BISS_wtplot + theme(legend.position = 'none')
BISS_avgplot_nolegend <- BISS_avgplot + theme(legend.position = 'none')

BISS_wtplot + BISS_avgplot

ggsave(BISS_wtplot, file = 'figs/5.CET/BISS_wtplot.png', bg = 'transparent')


legend<- get_legend(BISS_wtplot)

# Combine the plots without individual titles
combined_plots <-BISS_avgplot_nolegend / BISS_wtplot_nolegend +
  plot_layout(ncol = 2) 

title_plot <- ggplot() + 
  theme_void() +
  labs(title = 'Change in BISS across Exercise Sessions by CET total and Wt Control') +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold")) +
  coord_cartesian(clip = 'off') +
  theme(plot.margin = margin(t = 0, b = 0, l = 0, r = 0, "pt"))

biss_plot <- (title_plot / 
              legend/
               combined_plots) + 
  plot_layout(heights = c(0.02, 0.05, 1))



biss_plot <- biss_plot & 
  theme(panel.background = element_rect(fill = 'transparent', colour = 'transparent'),
        plot.background = element_rect(fill = 'transparent', colour = 'transparent')) 

ggsave(biss_plot, file = 'figs/5.CET/BISS_combinedplot.png', bg = 'transparent')
