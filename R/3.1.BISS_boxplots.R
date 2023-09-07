library(gridExtra)
library(cowplot)
library(ggplot2)
library(grid)
library(gridSVG)

theme_1 <- 
  theme_minimal() +
  theme(
    text = element_text(size = 18, family = "Avenir"),
    strip.background = element_rect(fill = 'darkgrey'),
    strip.text = element_text(color = 'white', face = 'bold', family = "Avenir"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 18),
    plot.title = element_text(hjust = 0.5, size = 18, family = "Avenir", face = "bold"),
    legend.position = "top",
    plot.background = element_rect(fill = 'transparent', colour = "#1a4e66", size = 3)
  )


load(file = 'data/BISS/biss_data.RData')
custom_colors <- c("ED" = "#1a4e66", "Control" = "#fc6d46")


pilot_ids <- c('MAXED_1001', 'MAXED_1003', 'MAXED_1010', "MAXED_1011", "MAXED_1012")

# Filter data not in pilot_ids and where variable is not 'Percieved Exertion'
biss <- biss %>%
  filter(!record_id %in% pilot_ids)

vars <- unique(biss$variable)

# Initialize an empty list to store the data frames
df_list <- list()

for (var in vars) {
  # For each unique variable, filter, group, mutate and then store the resultant data frame in the list
  df_temp <- biss %>%
    filter(variable == var, 
           day != 'c') %>%
    group_by(record_id, day) %>%
    mutate(var_30 =  ifelse(time == 30, value, NA_real_),
           max_var = max(value, na.rm = TRUE),
           bl_var = ifelse(time == 0, value, NA_real_)) %>%
    mutate(bl_var = first(na.omit(bl_var)), 
           var_30 = first(na.omit(var_30))) %>% 
    mutate(var_change = max_var - bl_var) |> 
    mutate(var_change_30 = var_30 - bl_var) |> 
    select(record_id, group_factor, day, max_var, bl_var, var_change, var_change_30) |> 
    ungroup() %>%
    distinct()
  
  # Append the data frame to the list
  df_list[[var]] <- df_temp
}

all_data <- bind_rows(df_list, .id = "variable")
all_data$variable <- factor(all_data$variable, levels = c("Phys Attract", "Appearance", "Looks", "Shape", 'Weight', 'Average'))

annotation_df <- list()

for (var in vars) {
  d_p <- cohen.d(data = df_list[[var]] %>% filter(day == 'Prescribed'), var_change ~ group_factor)
  d_sp <- cohen.d(data = df_list[[var]] %>% filter(day == 'Self-Paced'), var_change ~ group_factor)
  
  df_temp <- data.frame(
    variable = var,
    day = c('Prescribed', 'Self-Paced'),
    d_value = c(d_p$estimate*-1, d_sp$estimate*-1)
  )
  
  annotation_df[[var]] <- df_temp
}

# Bind all the dataframes in the list into one
annotation_df <- bind_rows(annotation_df)
all_data <- left_join(all_data, annotation_df, by = c("variable", "day"))
all_data$variable <- factor(all_data$variable, levels = c("Phys Attract", "Appearance", "Looks", "Shape", 'Weight', 'Average'))
all_data <- all_data %>%
  mutate(d_color = ifelse(!is.na(d_value) & d_value > 0.5, "red", "black"))



bp2 <- ggplot(all_data, aes(x = day, y = var_change, color = group_factor, fill = group_factor, alpha = 0.2)) +
  geom_point() +
  geom_boxplot() +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  labs(y = '', x = '') +
  theme_1  +
  guides(alpha = FALSE) +
  ylim(0,6) +
  # If you still want to annotate the plots, this becomes a bit trickier with facet_wrap.
  # You might need to find another way or adjust based on the specific needs and look of the final plots.
  # For simplicity, I've removed them here.
  theme(legend.position = 'none') +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  geom_text(aes(
    label = ifelse(!is.na(d_value), paste("d =", round(d_value, 2)), NA), 
    x= day, y = 4.5),
    vjust = -1, size = 5, fontface = 'bold', family = 'Avenir', 
   inherit.aes = FALSE)# Adjust ncol to your preference

theme_no_x_text <- function() {
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
}

bp2 <- bp2 + 
  labs(title = "BISS Changes During Exercise Across Groups") +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Avenir", face = "bold", size = 16),
    panel.background = element_rect(fill = 'transparent', colour = 'transparent'),
    plot.background = element_rect(fill = 'transparent', colour = 'transparent')
  )

ggsave(bp2, file = 'figs/3.body_image/biss_change_plot_2.png')

# Initialize a list to store the plots
plot_list <- list()

for (var in vars) {
  # Compute Cohen's d for each variable
  d_p <- cohen.d(data = df_list[[var]] %>% filter(day == 'Prescribed'), var_change ~ group_factor)
  d_sp <- cohen.d(data = df_list[[var]] %>% filter(day == 'Self-Paced'), var_change ~ group_factor)
  
  # Create a plot for each variable
  bp <- ggplot(df_list[[var]], aes(x = day, y = var_change, color = group_factor, fill = group_factor, alpha = 0.2)) +
    geom_point() +
    geom_boxplot() +
    scale_color_manual(name = 'Group', values = custom_colors) +
    scale_fill_manual(name = "Group", values = custom_colors) +
    labs(
      y = '', 
      title = var, # Using the variable name as the title
      x = ''
    ) +
    theme_1  +
    guides(alpha = FALSE) +
    ylim(-3,6) +
    # Annotating Cohen's d val
    annotate("text", x = 1, y = 3, 
             label = paste("d =", round(d_p$estimate*-1, 2)), vjust = -1, size = 5, fontface = 'bold', family = 'Avenir') +
    annotate("text", x = 2, y = 3, 
             label = paste("d =", round(d_sp$estimate*-1, 2)), vjust = -1, size = 5, fontface = 'bold', family = 'Avenir') +
    theme(legend.position = 'none')
  
  # Append the plot to the list
  plot_list[[var]] <- bp
}
library(gridExtra)
library(cowplot)

bp <- ggplot(df_list$Appearance, aes(x = day, y = var_change, color = group_factor, fill = group_factor)) +
  geom_point(alpha = 0.2) +
  geom_boxplot(alpha = 0.2) +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  theme_1 

bp

# Create a combined title
title_plot <- ggdraw() + 
  draw_label("BISS Changes during Exercsie (Max vs. BL)", fontface='bold', size=18)

# Create a plot for the legend
legend_plot <- get_legend(
  ggplot(df_list$Appearance, aes(x = day, y = var_change_30, color = group_factor, fill = group_factor)) +
    geom_point(alpha = 0.2) +
    geom_boxplot(alpha = 0.2) +
    scale_color_manual(name = 'Group', values = custom_colors) +
    scale_fill_manual(name = "Group", values = custom_colors) +
    theme_1
)
library(cowplot)

plot_list[[vars[[1]]]] <- plot_list[[vars[[1]]]] +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank())
plot_list[[vars[[4]]]] <- plot_list[[vars[[4]]]] +
  theme(axis.text.x = element_blank())
plot_list[[vars[[3]]]] <- plot_list[[vars[[3]]]] +
  theme(axis.text.y = element_blank(), 
        axis.text.x = element_blank())
plot_list[[vars[[2]]]] <- plot_list[[vars[[2]]]] +
  theme(axis.text.y = element_blank())
plot_list[[vars[[6]]]] <- plot_list[[vars[[6]]]] +
  theme(axis.text.y = element_blank())

# 2x2 grid of plots
plots_grid <- plot_grid(
  plot_list[[vars[4]]], plot_list[[vars[1]]], plot_list[[vars[3]]],     plot_list[[vars[5]]], plot_list[[vars[6]]], plot_list[[vars[2]]],
  ncol = 3
)

# Now, combine the title, legend, and 2x2 grid
biss_change_plot <- plot_grid(
  title_plot, 
  legend_plot, 
  plots_grid, 
  ncol = 1,
  rel_heights = c(0.1, 0.05, 1)  # Adjust these values as needed for better alignment
)

# View the combined plot
biss_change_plot

save_plot("figs/3.body_image/biss_change_plot.png", biss_change_plot, base_height=10, base_width=15)


load(file = 'data/BISS/biss_data.RData')
custom_colors <- c("ED" = "#1a4e66", "Control" = "#fc6d46")


pilot_ids <- c('MAXED_1001', 'MAXED_1003', 'MAXED_1010', "MAXED_1011", "MAXED_1012")

# Filter data not in pilot_ids and where variable is not 'Percieved Exertion'
biss <- biss %>%
  filter(!record_id %in% pilot_ids)

vars <- unique(biss$variable)

# Initialize an empty list to store the data frames
df_list <- list()

for (var in vars) {
  # For each unique variable, filter, group, mutate and then store the resultant data frame in the list
  df_temp <- biss %>%
    filter(variable == var, 
           day != 'c') %>%
    group_by(record_id, day) %>%
    mutate(var_30 =  ifelse(time == 30, value, NA_real_),
           max_var = max(value, na.rm = TRUE),
           bl_var = ifelse(time == 0, value, NA_real_)) %>%
    mutate(bl_var = first(na.omit(bl_var)), 
           var_30 = first(na.omit(var_30))) %>% 
    mutate(var_change = max_var - bl_var) |> 
    mutate(var_change_30 = var_30 - bl_var) |> 
    select(record_id, group_factor, day, max_var, bl_var, var_change, var_change_30) |> 
    ungroup() %>%
    distinct()
  
  # Append the data frame to the list
  df_list[[var]] <- df_temp
}




# Initialize a list to store the plots
plot_list <- list()

for (var in vars) {
  # Compute Cohen's d for each variable
  d_p <- cohen.d(data = df_list[[var]] %>% filter(day == 'Prescribed'), var_change ~ group_factor)
  d_sp <- cohen.d(data = df_list[[var]] %>% filter(day == 'Self-Paced'), var_change ~ group_factor)
  
  # Create a plot for each variable
  bp <- ggplot(df_list[[var]], aes(x = day, y = var_change, color = group_factor, fill = group_factor, alpha = 0.2)) +
    geom_point() +
    geom_boxplot() +
    scale_color_manual(name = 'Group', values = custom_colors) +
    scale_fill_manual(name = "Group", values = custom_colors) +
    labs(
      y = '', 
      title = var, # Using the variable name as the title
      x = ''
    ) +
    theme_1  +
    guides(alpha = FALSE) +
    ylim(-3,6) +
    # Annotating Cohen's d val
    annotate("text", x = 1, y = 3, 
             label = paste("d =", round(d_p$estimate*-1, 2)), vjust = -1, size = 5, fontface = 'bold', family = 'Avenir') +
    annotate("text", x = 2, y = 3, 
             label = paste("d =", round(d_sp$estimate*-1, 2)), vjust = -1, size = 5, fontface = 'bold', family = 'Avenir') +
    theme(legend.position = 'none')
  
  # Append the plot to the list
  plot_list[[var]] <- bp
}




