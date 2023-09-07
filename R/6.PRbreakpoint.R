library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

theme_1 <- 
  theme_minimal() +
  theme(
    text = element_text(size = 18, family = "Avenir"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 18),
    plot.title = element_text(hjust = 0.5, size = 18, family = "Avenir", face = "bold"),
    legend.position = "top")

ex_reward_files <- list.files('data/Reward_tasks/Exercise/data', '*.log', full.names = TRUE)
ids_ex <- str_extract(ex_reward_files, "MAXED_\\d{4}")

# Function to extract max press counts and the number of press count lines from a given file
extract_press_info <- function(file) {
  log_data <- readLines(file)
  
  # Filter lines that contain 'press_count: text ='
  press_count_lines <- grep("press_count: text =", log_data, value = TRUE)
  
  # If there are no press count lines, return NA for both max and number of lines
  if(length(press_count_lines) == 0) {
    return(list(max = 0, total_lines = 0))
  }
  
  # Extract the number after 'press_count: text ='
  press_counts <- gsub(".*press_count: text = '([0-9]+)'.*", "\\1", press_count_lines)
  
  # Convert to integer
  press_counts <- as.integer(press_counts)
  
  # Return the highest value and the number of lines
  return(list(max = max(press_counts), total_lines = length(press_count_lines)))
}

# Extract max press counts and number of press count lines for each file
press_info_ex <- lapply(ex_reward_files, extract_press_info)

# Convert the list to a data frame
PR_ex_df <- data.frame(
  id = ids_ex,
  Max_Reward = sapply(press_info_ex, `[[`, "max"),
  TotalPresses = sapply(press_info_ex, `[[`, "total_lines")
)


money_reward_files <- list.files('data/Reward_tasks/Money/data', '*.log', full.names = TRUE)
ids_money <- str_extract(money_reward_files, "MAXED_\\d{4}")

# Extract max press counts and number of press count lines for each file
press_info_money <- lapply(money_reward_files, extract_press_info)

# Convert the list to a data frame
PR_money_df <- data.frame(
  id = ids_money,
  Max_Reward = sapply(press_info_money, `[[`, "max"),
  TotalPresses = sapply(press_info_money, `[[`, "total_lines")
)

PR_money_df$Task <- "Money"
PR_ex_df$Task <- "Exercise"

PR_tasks <- full_join(PR_money_df, PR_ex_df)
load('data/Exercise_Params/Exercise_Session_Data.RData')

group <- ex_data |> 
  select(id, group, group_factor) |> 
  distinct()

cet <- ex_data |> 
  select(id, cet_total_weighted_sum) |> 
  distinct()


PR_tasks <- full_join(PR_tasks, group)
PR_tasks <- full_join(PR_tasks, cet)
PR_tasks <- PR_tasks |> filter(!is.na(Task)) |> 
  mutate(Max_Reward = Max_Reward+1)

PR_tasks <- PR_tasks |> 
  mutate(`Reward Total` = case_when(
    Max_Reward >= 1849 ~ 30,    
    Max_Reward >= 1649 ~ 27,
    Max_Reward >= 1449 ~ 24,
    Max_Reward >= 1249 ~ 21,
    Max_Reward >= 1049 ~ 18,
    Max_Reward >= 849 ~ 15,
    Max_Reward >= 649 ~ 12,
    Max_Reward >= 449 ~ 9,
    Max_Reward >= 249 ~ 6,
    Max_Reward >= 49 ~ 3,
    Max_Reward <  49 ~ 0, 
  ))

custom_linetypes <- c("Exercise" = "dotdash", "Money" = "longdash")
custom_colors <- c("ED" = "#1a4e66", "Control" = "#fc6d46")

PR_tasks <- PR_tasks |> 
  rename(Reward = 'Task')

PR_tasks_ex <- PR_tasks |> filter(Reward == 'Exercise')
PR_tasks_money <- PR_tasks |> filter(Reward == 'Money')

control_df_ex <- PR_tasks_ex |> filter(group_factor == 'Control') 
b1 <- glm(data = control_df, Max_Reward ~ cet_total_weighted_sum)
c1 <- round (b1$coefficients[["cet_total_weighted_sum"]], 1)

control_df_money <- PR_tasks_money |> filter(group_factor == 'Control') 
b1 <- glm(data = control_df_money, Max_Reward ~ cet_total_weighted_sum)
c2 <- round (b1$coefficients[["cet_total_weighted_sum"]], 1)

ed_df_ex <- PR_tasks_ex |> filter(group_factor == 'ED') 
b1 <- glm(data = ed_df, Max_Reward ~ cet_total_weighted_sum)
e1 <- round (b1$coefficients[["cet_total_weighted_sum"]], 1)

ex_df_money <- PR_tasks_money |> filter(group_factor == 'ED') 
b1 <- glm(data = ex_df_money, Max_Reward ~ cet_total_weighted_sum)
e2 <- round (b1$coefficients[["cet_total_weighted_sum"]], 1)

results <- data.frame(
  Group = c('Control', 'Control', 'ED', 'ED'),
  Reward = c('Exercise', 'Money', 'Exercise', 'Money'),
  Coefficient = c(c1, c2, e1, e2)
  )


library(effsize)
d_exercise <- cohen.d(data = PR_tasks_ex, Max_Reward~ group_factor)
d_money <- cohen.d(data = PR_tasks_money, Max_Reward~ group_factor)


ggplot(PR_tasks, aes(x = Reward, y = Max_Reward, color = group_factor, fill = group_factor, alpha = 0.2)) +
  geom_point() +
  geom_boxplot() +
  scale_color_manual(name = 'Group', values = custom_colors) +
  scale_fill_manual(name = "Group", values = custom_colors) +
  labs(x = 'Reward',
       y = 'Breakpoint', 
       title = 'Progressive Ratio Task Performance') +
  theme_1 + 
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
  # Annotating Cohen's d values
  annotate("text", x = 1, y = 1600, 
           label = paste("d =", round(d_exercise$estimate*-1, 2)), vjust = -1, size = 5, fontface = 'bold', family = 'Avenir') +
  annotate("text", x = 2, y = 1600, 
           label = paste("d =", round(d_money$estimate*-1, 2)), vjust = -1, size = 5, fontface = 'bold', family = 'Avenir')

ggsave(file = 'figs/6.PR_breakpoint/PR_Tasks.png')


# Compute mean positions for annotations
mean_positions <- PR_tasks %>%
  group_by(group_factor, Reward) %>%
  summarise(mean_x = min(cet_total_weighted_sum, na.rm = TRUE),
            mean_y = mean(Max_Reward, na.rm = TRUE)) |> 
  left_join(results, by = c("group_factor" = "Group", 'Reward' = 'Reward')) |> 
  mutate(mean_y = ifelse(Reward == 'Money', mean_y + 400, mean_y -500 )) |> 
mutate(mean_x = ifelse(group_factor == 'Control', mean_x+3, mean_x + 10 ))



# Plot
PR_CET <- ggplot(PR_tasks, aes(x = cet_total_weighted_sum, y = Max_Reward, color = group_factor, 
                             group = interaction(group_factor, Reward), linetype = Reward)) +
  geom_smooth(method = 'lm', aes(fill = group_factor), size = 2.5, alpha = 0.2) + 
  scale_color_manual(name = 'Group', values = custom_colors) + 
  scale_fill_manual(name = 'Group', values = custom_colors) +
  labs(x = 'Compulsive Exercise Test (CET) Total', 
       y = 'Breakpoint',
       title = 'Progressive Ratio Task Breakpoint x CET') + 
  scale_linetype_manual(values = custom_linetypes, 
                        guide = guide_legend(override.aes = list(color = "black"))) +
  geom_text(data = mean_positions, aes(x = mean_x, y = mean_y,
                                       label = paste("b = ", Coefficient)),             vjust = -1, size = 5,
            fontface = 'bold.italic') +
  theme_1

PR_CET

ggsave(PR_CET, file = 'figs/6.PR_breakpoint/PR_Tasks_CET.png')


