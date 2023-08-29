library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(haven)
library(ggthemes)
library(cowplot)
library(sjmisc)


load('data/RedCap/redcap_raw_enrolled.RData')
load('data/Survey_Data/MAXED_redcap_wide.2023-08-01.RData')
load('data/Exercise_Params/Exercise_Session_Data.RData')

group_colors <- c("ED" = "#1a4e66", "Control" = "#fc6d46")

mean_age <- mean(redcap_raw_enrolled$studya_age, na.rm = TRUE)
median_age <- median(redcap_raw_enrolled$studya_age, na.rm = TRUE)

# Age categories - categorize into 14-17; 18-22
library(lubridate)
age_tab <- redcap_raw_enrolled |> 
  mutate(dcf_age = as.numeric(as_date(dcf_date) - as_date(dcf_age))/364.25) |> 
  select(dcf_age) |> 
  mutate(u18 = ifelse(dcf_age < 18, 1, 0))

u18 <- age_tab |> 
  group_by(u18) |> 
  summarise(n = n()) |> 
  filter(!is.na(u18))

ggplot(u18, aes (y = n, fill = u18)) +
   geom_bar(position = position_stack())

# Race Categories Donut Chart

race_tab <- redcap_raw_enrolled |> 
  mutate(race_total = sum(dcf_race___1, dcf_race___2, dcf_race___3, dcf_race___4, dcf_race___5, dcf_race___6, dcf_race___8, dcf_race___9, dcf_race___10, dcf_race___11)) |> 
  mutate(race = case_when(race_total > 1 ~ 'Mixed Race',
                          dcf_race___2 == 1 ~ 'Black',
                          dcf_race___3 == 1 | dcf_race___6 == 1 | dcf_race___10 == 1 |dcf_race___11 == 1 ~ 'Asian/PI', 
                          dcf_race___4 == 1  ~ 'Indigenous', 
                          dcf_race___1 == 1 ~ 'White', 
                          dcf_race___9 == 1 ~ 'Middle East', 
                          dcf_race___5 == 1 ~ 'Hispanic/Latina', 
                          !is.na(dcf_race___7) ~ 'Other')) |> 
  select(race) |> 
  filter(!is.na(race))


# Compute percentages

race_tab <- sjmisc::frq(race_tab$race)[[1]] |> 
  filter(!is.na(val))

# Compute the bottom of each rectangle
race_tab$ymin <- c(0, head(race_tab$cum.prc, n=-1))

# Compute label position
race_tab$labelPosition <- (race_tab$cum.prc + race_tab$ymin) / 2

# Compute a good label
race_tab$label <- paste0(race_tab$val," - ", race_tab$frq)

race_colors <- c("#fc6d46", "#c2b824", "#ffd583", "#1a4e66","#fc6d46", "#1a4e66" )
alpha = c(1,1,1,1,0.7,0.7)
race_colors_2 <- scales::alpha(race_colors, alpha)


race_donut <- ggplot(race_tab, aes(ymax=cum.prc, ymin=ymin, xmax=3, xmin=0.5, fill=val)) +
  geom_rect() +
  geom_text(x = 5, aes(y=labelPosition, label=label, color = val), size=4) + # x here controls label position (inner / outer)
  scale_fill_manual(values = race_colors_2) +
  scale_color_manual(values = race_colors_2) +
  coord_polar(theta="y") +
  xlim(c(-1, 5.5)) +
  theme_void() +
  labs(title = 'Participant Race and Ethnicity') +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5, vjust = -8, size = 16, family = "Avenir", face = "bold"),
        text = element_text(family = "Avenir"))

race_donut

ggsave(race_donut, file = 'figs/1.demographics/race_donut.png')

# CET Barplot
CET <- ex_data |> select(id, cet_total_weighted_sum:group_factor) |> 
  distinct()

CET_plot <- ggplot(CET, aes(x = group_factor, y = cet_total_weighted_sum, color = group_factor, fill = group_factor)) +
  geom_point() + 
  geom_boxplot(alpha = 0.2) + 
  geom_hline(yintercept=15, linetype="dashed", color = "black") +
  scale_color_manual(name = 'Group', values = group_colors) +
  scale_fill_manual(name = 'Group', values = group_colors) +
  theme_minimal() + 
  # Adding median annotation using stat_summary
  stat_summary(
    fun.y=median,
    geom="text",
    aes(label=sprintf("%.2f", ..y..)),
    vjust=-0.5,
    hjust = -0.5,
    size=5,
    colour = 'black',
    face = "bold"
  ) + 
  annotate(
    geom="text", 
    x=Inf, y=15.2, 
    label="clinical cutoff", 
    vjust=-1, 
    hjust=1, 
    color="black",
    size=5
  ) + 
  labs(x = element_blank(), 
       y = element_blank(), 
       title = 'CET Total Score by Group') +
  theme(text = element_text(size = 18, family = 'Avenir'),
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        legend.position = 'right')  

CET_plot
  
ggsave(CET_plot, file = 'figs/1.demographics/CET.png')


# CET Clinical percentage

CET_clinical <- table(CET$cet_clinical, CET$group_factor)
prop.table(CET_clinical, margin = 2)

frq(ex_data$cet_total_weighted_sum)

MAXED_redcap_wide$cet_avoid_raw_Day_C
MAXED_redcap_wide$cet_mood_raw_Day_C
MAXED_redcap_wide$cet_wtcontrol_raw_Day_C

# Diagnosis Waffle

dx_1 <- table(redcap_raw_enrolled$participant_assignment.factor)
names(dx_1) <- c("Control", "Partial Remission ED", "Subthreshold ED", "Full ED")


dx_2 <- table(redcap_raw_enrolled$group_factor)

my_waffle <- function(x, rows = 5, use_glyph = 'square', glyph_size = 6,
                      title = 'Waffle chart', 
                      my_colors = c("Control" = "#fc6d46", 
                                    "Partial Remission ED" = "#1a4e66",
                                    "Subthreshold ED" = "#1a4e66",
                                    "Full ED" = "#1a4e66"),
                      my_alpha = c("Control" = 1, 
                                   "Partial Remission ED" = 0.3,
                                   "Subthreshold ED" = 0.6, 
                                   "Full ED" = 1)
) {
  len <- sum(x)
  waffles <- seq(len) - 1
  nms <- if(is.null(names(x))) seq_along(x) else names(x)
  
  df <- data.frame(xvals = waffles %/% rows,
                   yvals = 1 - (waffles %% rows),
                   fill_col = factor(rep(nms, times = x), levels = names(my_colors)))
  
  # Combine alpha with colors directly
  combined_colors <- scales::alpha(my_colors, my_alpha)
  
  ggplot(df, aes(xvals, yvals, color = fill_col)) +
    geom_text(label = fontawesome(paste('fa', use_glyph, sep = '-')), 
              family = 'fontawesome-webfont', size = glyph_size, aes(color = fill_col)) +
    coord_equal(expand = TRUE) +
    lims(x  = c(min(df$xvals)-0.5, max(df$xvals)+0.5),
         y  = c(min(df$yvals)-0.5, max(df$yvals)+0.5)) + 
    theme_void(base_size = 10) +
    labs(title = title, color = "Group") +
    theme(text = element_text(size = 18, family = "Avenir"),
          plot.title = element_text(size = 36, hjust = 0.5, family = "Avenir", face = "bold",         margin = margin(b = -10)),
          legend.position = 'bottom',
          legend.text = element_text(size = 30),
          legend.margin = margin(t = -10),   # reduce top margin of legend
          legend.key.size = unit(0.5, "lines"), # reduce the size of legend keys
          plot.margin = margin(0, 0, 0, 0),
          legend.title = element_text(size = 32, face = 'bold'),
          legend.spacing.y = unit(-1, "cm")) + # Adjust to reduce space between legend items. Negative values will bring items closer together 
    guides(color = guide_legend(ncol = 2)) +
    scale_color_manual(name = 'Group', values = combined_colors)
}
library(emojifont)
Sample_Diagnosis <- my_waffle(dx_1, rows = 3, use_glyph = "female", glyph_size = 18, 
                              title = "Sample Diagnosis")

Sample_Diagnosis
png("figs/1.demographics/Sample_Diagnosis.png", width = 800, height = 400, bg = "transparent")
print(Sample_Diagnosis)
dev.off()
detach("package:emojifont", unload = TRUE)

