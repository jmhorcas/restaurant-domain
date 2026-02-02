##############################
## Cargar y preparar los datos
##############################
library(tidyverse)
library(readr)

# Load CSV
data <- read_csv("answers.csv")

# Select only CVMS questions
cvms_data <- data %>%
  select(starts_with("CVMS"))

# Rename CVMS columns (short labels for plots)
colnames(cvms_data) <- c(
  "CVMS 1", "CVMS 2", "CVMS 3", "CVMS 4", "CVMS 5",
  "CVMS 6", "CVMS 7", "CVMS 8", "CVMS 9", "CVMS 10"
)

# Convert to long format
cvms_long <- cvms_data %>%
  pivot_longer(
    cols = everything(),
    names_to = "CVMS",
    values_to = "Importance"
  ) %>%
  mutate(Importance = as.numeric(Importance))

# Order data
cvms_order <- paste("CVMS", 1:10)

cvms_long <- cvms_long %>%
  mutate(
    CVMS = factor(CVMS, levels = cvms_order)
  )

############################################################
## Estadísticos descriptivos (tabla para el paper)
############################################################
cvms_stats <- cvms_long %>%
  group_by(CVMS) %>%
  summarise(
    N = sum(!is.na(Importance)),
    Mean = mean(Importance, na.rm = TRUE),
    Median = median(Importance, na.rm = TRUE),
    Q1 = quantile(Importance, 0.25, na.rm = TRUE),
    Q3 = quantile(Importance, 0.75, na.rm = TRUE)
  ) %>%
  arrange(desc(Median))

# Reorder
cvms_stats <- cvms_stats %>%
  mutate(CVMS = factor(CVMS, levels = cvms_order))

cvms_stats


############################################################
## Figura principal: Boxplot por CVMS
############################################################
ggplot(cvms_long, aes(x = Importance, y = CVMS)) +
  geom_boxplot() +
  scale_x_continuous(breaks = 1:5, limits = c(1,5)) +
  labs(
    x = "Importance (1 = Not important, 5 = Very important)",
    y = "Challenging Variability Modeling Scenarios"
  ) +
  scale_y_discrete(limits = rev(levels(cvms_long$CVMS))) +
  theme_minimal()


############################################################
## Ranking por media (barras)
############################################################
cvms_stats_plot <- cvms_stats %>%
  filter(!is.na(Mean))

ggplot(cvms_stats_plot, aes(x = CVMS, y = Mean)) +
  geom_col() +
  coord_flip() +
  coord_cartesian(ylim = c(1,5)) +
  labs(
    x = "Variability Challenge Scenario (CVMS)",
    y = "Mean importance (1–5)"
  ) +
  theme_minimal()

############################################################
## Distribución Likert apilada (opcional)
############################################################
cvms_long %>%
  filter(!is.na(Importance)) %>%
  mutate(Importance = factor(Importance, levels = 1:5)) %>%
  ggplot(aes(x = CVMS, fill = Importance)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(
    x = "CVMS",
    y = "Proportion of responses",
    fill = "Importance"
  ) +
  scale_x_discrete(limits = cvms_order) + 
  theme_minimal()
