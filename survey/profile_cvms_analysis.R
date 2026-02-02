##############################
## Cargar y preparar los datos
##############################
library(tidyverse)
library(readr)
library(janitor)

data <- read_csv("answers.csv") %>%
  clean_names()


colnames(data)

##############################
## Preparar datos CVMS en formato largo
##############################
cvms_long <- data %>%
  select(starts_with("cvms")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "cvms",
    values_to = "importance"
  ) %>%
  mutate(
    importance = as.numeric(importance),
    cvms = factor(cvms, levels = paste0("cvms_", 1:10),
                  labels = paste("CVMS", 1:10))
  )


##############################
## Añadir variables de perfil relevantes
##############################
profile_cvms <- cvms_long %>%
  bind_cols(
    data %>%
      select(
        experience_how_many_years_of_experience_do_you_have_with_variability_modeling,
        uvl_experience_what_is_your_level_of_experience_with_the_uvl_language,
        primary_role_what_best_describes_your_primary_role
      )
  )


##############################
## Perfil × CVMS: experiencia (junior vs senior)
##############################
profile_cvms <- profile_cvms %>%
  mutate(
    experience_group = case_when(
      experience_how_many_years_of_experience_do_you_have_with_variability_modeling %in%
        c("Less than 1 year", "1–3 years") ~ "Junior",
      experience_how_many_years_of_experience_do_you_have_with_variability_modeling %in%
        c("4–6 years", "7–10 years", "More than 10 years") ~ "Senior",
      TRUE ~ NA_character_
    )
  )

##############################
## Boxplot: experiencia × CVMS
##############################
ggplot(
  profile_cvms %>% filter(!is.na(experience_group)),
  aes(x = importance, y = cvms, fill = experience_group)
) +
  geom_boxplot(position = "dodge") +
  scale_x_continuous(breaks = 1:5, limits = c(1,5)) +
  labs(
    x = "Importance",
    y = "CVMS",
    fill = "Experience level"
  ) +
  theme_minimal()


##############################
## Perfil × CVMS: UVL insiders vs users
##############################
profile_cvms <- profile_cvms %>%
  mutate(
    uvl_group = if_else(
      uvl_experience_what_is_your_level_of_experience_with_the_uvl_language == "Part of the UVL ecosystem design and/or development",
      "UVL insiders",
      "UVL users"
    )
  )

uvl_cvms_stats <- profile_cvms %>%
  group_by(cvms, uvl_group) %>%
  summarise(
    Mean = mean(importance, na.rm = TRUE),
    N = sum(!is.na(importance)),
    .groups = "drop"
  )


ggplot(uvl_cvms_stats, aes(x = cvms, y = Mean, fill = uvl_group)) +
  geom_col(position = "dodge") +
  coord_flip() +
  coord_cartesian(ylim = c(1,5)) +
  labs(
    x = "CVMS",
    y = "Mean importance",
    fill = "Participant group"
  ) +
  theme_minimal()

uvl_cvms_stats

