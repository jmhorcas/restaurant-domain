##############################
## Cargar y preparar los datos
##############################
library(tidyverse)
library(readr)
library(janitor)
library(ggplot2)
library(dplyr)
library(patchwork)

data <- read_csv("answers.csv") %>%
  clean_names()

colnames(data)

##############################
## Rol principal (P1)
##############################
role_stats <- data %>%
  count(primary_role_what_best_describes_your_primary_role, sort = TRUE)

role_stats

ggplot(role_stats, aes(x = reorder(primary_role_what_best_describes_your_primary_role, n), y = n)) +
  # Añadimos color según la cantidad para que sea más visual
  geom_col(fill = "steelblue") +
  # Añadimos los números al final de cada barra
  geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
  coord_flip() +
  # Expandimos el eje y para que las etiquetas no se corten
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = "Primary role",
    y = "Number of participants"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10)
  )

# Gráfico de tarta
ggplot(role_stats, aes(x = "", y = n, fill = reorder(primary_role_what_best_describes_your_primary_role, n))) +
  geom_col(width = 1, color = "white") +
  # Convertir a círculo
  coord_polar("y", start = 0) +
  # Añadir el número exacto (n)
  geom_text(aes(label = n), 
            position = position_stack(vjust = 0.5), 
            color = "white", 
            size = 5) +
  labs(
    title = "Primary rol",
    fill = "Rol:"
  ) +
  theme_void() + 
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.title = element_text(hjust = 0.5, margin = margin(b = -20)),
    legend.box.margin = margin(t = -40),
    legend.spacing.x = unit(0.1, 'cm'),
    # 3. REDUCIR MÁRGENES GENERALES:
    plot.margin = margin(1, 1, 1, 1, "cm")
  )



##############################
## Experiencia en variabilidad (años) – P2
##############################
exp_stats <- data %>%
  count(experience_how_many_years_of_experience_do_you_have_with_variability_modeling, sort = TRUE)

exp_stats

ggplot(exp_stats, aes(x = experience_how_many_years_of_experience_do_you_have_with_variability_modeling, y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Experience with variability modeling (years)",
    y = "Number of participants"
  ) +
  theme_minimal()

# 1. Creamos el factor con el orden cronológico correcto
# Asegúrate de que los nombres coincidan exactamente con tus datos
data$experience_factor <- factor(
  data$experience_how_many_years_of_experience_do_you_have_with_variability_modeling, 
  levels = c("Less than 1 year", "1-3 years", "7-10 years", "More than 10 years")
)

# 1. Creamos el factor con las etiquetas cortas que me pediste
data$experience_factor <- factor(
  data$experience_how_many_years_of_experience_do_you_have_with_variability_modeling, 
  levels = c("Less than 1 year", "1-3 years", "7-10 years", "More than 10 years"),
  labels = c("1", "1-3", "7-10", ">10") # Aquí renombramos para el eje X
)

# 2. Agrupamos
exp_stats <- data %>%
  count(experience_factor) %>%
  filter(!is.na(experience_factor))

# 3. Gráfico Vertical
ggplot(exp_stats, aes(x = experience_factor, y = n)) +
  # Línea vertical
  geom_segment(aes(x = experience_factor, xend = experience_factor, y = 0, yend = n), 
               color = "grey75", size = 0.9) +
  # Punto (cabeza del lollipop)
  geom_point(color = "firebrick", size = 6) + 
  # Número exacto encima del punto
  geom_text(aes(label = n), vjust = -1.2, size = 4.5) +
  # Ajustamos el límite superior para que el número no se corte arriba
  scale_y_continuous(limits = c(0, max(exp_stats$n) + 2), labels = NULL) +
  # 'expand' controla el aire a los lados de las categorías
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
  labs(
    title = "Experience on variability modeling",
    x = "Years",
    y = NULL,
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, margin = margin(b = -25)),
    aspect.ratio = 1.5,
  )

##############################
## Experiencia con UVL (P3)
##############################
# 1. Definimos el orden lógico de menor a mayor intensidad
data$uvl_experience <- factor(
  data$uvl_experience_what_is_your_level_of_experience_with_the_uvl_language, # Cambia por el nombre real de tu columna
  levels = c(
    "None", 
    "Read-only / learning", 
    "Used in small examples", 
    "Used in research prototypes", 
    "Part of the UVL ecosystem design and/or development"
  ),
  labels = c(
    "None",                  # Para "None"
    "Read-only / Learning",             # Para "Read-only / learning"
    "Small examples",        # Para "Used in small examples"
    "Research prototypes", # Para "Used in research prototypes"
    "UVL developer"         # Para "Part of the UVL ecosystem..."
  )
)

uvl_stats <- data %>%
  count(uvl_experience) %>%
  filter(!is.na(uvl_experience))

# 2. Gráfico de barras mejorado
# 1. Gráfico Vertical
ggplot(uvl_stats, aes(x = uvl_experience, y = n)) +
  # Barras con el ancho estandarizado que venimos usando
  geom_col(fill = "#333333", width = 0.6) +
  
  # Número encima de la barra (vjust en lugar de hjust)
  geom_text(aes(label = n), 
            vjust = -0.5, 
            color = "#333333", 
            size = 4) +
  
  # Escala uniforme (N=21) para que el tamaño sea idéntico a los otros PDFs
  scale_y_continuous(limits = c(0, 21), expand = expansion(mult = c(0, 0.1)), breaks = NULL) +
  
  labs(
    title = "Participants' experience level with UVL",
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    # Rotamos las etiquetas un poco si ves que chocan, o las dejamos rectas si caben
    axis.text.x = element_text(size = 10, color = "#333333", angle = 0), 
    plot.title = element_text(hjust = 0.5, size = 12, margin = margin(b = -300)),
    # Aspect ratio para que la caja del gráfico sea igual a las otras
    aspect.ratio = 0.75
  )


##############################
## Contexto de uso (P4)
##############################
context_stats <- data %>%
  # 1. Limpiamos espacios en los extremos y separamos por coma O punto y coma
  mutate(context_clean = str_trim(usage_context_in_which_context_s_do_you_mainly_use_variability_models_select_all_that_apply)) %>%
  separate_rows(context_clean, sep = "[,;]\\s*") %>% 
  
  # 2. Contamos cada mención individual
  count(context_clean, sort = TRUE) %>%
  
  # 3. Quitamos posibles filas vacías o NAs
  filter(!is.na(context_clean) & context_clean != "")

# 4. Gráfico de barras mejorado
ggplot(context_stats, aes(x = reorder(context_clean, -n), y = n)) + # -n para orden descendente
  # Barras verticales
  geom_col(fill = "#333333", width = 0.6) +
  
  # Número encima de la barra
  geom_text(aes(label = n), 
            vjust = -0.5, 
            color = "#333333", 
            size = 4) +
  
  # Ajustamos el límite a 21 o un poco más si las menciones totales son altas
  scale_y_continuous(limits = c(0, 21), expand = expansion(mult = c(0, 0.1)), breaks = NULL) +
  
  labs(
    title = "Contexts of UVL usage (Aggregated Mentions: Multiple choices allowed)",
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    # Rotación necesaria para que los contextos no se solapen
    axis.text.x = element_text(size = 10, color = "#333333"),
    plot.title = element_text(hjust = 0.5, size = 12, margin = margin(b = -15)),
    # Aspect ratio idéntico a los anteriores
    aspect.ratio = 0.75
  )

# ggplot(context_stats, aes(x = reorder(context_clean, n), y = n)) +
#   geom_col(fill = "#333333", width = 0.7) +
#   geom_text(aes(label = n), hjust = -0.5) +
#   coord_flip() +
#   scale_y_continuous(
#     breaks = seq(0, 20, by = 5),
#     labels = NULL,
#   ) +
#   labs(
#     title = "Contexts of UVL usage (Aggregated mentions)",
#     x = NULL,
#     y = NULL,
#   ) +
#   theme_minimal() +
#   theme(
#     panel.grid.major.x = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     axis.text.y = element_text(size = 11, margin = margin(r = -25)),
#     plot.title = element_text(hjust = 0.5)
#   )


##############################
## País / región (P5)
##############################
country_stats <- data %>%
  count(your_country, sort = TRUE)

# Gráfico de barras horizontal
ggplot(country_stats, aes(x = reorder(your_country, n), y = n)) +
  # Usamos un color que resalte y barras un poco más delgadas para elegancia
  geom_col(fill = "#333333", width = 0.7) +
  # Añadimos el número exacto al final para evitar leer el eje
  geom_text(aes(label = n), hjust = -0.5) +
  coord_flip() +
  # Ajustamos el eje para números enteros y damos espacio al texto
  scale_y_continuous(
    breaks = seq(0, max(country_stats$n), by = 3),
    labels = NULL,
  ) +
  labs(
    title = "Participants by country",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 11, margin = margin(r = -25)),
    plot.title = element_text(hjust = 0.5)
  )

# Gráfico de barras vertical
ggplot(country_stats, aes(x = reorder(your_country, -n), y = n)) + # -n para que la más alta vaya primero
  geom_col(fill = "#333333", width = 0.9) +
  
  # NÚMERO ARRIBA: Cambiamos hjust por vjust para que el número flote sobre la barra
  geom_text(aes(label = n), 
            vjust = -0.5, 
            color = "#333333") +
  
  # Quitamos coord_flip() para que sea vertical por defecto
  
  # Ajustamos la escala (ahora es el eje Y el que tiene los datos)
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)), 
    labels = NULL, 
    breaks = NULL
  ) +
  
  labs(
    title = "Geographical distribution of participants",
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal() +
  theme(
    # Limpiamos rejillas
    panel.grid = element_blank(),
    
    # Estilizamos el eje X (países)
    # Si los nombres son largos, podrías necesitar angle = 45
    axis.text.x = element_text(size = 10, color = "#333333", margin = margin(t = 0)),
    
    plot.title = element_text(hjust = 0.5, size = 13, margin = margin(b = -35)),
    aspect.ratio = 1.5,
  )

##############################
## Institución (opcional, normalmente solo tabla)
##############################
institution_stats <- data %>%
  filter(!is.na(your_institution_or_organization_optional), your_institution_or_organization_optional != "") %>%
  count(your_institution_or_organization_optional, sort = TRUE)

head(institution_stats, 10)

##############################
## Uso de constructores de UVL (P7)
##############################
uvl_constructs_stats <- data %>%
  separate_rows(
    uvl_constructs_usage_which_uvl_language_constructs_do_you_typically_use_to_model_variability_select_all_that_apply,
    sep = ";"
  ) %>%
  count(
    uvl_constructs_usage_which_uvl_language_constructs_do_you_typically_use_to_model_variability_select_all_that_apply,
    sort = TRUE
  )

uvl_constructs_stats

library(stringr)

ggplot(
  uvl_constructs_stats,
  aes(
    x = reorder(
      str_wrap(
        uvl_constructs_usage_which_uvl_language_constructs_do_you_typically_use_to_model_variability_select_all_that_apply,
        width = 35
      ),
      n
    ),
    y = n
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    x = "UVL language constructs used",
    y = "Number of participants"
  ) +
  theme_minimal()

##############################
## Uso de constructores de UVL (P7) manual
##############################
manual_stats <- tibble(
category = c("Boolean level", "Feature attributes", "Arithmetic level", "Feature cardinalities", "Typed features"),
n = c(18, 17, 11, 10, 9)  # Pon aquí tus valores exactos
)

ggplot(manual_stats, aes(x = reorder(category, -n), y = n)) +
  # Barras verticales con ancho estandarizado
  geom_col(fill = "#333333", width = 0.6) +
  
  # Número encima de la barra (vjust)
  geom_text(aes(label = n), 
            vjust = -0.5, 
            color = "#333333", 
            size = 4) +
  
  # Escala uniforme (N=21)
  scale_y_continuous(
    limits = c(0, 21), 
    expand = expansion(mult = c(0, 0.1)), 
    labels = NULL, 
    breaks = NULL
  ) +
  
  labs(
    title = "UVL expressiveness usage (Aggregated Mentions: Multiple choices allowed)",
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    # Rotación a 45 grados para que los nombres largos no choquen
    axis.text.x = element_text(size = 10, color = "#333333"),
    plot.title = element_text(hjust = 0.5,  size = 12, margin = margin(b = -80)),
    # Mantener la proporción idéntica a las otras gráficas
    aspect.ratio = 0.75
  )

# 2. Generamos el gráfico
# ggplot(manual_stats, aes(x = reorder(category, n), y = n)) +
#   # Usamos el gris oscuro/negro (#333333)
#   geom_col(fill = "#333333", width = 0.7) +
#   
#   # Añadimos el número fuera de la barra
#   geom_text(aes(label = n), 
#             hjust = -0.5, 
#             color = "#333333") +
#   
#   coord_flip() +
#   
#   # Eliminamos los números del eje X (ya están en las barras)
#   scale_y_continuous(
#     expand = expansion(mult = c(0, 0.15)), 
#     labels = NULL, 
#     breaks = NULL
#   ) +
#   
#   labs(
#     title = "UVL expressiveness usage (Aggregated mentions)",
#     x = NULL,
#     y = NULL
#   ) +
#   
#   theme_minimal() +
#   theme(
#     # Limpieza total de cuadrículas
#     panel.grid = element_blank(),
#     
#     # Ajuste de las etiquetas de las categorías
#     # Si pusiste margin(r = -25) y se solapan, prueba con un valor positivo como 10
#     axis.text.y = element_text(size = 11, color = "#333333", margin = margin(r = 0)),
#     
#     # Centrado de títulos
#     plot.title = element_text(hjust = 0.5, size = 13, margin = margin(b = 0)),
#   )


