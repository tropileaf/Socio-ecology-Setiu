library(nnet)
library(logistf)
library(brglm2)
library(ggeffects)
library(ggplot2)
library(forcats)
library(readxl)
library(dplyr)
library(tidyverse)
Database_compact <- read_excel("/Users/marianagomez/Documents/TROPIMUNDO/Thesis/Database_compact.xlsx")
View(Database_compact)
Database_compact <- Database_compact %>%
  rename_with(make.names)
names(Database_compact)
###MODEL A - NIPAH KNOWLEDGE
Database_model <- Database_compact %>%
  mutate(
    Nipah = as.numeric(Nipah),
    Occupation = as.factor(Occupation),
    Age_range = as.factor(Age_range),
    Gender = as.factor(Gender)
  ) %>%
  filter(!is.na(Nipah), !is.na(Occupation), !is.na(Age_range), !is.na(Gender))

Database_model$Age_range <- relevel(
  factor(Database_model$Age_range),
  ref = "40-49")
modelo_firth <- logistf(Nipah ~ Occupation + Age_range + Gender, 
                        data = Database_model)
summary(modelo_firth)

#forest plot
plot_data <- plot_data %>%
  filter(term != "(Intercept)") %>%
  mutate(
    OR = exp(estimate),
    OR_low = exp(lower),
    OR_high = exp(upper),
    group = case_when(
      grepl("Occupation", term) ~ "Occupation",
      grepl("Age", term) ~ "Age",
      grepl("Gender", term) ~ "Gender"
    ),
    term_clean = term %>%
      str_replace("Occupation", "") %>%
      str_replace("Age_range", "Age ") %>%
      str_replace("Gender", "") %>%
      str_replace_all("_", " "),
    Significancia = ifelse(
      p.value < 0.05,
      "Significant",
      "Not significant"
    ),
    stars = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ))

ggplot(plot_data, aes(x = OR, y = forcats::fct_rev(term_clean), color = Significancia)) +
  geom_errorbarh(aes(xmin = OR_low, xmax = OR_high),
                 width = 0.2, linewidth = 0.8) +
    geom_point(size = 3.5, shape = 19) +
    geom_text(aes(label = stars, x = pmin(OR_high, 2000)),
            hjust = -0.3, size = 8, vjust = 0.5, show.legend = FALSE) +
  
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 0.5) +
    scale_x_log10(breaks = c(0.1, 1, 10, 100, 1000, 10000),
                labels = scales::comma) +
  scale_color_manual(values = c("Significant" = "#2E4F24", "Not significant" = "#E6AB02")) +
  facet_wrap(~group, scales = "free_y", ncol = 1) +
  
  labs(
    x = "Odds ratio (log scale)",
    y = "",
    caption = "Reference groups: Female | Visitor | 40–49 years"
  ) +
  
  theme_minimal(base_family = "Arial Narrow") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#f0f0f0"), 
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.y = element_text(size = 12),
    plot.caption = element_text(
      hjust = 0.5,
      size = 11,
      face = "italic")
  )
ggsave("NipahFINAL.png",
       bg = "transparent", dpi = 600)

#other option
plot_model(modelo_firth, type = "est", transform = "exp")

#table
install.packages("gt")
library(gt)
resultados %>%
  filter(term != "(Intercept)") %>%
  select(term, OR, OR_low, OR_high, p.value) %>%
  gt()

### MODEL B - DISEASE KNOWLEDGE
Database_model$Diseases_knowledge <- factor(Database_model$Diseases_knowledge, levels = c("0", "1", "2"))
Database_model$Age_range <- relevel(factor(Database_model$Age_range), ref = "40-49")
Database_model$Occupation <- relevel(factor(Database_model$Occupation), ref = "Visitor")

modelo_multi_firth_v2 <- nnet::multinom(Diseases_knowledge ~ Occupation + Age_range + Gender, 
                                        data = Database_model, 
                                        method = "brglmFit")
summary(modelo_multi_firth_v2)

#graph
plot_multinomial <- tidy(modelo_multi_firth_v2, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  # Limpiamos nombres de variables
  mutate(
    term_clean = term %>%
      str_replace("Occupation", "") %>%
      str_replace("Age_range", "") %>%
      str_replace("Gender", "Gender: ") %>%
      str_replace("Local students", "Students") %>%
      str_replace("Wetland management and tourism", "Wetland management"),
    # Asignamos grupos para los facetas
    y_label = case_when(
      y.level == "1" ~ "Knowledge Level 1: General/Ambiguous",
      y.level == "2" ~ "Knowledge Level 2: Specific/Zoonotic"
    )
  ) %>%
  # Filtramos el valor extremo de 10-19 en Nivel 2 para no romper la escala
  filter(!(term == "Age_range10-19" & y.level == "2"))

# 2. Definimos el orden del eje Y para que sea lógico
plot_multinomial$term_clean <- factor(
  plot_multinomial$term_clean,
  levels = rev(c(
    "10-19", "20-29", "30-39", "50+",
    "Students", "Wetland management", "Fishing and related activities", 
    "Homemakers", "Other employment", "Gender: Male"
  ))
)

# 3. Creación del gráfico
ggplot(plot_multinomial, aes(x = estimate, y = term_clean)) +
  # Línea de referencia (No effect)
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  
  # Barras de error e Intervalos de Confianza
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0.2, color = "#8c510a", linewidth = 0.7) +
  
  # Puntos (Odds Ratios) - Usando el azul de tu paleta
  geom_point(size = 3.5, color = "#2166ac") +
  
  # Escala logarítmica para manejar la dispersión de OR
  scale_x_log10(breaks = c(0.1, 0.5, 1, 5, 10, 50), labels = c("0.1", "0.5", "1", "5", "10", "50")) +
  
  # División por nivel de conocimiento
  facet_wrap(~y_label, ncol = 1) +
  
  # Etiquetas y títulos
  labs(
    x = "Odds Ratio (log scale, ref: 40-49 years/visitors/female)",
    y = "",
    title = "Multinomial Predictors of Disease Knowledge"
  ) +
  
  # Formato solicitado
  theme_minimal(base_family = "Arial Narrow") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#f0f0f0"),
    strip.background = element_rect(fill = "#f7f7f7", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA)
  )
ggsave("Disease.png",
       bg = "transparent", dpi = 300)

## only by occupation
Database_model$Occupation <- relevel(factor(Database_model$Occupation), ref = "Visitor")
modelo_occ_only <- nnet::multinom(Diseases_knowledge ~ Occupation, 
                                  data = Database_model, 
                                  method = "brglmFit")

tidy_occ <- tidy(modelo_occ_only, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)")
summary(modelo_occ_only)

## by age
Database_model$Age_range <- relevel(factor(Database_model$Age_range), ref = "40-49")
modelo_age_only <- nnet::multinom(Diseases_knowledge ~ Age_range, 
                                  data = Database_model, 
                                  method = "brglmFit")
summary(modelo_age_only)

#graph
tidy_age_fixed <- tidy(modelo_age_only, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  # ESTA ES LA CLAVE: Quitamos la barra kilométrica que rompe la escala
  filter(!(term == "Age_range10-19" & y.level == "2")) %>%
  mutate(
    p.value = 2 * (1 - pnorm(abs(statistic))),
    sig_label = ifelse(p.value < 0.05, "*", ""),
    term_clean = str_replace(term, "Age_range", ""),
    y_label = ifelse(y.level == "1", "Level 1: General Knowledge", "Level 2: Zoonotic Knowledge")
  )

# 2. El Gráfico Corregido
ggplot(tidy_age_fixed, aes(x = estimate, y = term_clean)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  
  # Intervalos en Café
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0.2, color = "#8c510a", linewidth = 0.8) +
  
  # Puntos en Azul
  geom_point(size = 4, color = "#2166ac") +
  
  # Estrellita ajustada para que quede justo arriba del punto
  geom_text(aes(label = sig_label), vjust = -0.02, size = 6, color = "black") +
  
  # Escala X arreglada con límites fijos para que respire
  scale_x_log10(
    breaks = c(0.01, 0.1, 1, 5), 
    labels = c("0.01", "0.1", "1", "5"),
    limits = c(0.01, 5) # Esto evita que se apachurren los números
  ) +
  facet_wrap(~y_label, ncol = 1) +
  
  labs(
    x = "Odds Ratio (log scale, ref: 40-49 years)",
    y = "Age Range",
    title = "Effect of Age on Disease Knowledge Levels",
    caption = "* indicates p < 0.05\nNote: 10-19 age group omitted from Level 2 due to zero observed cases."
  ) +
  
  theme_minimal(base_family = "Arial Narrow") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 11),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#f0f0f0"),
    strip.background = element_rect(fill = "#f7f7f7", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background  = element_rect(fill = "transparent", color = NA)
  )
ggsave("Disease_age.png",
       bg = "transparent", dpi = 300)


