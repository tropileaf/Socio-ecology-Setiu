library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(patchwork)
library(tidyverse)
library(scales)

theme_tesis <- theme_minimal(base_family = "Arial Narrow") +
  theme( text = element_text(size = 16),
    plot.title = element_text(
      size = 18,
      face = "bold",
      hjust = 0.5 ),
    axis.title = element_text(size = 16),
    axis.text = element_text(
      size = 14,
      color = "black" ),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    panel.grid = element_blank(),     
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),)

Database <- read_excel("/Users/marianagomez/Downloads/Database.xlsx")

#New categories
domestic_animals_ids <- c(1, 2, 3, 4)
mangrove_ids <- c(5, 14, 42, 44, 47, 48, 23, 30, 32, 24, 36, 37, 49)  
other_primates_ids <- c(6, 9, 13)
other_rodents_ids <- c(7, 8, 11, 15, 21, 29, 33, 34, 43, 12, 16, 17, 22, 10, 26)
other_bats_ids <- c(38, 39, 40)
other_mammals_ids <- c(45, 18, 46, 19, 20)
other_reptiles_ids <- c(25, 27, 28, 31, 35, 41, 51, 58, 59, 60)
ticks_flies_ids <- c(50, 52, 53, 54, 55, 57, 56)

database_ <- Database %>%
  mutate(
    Functional_Group = case_when(
      ID_animal %in% domestic_animals_ids ~
        "Domestic animals",
      ID_animal %in% mangrove_ids ~
        "Mangrove-associated fauna",
      ID_animal %in% other_primates_ids ~
        "Other primates",
      ID_animal %in% other_rodents_ids ~
        "Other rodents and shrews",
      ID_animal %in% other_bats_ids ~
        "Other bats",
      ID_animal %in% other_mammals_ids ~
        "Other mammals",
      ID_animal %in% other_reptiles_ids ~
        "Other reptiles and amphibians",
      ID_animal %in% ticks_flies_ids ~
        "Ticks and flies",
      TRUE ~ NA_character_
    ))

species_count <- data.frame(
  Functional_Group = c("Domestic animals", "Mangrove-associated fauna", "Other primates", "Other rodents and shrews",
                      "Other bats", "Other mammals", "Other reptiles and amphibians", "Ticks and flies"),
  n_species = c(4, 13, 3, 15, 3, 5, 10, 7)
)
animal_obs <- database_ %>%
  count(Functional_Group)
animal_adjusted <- animal_obs %>%
  left_join(species_count, by = "Functional_Group") %>%
  mutate(encounters_per_species = n / n_species)

ggplot(animal_adjusted, aes(x = reorder(Functional_Group, -encounters_per_species),
                            y = encounters_per_species)) +
  geom_bar(stat = "identity", fill = "#009E73") +
  labs(x = "Animal category", y = "Encounters per species", title = "") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial Narrow"),
    axis.text.x = element_text(hjust = 0.5, size = 12),
    panel.grid.major.y =  element_line(color = "gray90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    title = element_text(size = 14, face = "bold", vjust = 0.5),
    panel.grid = element_blank(),     
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),)
ggsave("Exposure.png", dpi = 300,bg = "transparent")

##New places/categories
database_ <- database_ %>%
  filter(Place_category!= "n/a")
database_ <- database_ %>%
  mutate(
    Place_category = recode(
      Place_category,
      "Domestic" = "Domestic",
      "Village" = "Peridomestic",
      "Buffer zone" = "Transitional",
      "Nature areas" = "Sylvatic" )
  )

order_domestic <- database_ %>%
  count(Functional_Group, Place_category) %>%
  group_by(Functional_Group) %>%
  mutate(prop = n / sum(n)) %>%
  filter(Place_category == "Domestic") %>%
  arrange(desc(prop)) %>%
  pull(Functional_Group)

#New graph
plot_data <- database_ %>%
  mutate(Functional_Group = fct_relevel(Functional_Group, 
                                        "Domestic animals", 
                                        "Mangrove-associated fauna")) %>%
  count(Functional_Group, Place_category) %>%
  group_by(Functional_Group) %>%
  mutate(Percentage = n / sum(n)) %>%
  ungroup()
plot_data <- plot_data %>%
  mutate(
    Panel = if_else(
      as.numeric(Functional_Group) <= 4, 
      "Panel 1", 
      "Panel 2"
    ))

plot_data %>%
  ggplot(aes(
    x = Functional_Group, 
    y = Percentage, 
    fill = Place_category
  )) +
  geom_col(
    position = position_dodge2(preserve = "single", padding = 0.1), 
    width = 0.8
  ) +
  facet_wrap(~ Panel, scales = "free_x", nrow = 2) +
  scale_fill_manual(values = c(
    "Domestic" = "#3374B0",
    "Peridomestic" = "#9ED3DF",
    "Transitional" = "#E7D674",
    "Sylvatic" = "#77AB76"
  )) +
  scale_y_continuous(
    labels = scales::percent_format(),
    breaks = seq(0, 1, by = 0.2),       
    minor_breaks = seq(0, 1, by = 0.1), 
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Functional Group",
    y = "Proportion of total observations",
    fill = "Location category"
  ) +
  theme_tesis +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(color = "grey70", linewidth = 0.6), 
        panel.grid.minor.y = element_line(color = "grey85", linewidth = 0.3), 
    strip.text = element_blank()
  )
ggsave("Figure1_FINAL.png", dpi = 300,bg = "transparent")

#cumulative bar graph
database_ <- database_ %>%
  mutate(
    Functional_Group = factor(
      Functional_Group,
      levels = rev(order_domestic)),
    Place_category = factor(
      Place_category,
      levels = c(
        "Domestic",
        "Peridomestic",
        "Transitional",
        "Sylvatic" )) )

database_ %>%
  ggplot(aes(
    y = Functional_Group,
    fill = Place_category
  )) +
  geom_bar(
    position = position_fill(reverse = TRUE),
    width = 0.7
  ) +
  scale_fill_manual(values = c(
    "Domestic" = "#E69F00",
    "Peridomestic" = "#D55E00",
    "Transitional" = "#56B4E9",
    "Sylvatic" = "#009E73"
  )) +
  scale_x_continuous(
    labels = scales::percent_format(),
    expand = c(0, 0.03)
  ) +
  labs(
    x = "Proportion of observations",
    y = NULL,
    fill = "Location category"
  ) +
  theme_tesis +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  )

##PERCEPTIONS##
#Risk
risk_long <- Database %>%
  separate_rows(Risk, sep = "; ")
risk_clean <- risk_long %>%
  filter(Risk != "n/a")

prop_animal_risk <- prop.table(
  table(risk_clean$Functional_Group, risk_clean$Risk),
  margin = 1)
plot_risk <- as.data.frame(prop_animal_risk) %>%
  rename(
    Functional_Group = Var1,
    Risk = Var2)
animal_order <- c(
  "Ticks and flies",
  "Other reptiles and amphibians",
  "Other bats",
  "Other rodents and shrews",
  "Other mammals",
  "Mangrove-associated fauna",
  "Other primates",
  "Domestic animals")

plot_risk$Functional_Group <- factor(
  plot_risk$Functional_Group,
  levels = animal_order)
plot_risk$Risk <- factor(
  plot_risk$Risk,
  levels = c(
    "None",
    "Nuisance",
    "Other",
    "Disease",
    "Toxic",
    "Physical harm"))

#plot
plot_risks <- ggplot(
  plot_risk,
  aes(
    x = Functional_Group,
    y = Freq,
    fill = Risk
  )) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_manual(values = c(
    "None" = "#2C7FB8", 
    "Nuisance" = "#7FCDBB",
    "Other" = "#BDBDBD",
    "Disease" = "#FDAE61",
    "Toxic" = "#F46D43",
    "Physical harm" = "#A50026"
  )) +
  labs(
    title ="",
    x = "",
    y = "Proportion (%)"
  ) +
  theme_tesis +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "right",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank()) +
guides(fill = guide_legend(reverse = TRUE))

#Perception
perception_long <- Database %>%
  separate_rows(Perception, sep = "; ")
perception_clean <- perception_long %>%
  mutate(
    Perception = case_when(
      Perception == "Pests" ~ "Pest",
      Perception == "n/a" ~ NA_character_,
      TRUE ~ Perception
    )
  ) %>%
  filter(!is.na(Perception))

prop_animal_perception <- prop.table(
  table(
    perception_clean$Functional_Group,
    perception_clean$Perception),
  margin = 1)
plot_perception_data <- as.data.frame(prop_animal_perception) %>%
  rename(
    Functional_Group = Var1,
    Perception = Var2)

plot_perception_data$Functional_Group <- factor(
  plot_perception_data$Functional_Group,
  levels = animal_order)
plot_perception_data$Perception <- factor(
  plot_perception_data$Perception,
  levels = c(
    "Friendly",
    "Not dangerous",
    "Neutral",
    "Pest",
    "Dangerous"))

#plot
plot_perception <- ggplot(
  plot_perception_data,
  aes(
    x = Functional_Group,
    y = Freq,
    fill = Perception
  )
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_fill_manual(values = c(
    "Friendly" = "#2C7FB8", 
    "Not dangerous" = "#66C2A5",
    "Neutral" = "#BDBDBD",
    "Pest" = "#FC8D62",
    "Dangerous" = "#A50026")) +
  labs(
    title = "",
    x = "",
    y = "Proportion (%)"
  ) +
  theme_tesis +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "right",
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  guides(fill = guide_legend(reverse = TRUE))
#combined
plot_perception <- plot_perception +
  theme(plot.margin = margin(0, 0, 0, 0))
plot_risks <- plot_risks +
  theme(plot.margin = margin(0, 0, 0, 0))

combined_plot <- plot_perception / plot_risks +
  plot_layout(heights = c(1, 1)) &
  theme(
    plot.margin = margin(1, 1, 1, 1))
ggsave(
  "Figure1_perception_risk_V2.png",
  combined_plot,
  dpi = 600,
  bg = "transparent")

##NEW GRAPHS
#Risk
risk_long <- database_ %>%
  separate_rows(Risk, sep = "; ")
risk_clean <- risk_long %>%
  filter(Risk != "n/a" & Risk != "Other")

prop_animal_risk <- prop.table(table(risk_clean$Functional_Group, risk_clean$Risk), margin = 1)
plot_risk <- as.data.frame(prop_animal_risk) %>%
  rename(Functional_Group = Var1, Risk = Var2)

animal_order <- c(
  "Domestic animals",
  "Mangrove-associated fauna",
  "Other primates",
  "Other bats",
  "Other rodents and shrews",
  "Other mammals",
  "Other reptiles and amphibians",
  "Ticks and flies"
)
plot_risk <- plot_risk %>%
  mutate(
    Functional_Group = factor(Functional_Group, levels = animal_order),
    Risk = factor(Risk, levels = c("Physical harm", "Disease", "Nuisance", "Toxic", "None"))
  )

plot_risks <- ggplot(plot_risk, aes(x = Functional_Group, y = Freq, fill = Risk)) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0.1), width = 0.8) +
  scale_y_continuous(
    labels = scales::percent_format(),
    breaks = seq(0, 1, by = 0.2),       
    minor_breaks = seq(0, 1, by = 0.1), 
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(values = c(
    "None" = "#2C7FB8", "Nuisance" = "#7FCDBB",
    "Disease" = "#FDAE61", "Toxic" = "#F46D43", "Physical harm" = "#A50026"
  )) +
  labs(title = NULL, x = "Functional Group", y = "Proportion within group", fill = "Risk") +
  theme_tesis +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey70", linewidth = 0.6),
    panel.grid.minor.y = element_line(color = "grey85", linewidth = 0.3),
    strip.text = element_blank()
  )
#Perception
perception_long <- database_ %>%
  separate_rows(Perception, sep = "; ")
perception_clean <- perception_long %>%
  mutate(
    Perception = case_when(
      Perception == "Pests" ~ "Pest",
      Perception == "n/a" ~ NA_character_,
      TRUE ~ Perception
    )
  ) %>%
  filter(!is.na(Perception) & Perception != "Neutral")

prop_animal_perception <- prop.table(table(perception_clean$Functional_Group, perception_clean$Perception), margin = 1)
plot_perception_data <- as.data.frame(prop_animal_perception) %>%
  rename(Functional_Group = Var1, Perception = Var2)

plot_perception_data <- plot_perception_data %>%
  mutate(
    Functional_Group = factor(Functional_Group, levels = animal_order),
    Perception = factor(Perception, levels = c("Dangerous", "Pest","Not dangerous", "Friendly"))
  )

plot_perception <- ggplot(plot_perception_data, aes(x = Functional_Group, y = Freq, fill = Perception)) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0.1), width = 0.8) +
  scale_y_continuous(
    labels = scales::percent_format(),
    breaks = seq(0, 1, by = 0.2),       
    minor_breaks = seq(0, 1, by = 0.1), 
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(values = c(
    "Friendly" = "#2C7FB8", "Not dangerous" = "#66C2A5",
    "Pest" = "#FC8D62", "Dangerous" = "#A50026"
  )) +
  labs(title = NULL, x = NULL, y = "Proportion of observations", fill = "Perception") +
  theme_tesis +
  theme(
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey70", linewidth = 0.6),
    panel.grid.minor.y = element_line(color = "grey85", linewidth = 0.3),
    strip.text = element_blank(),
    axis.text.x = element_blank()
  )

#Combined
combined_plot <- plot_perception / plot_risks +
  plot_layout(heights = c(1, 1)) &
  theme(plot.margin = margin(10, 10, 10, 10))

combined_plot
ggsave(
  filename = "Perception_FINAL.png", 
  plot = combined_plot, 
  width = 15,  
  height = 8,   
  dpi = 600    
)

# --- 1. Gráfica de Riesgo (Small Multiples) ---
# Usamos facet_grid para separar por la variable Risk
plot_risks_sm <- ggplot(plot_risk, aes(x = Functional_Group, y = Freq, fill = Risk)) +
  geom_col(width = 0.7) +
  facet_grid(cols = vars(Risk), scales = "free_y") + 
  scale_y_continuous(labels = scales::percent_format(), breaks = c(0, 0.5, 1)) +
  scale_fill_manual(values = c("None" = "#2C7FB8", "Nuisance" = "#7FCDBB", 
                               "Disease" = "#FDAE61", "Toxic" = "#F46D43", 
                               "Physical harm" = "#A50026")) +
  labs(title = "Perceived Risks by Category", x = NULL, y = "Proportion") +
  theme_tesis +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "none" # Ya no necesitamos leyenda si cada panel tiene su título
  )

# --- 2. Gráfica de Percepción (Small Multiples) ---
plot_perception_sm <- ggplot(plot_perception_data, aes(x = Functional_Group, y = Freq, fill = Perception)) +
  geom_col(width = 0.7) +
  facet_grid(cols = vars(Perception), scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(), breaks = c(0, 0.5, 1)) +
  scale_fill_manual(values = c("Friendly" = "#2C7FB8", "Not dangerous" = "#66C2A5", 
                               "Pest" = "#FC8D62", "Dangerous" = "#A50026")) +
  labs(title = "General Perceptions by Category", x = NULL, y = "Proportion") +
  theme_tesis +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "none"
  )

# Combinamos
combined_plot_sm <- plot_perception_sm / plot_risks_sm
combined_plot_sm

# --- 1. Mapa de Calor para Percepción ---
# Asegúrate de que los niveles estén definidos como antes (animal_order)
plot_perception_hm <- ggplot(plot_perception_data, aes(x = Perception, y = Functional_Group, fill = Freq)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b", labels = scales::percent_format()) +
  labs(title = "Heatmap: General Perceptions", x = NULL, y = NULL, fill = "Proportion") +
  theme_tesis +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# --- 2. Mapa de Calor para Riesgos ---
plot_risks_hm <- ggplot(plot_risk, aes(x = Risk, y = Functional_Group, fill = Freq)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient(low = "#fff5eb", high = "#7f2704", labels = scales::percent_format()) +
  labs(title = "Heatmap: Perceived Risks", x = NULL, y = NULL, fill = "Proportion") +
  theme_tesis +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# Combinamos
combined_hm <- plot_perception_hm / plot_risks_hm
combined_hm

##nuevo intento

animal_order <- c(
  "Domestic animals", "Mangrove-associated fauna", "Other primates",
  "Other bats", "Other rodents and shrews", "Other mammals",
  "Other reptiles and amphibians", "Ticks and flies"
)

# --- 1. Gráfica de Percepción por Animal ---
plot_perception_animal <- ggplot(plot_perception_data, aes(x = Perception, y = Freq, fill = Perception)) +
  geom_col(width = 0.8) +
  # Creamos los 8 paneles, uno por animal
  facet_wrap(~ factor(Functional_Group, levels = animal_order), ncol = 4) +
  scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.1))) +
  # Colores amigables con daltonismo que ya teníamos
  scale_fill_manual(values = c("Friendly" = "#2C7FB8", "Not dangerous" = "#66C2A5", 
                               "Pest" = "#FC8D62", "Dangerous" = "#A50026")) +
  labs(title = "A. General Perceptions by Animal Group", x = NULL, y = "Proportion") +
  theme_tesis +
  theme(
    legend.position = "none", # Quitamos leyenda porque el eje X ya dice qué es cada barra
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5)
  )

# --- 2. Gráfica de Riesgo por Animal ---
plot_risk_animal <- ggplot(plot_risk, aes(x = Risk, y = Freq, fill = Risk)) +
  geom_col(width = 0.8) +
  # Creamos los 8 paneles, uno por animal
  facet_wrap(~ factor(Functional_Group, levels = animal_order), ncol = 4) +
  scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("None" = "#2C7FB8", "Nuisance" = "#7FCDBB", 
                               "Disease" = "#FDAE61", "Toxic" = "#F46D43", "Physical harm" = "#A50026")) +
  labs(title = "B. Perceived Risks by Animal Group", x = NULL, y = "Proportion") +
  theme_tesis +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5)
  )

# --- 3. Combinar todo en una mega-figura ---
# Apilamos la sección de Percepción arriba y la de Riesgos abajo
mega_figura <- plot_perception_animal / plot_risk_animal +
  plot_layout(heights = c(1, 1))

ggsave("Animal_Profiles_Plot.png", plot = mega_figura, width = 12, height = 11, dpi = 300)

###FINAL GRAPH###
animal_order_rev <- rev(c(
  "Domestic animals", "Mangrove-associated fauna", "Other primates",
  "Other bats", "Other rodents and shrews", "Other mammals",
  "Other reptiles and amphibians", "Ticks and flies"
))

# --- 1. DATOS Y GRÁFICA DE PERCEPCIÓN ---
plot_perception_div <- plot_perception_data %>%
  mutate(
    Functional_Group = factor(Functional_Group, levels = animal_order_rev),
    Freq_Div = if_else(Perception %in% c("Pest", "Dangerous"), -Freq, Freq),
    Perception = factor(Perception, levels = c("Dangerous", "Pest", "Friendly","Not dangerous")),
    Label = if_else(Freq > 0.06, scales::percent(Freq, accuracy = 1), "")
  )

p_div_perc <- ggplot(plot_perception_div, aes(x = Functional_Group, y = Freq_Div, fill = Perception)) +
  geom_col(width = 0.75) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 4, color = "black", fontface = "bold") +
  coord_flip() +
  scale_fill_manual(
    values = c("Dangerous" = "#E78AC3", "Pest" = "#FC8D62", 
               "Not dangerous" = "#8DA0CB", "Friendly" = "#66C2A5"),
    breaks = c("Dangerous", "Pest", "Not dangerous", "Friendly")
  ) +
  scale_y_continuous(
    labels = function(x) scales::percent(abs(x)), 
    breaks = seq(-1, 1, by = 0.2),
    minor_breaks = seq(-1, 1, by = 0.1)
  ) +
  labs(title = "A.", x = NULL, y = NULL) +
  theme_tesis +
  theme(
    plot.title = element_text(hjust = 0),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major.x = element_line(color = "grey70", linewidth = 0.6),
    panel.grid.minor.x = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold")
  )

# --- 2. DATOS Y GRÁFICA DE RIESGO ---
plot_risk_div <- plot_risk %>%
  mutate(
    Functional_Group = factor(Functional_Group, levels = animal_order_rev),
    Freq_Div = if_else(Risk %in% c("Nuisance", "Disease", "Toxic", "Physical harm"), -Freq, Freq),
    Risk = factor(Risk, levels = c("Physical harm", "Toxic", "Disease", "Nuisance", "None")),
    Label = if_else(Freq > 0.06, scales::percent(Freq, accuracy = 1), "")
  )

p_div_risk <- ggplot(plot_risk_div, aes(x = Functional_Group, y = Freq_Div, fill = Risk)) +
  geom_col(width = 0.75) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 4, color = "black", fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("Physical harm" = "#E78AC3", "Toxic" = "#A6D854", 
                               "Disease" = "#FC8D62", "Nuisance" = "#E6D66B", "None" = "#66C2A5")) +
  scale_y_continuous(
    labels = function(x) scales::percent(abs(x)),
    breaks = seq(-1, 1, by = 0.2),       
    minor_breaks = seq(-1, 1, by = 0.1)  
  ) +
  labs(title = "B.", x = NULL, y = "Proportion of responses") +
  theme_tesis +
  theme(
    plot.title = element_text(hjust = 0),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major.x = element_line(color = "grey70", linewidth = 0.6),
    panel.grid.minor.x = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold"),
  )

# --- 3. COMBINAR GRÁFICAS ---
combined_divergent <- p_div_perc / p_div_risk +
  plot_layout(heights = c(1, 1))

ggsave(
  filename = "Divergent_PlotGOOD.png", 
  plot = combined_divergent,
  width = 10,
  height = 9,
  dpi = 300
)





