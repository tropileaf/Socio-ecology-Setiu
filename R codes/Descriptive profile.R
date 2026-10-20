library(dplyr)
library(ggplot2)
library(scales)

demographics_unique <- Database %>%
  distinct(ID_participant, .keep_all = TRUE)

#Occupation - sided barplot
demographics_unique %>%
  count(Occupation) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = reorder(Occupation, prop), y = prop)) +
  geom_col(fill = "#1b7837", width = 0.7) + # Verde manglar
  geom_text(aes(label = percent(prop, accuracy = 0.1)), 
            hjust = -0.2, family = "Arial Narrow", size = 4) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, .15))) + 
  labs(title = "Occupation", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 10, family = "Arial Narrow"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA)
  )
ggsave("Occupation.png", bg = "transparent")

#Age range - vertical bar plot
demographics_unique %>%
  count(Age_range) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = Age_range, y = prop)) +
  geom_col(fill = "#D98C56", width = 0.6) + 
  geom_text(aes(label = percent(prop, accuracy = 1)), 
            vjust = -0.5, family = "Arial Narrow") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1), # Formatea las etiquetas del eje Y a porcentaje
    expand = expansion(mult = c(0, .2))
  ) +
  labs(title = "Age Range", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(family = "Arial Narrow",  size = 12),
    axis.text.x = element_text(family = "Arial Narrow",  size = 12),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, vjust = 0.07),
    panel.grid.major.y = element_line(color = "grey70", linewidth = 0.6),
    panel.grid.minor.y = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA)
  )
ggsave("Age.png", bg = "transparent")

#Gender - donut chart
demographics_unique %>%
  count(Gender) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = 2, y = prop, fill = Gender)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  xlim(1, 2.5) +
  scale_fill_manual(values = c("#63B7A6", "#E9DD75")) + # Azul y Verde
  geom_text(aes(label = paste0(Gender, "\n", percent(prop, accuracy = 1))), 
            position = position_stack(vjust = 0.5), color = "black", fontface = "bold", size = 5) +
  labs(title = "Gender") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  )
ggsave("GenderWHITE.png", bg = "transparent")

#Nationality - donut chart
demographics_unique %>%
  count(Nationality) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = 2, y = prop, fill = reorder(Nationality, -prop))) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y") +
  xlim(1, 2.5) + 
  scale_fill_manual(values = c("#5A95C8", "#D98C56")) + 
  geom_text(aes(label = ifelse(prop > 0.03, 
                               paste0(Nationality, "\n", scales::percent(prop, accuracy = 1)), 
                               "")), 
            position = position_stack(vjust = 0.5), 
            color = "black", fontface = "bold", family = "Arial Narrow", size = 5) +
  labs(title = "Nationality", fill = NULL) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "none"
  )
ggsave("NationalityWHITE.png", bg = "transparent")

#bar graphs
plot_bar <- function(data, var, xlab, ordered = FALSE){
    data %>%
      count({{var}}) %>%
      mutate(prop = n / sum(n)) %>%
      ggplot(aes(
        x = if(ordered) {{var}} else reorder({{var}}, -prop),
        y = prop
      )) +
      geom_bar(stat = "identity", fill = "#698B22") +
      geom_text(aes(label = scales::percent(prop, accuracy = 1)),
                vjust = -0.3, size = 3) +
      labs(x = xlab, y = "Percentage") +
      theme_minimal() +
      theme(
        text = element_text(family = "Arial Narrow"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(hjust = 1, size = 11),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(10, 20, 10, 20)
        )
  }
plot_bar(demographics_unique, Occupation, "Occupation")
plot_bar(demographics_unique, Gender, "Gender")
plot_bar(demographics_unique, Age_range, "Age range", ordered = TRUE)
plot_bar(demographics_unique, Nationality, "Nationality")

#sided graphs
plot_bar <- function(data, var, xlab, ordered = FALSE){
  data %>%
    count({{var}}) %>%
    mutate(prop = n / sum(n)) %>%
    ggplot(aes(
      x = if(ordered) {{var}} else reorder({{var}}, prop), 
      y = prop
    )) +
    geom_bar(stat = "identity", fill = "#BCEE68", width = 0.7) +
    geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
              hjust = -0.2, size = 3.5, family = "Arial Narrow") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1), 
                       breaks = seq(0, 1, by = 0.2)) +
    labs(x = xlab, y = "Percentage") +
    coord_flip() +
    theme_minimal() +
    theme(
      text = element_text(family = "Arial Narrow"),
      axis.text.y = element_text(size = 11, color = "black"),
      axis.text.x = element_text(size = 10),
      panel.grid.major.x = element_line(color = "gray90"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(10, 30, 10, 10)
    )
}
plot_bar(demographics_unique, Occupation, "Occupation")
plot_bar(demographics_unique, Gender, "Gender")
plot_bar(demographics_unique, Age_range, "Age range", ordered = TRUE)
plot_bar(demographics_unique, Nationality, "Nationality")

##New occupation graph
plot_donut <- function(data, var, title = "Occupation"){
  paleta_personalizada <- c( "#3374B0", "#9ED3DF", "#77AB76", "#E7D674","#FEF3CC")
  data %>%
    count({{var}}) %>%
    mutate(prop = n / sum(n)) %>%
    ggplot(aes(x = 2, y = prop, fill = {{var}})) +
    geom_bar(stat = "identity", color = "white", width = 1) +
    coord_polar(theta = "y", start = 0) +
    geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
              position = position_stack(vjust = 0.5),
              size = 5, family = "Arial Narrow", color = "black") +
    # Si quieres un PIE CHART normal (sin hueco), elimina esta línea `xlim(...)`.
    xlim(0.5, 2.5) +
    scale_fill_manual(values = colorRampPalette(paleta_personalizada)(6)) +
    theme_void() +
    theme(
      text = element_text(family = "Arial Narrow"),
      legend.position = "right",
      legend.text = element_text(size = 13),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.margin = margin(1, 1, 1, 1)
    ) +
    labs(title = title, fill = "Category")
}
plot_donut(demographics_unique, Occupation, "Occupation")
ggsave("OccupationDonut.png", bg = "transparent")

### FREQUENCY + PART - MANGROVES###
#Mejorado - Frequency
frecuencia_data <- data.frame(
  Freq = c("Rarely", "Daily", "Weekly", "Monthly"),
  Value = c(0.461, 0.436, 0.073, 0.030)
)
ggplot(frecuencia_data, aes(x = reorder(Freq, Value), y = Value)) +
  # Línea del dulce (gris sutil)
  geom_segment(aes(xend = Freq, y = 0, yend = Value), color = "grey80", linewidth = 1) +
  # Círculos: Verde para los extremos, Gris para el resto
  geom_point(aes(color = (Freq %in% c("Rarely", "Daily"))), size = 5) +
  scale_color_manual(values = c("TRUE" = "#1b7837", "FALSE" = "#bdbdbd")) +
  # Etiquetas de porcentaje
  geom_text(aes(label = percent(Value, accuracy = 0.1)), 
            hjust = -0.5, family = "Arial Narrow", fontface = "bold") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 0.6), expand = c(0, 0)) +
  labs(title = "Frequency of visiting", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12, family = "Arial Narrow"),
    plot.title = element_text(face = "bold", size = 14),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
ggsave("Frequency_mangroves.png", bg = "transparent")

#Mejorado - part of mangrove
partes_data <- data.frame(
  Parte = c("n/a", "River", "Jetty", "General", "Boardwalk", "Mangrove forest", "Other"),
  Value = c(28.5, 21.2, 19.4, 14.5, 9.7, 4.8, 1.8)
)

ggplot(partes_data, aes(area = Value, fill = reorder(Parte, -Value), 
                        label = paste0(Parte, "\n", Value, "%"))) +
  geom_treemap(color = "white", size = 2) +
  # Usamos la paleta de manglar + Gris para el n/a
  scale_fill_manual(values = c(
    "n/a" = "#bdbdbd",             # Gris para datos faltantes
    "River" = "#2166ac",           # Azul (agua)
    "Jetty" = "#bf812d",           # Café (madera/muelle)
    "General" = "#1b7837",         # Verde
    "Boardwalk" = "#8c510a",       # Café oscuro
    "Mangrove forest" = "#5aae61", # Verde claro
    "Other" = "#dfc27d"            # Arena
  )) +
  geom_treemap_text(colour = "white", place = "centre", 
                    family = "Arial Narrow", fontface = "bold", grow = FALSE) +
  labs(title = "Part of mangrove visited (including missing data)") +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5, margin = margin(b=10, t=10)),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
ggsave("Part_mangroves.png", bg = "transparent")

#obsolete
database_clean <- demographics_unique %>%
  filter(Frequency_mangrove != "n/a")
database_clean1 <- demographics_unique %>%
  filter(Part_mangrove_Category != "n/a")

plot_bar(database_clean, Frequency_mangrove, "Frequency of visiting")
plot_bar(database_clean, Part_mangrove_Category, "Part of mangrove")

parts_long <- demographics_unique %>%
  select(ID_participant, Boardwalk_Wetlands, Mangrove_Forest, River, Jetty, Other) %>%
  distinct(ID_participant, .keep_all = TRUE) %>%
  pivot_longer(cols = -ID_participant, names_to = "Part", values_to = "Used") %>%
  filter(Used == 1)

library(stringr)
parts_long <- parts_long %>%
  mutate(Part = recode(Part,
                       "Boardwalk Wetlands" = "Boardwalk",
                       "Mangrove_Forest" = "Mangrove forest"
  ))
parts_long %>%
  count(Part) %>%
  mutate(prop = n / n_distinct(parts_long$ID_participant)) %>%
  ggplot(aes(x = reorder(Part, -prop), y = prop)) +
  geom_bar(stat = "identity", fill = "#BCEE68") +
  geom_text(aes(label = percent(prop, accuracy = 1)),
            vjust = -0.3, size = 4) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  labs(x = "Part of mangrove", y = "% of participants") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial Narrow"),
    axis.text.x = element_text(hjust = 0.5),
    panel.grid.major.y =  element_line(color = "gray90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
  )
unique(parts_long$Part)


### ANIMALS ENCOUNTERS ###
##What animal CATEGORIES are most encountered? --> reptiles won but it's biased
Database %>%
  count(Animal_category) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = reorder(Animal_category, -prop), y = prop)) +
  geom_bar(stat = "identity", fill = "#7570B3") + 
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            vjust = -0.3, size = 4) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(x = "Animal category", y = "% of observations") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial Narrow"),
    axis.text.x = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )
#donut chart
paleta_manglar_7 <- c(
  "#543005", # Café muy oscuro (Domestic)
  "#8c510a", # Café medio (Other wild mammals)
  "#bf812d", # Ocre/Arena (Primates)
  "#1b7837", # Verde bosque (Bats)
  "#5aae61", # Verde claro (Rodents)
  "#4393c3", # Azul medio (Flies and ticks)
  "#2166ac"  # Azul oscuro (Reptiles and amphibians)
)
Database %>%
  count(Animal_category) %>%
  mutate(prop = n / sum(n)) %>%
  # Reordenamos por proporción para que los colores sigan un orden lógico
  ggplot(aes(x = 2, y = prop, fill = reorder(Animal_category, -prop))) +
  geom_bar(stat = "identity", color = "white", size = 0.5) +
  coord_polar(theta = "y", start = 0) +
  xlim(0.5, 2.5) + 
  geom_text(aes(label = ifelse(prop > 0.05, percent(prop, accuracy = 1), "")), 
            position = position_stack(vjust = 0.5), 
            color = "white", fontface = "bold") +
  scale_fill_manual(values = paleta_manglar_7) +
  theme_void() + 
  labs(title = "Most observed animal category",
       fill = "Animal category") +
  theme(
    text = element_text(family = "Arial Narrow"),
    legend.position = "right",
    # Ajustamos el margen del título
    # t = 20 (margen superior para despegarlo del borde)
    # b = -10 (margen inferior negativo para acercarlo al centro si es necesario)
    plot.title = element_text(hjust = 0.5, 
                              face = "bold", 
                              size = 14,
                              margin = margin(t = 20, b = -10)), 
    plot.subtitle = element_text(hjust = 0.5, 
                                 size = 9, 
                                 color = "grey30",
                                 margin = margin(t = 5, b = 0))
  )
ggsave("Most observed animal cat.png", bg = "transparent")

##Tree map
install.packages("treemapify")
library(treemapify)

Database %>%
  count(Animal_category) %>%
  ggplot(aes(area = n, fill = Animal_category, label = Animal_category)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", grow = TRUE) +
  scale_fill_viridis_d(option = "plasma") +
  theme_minimal() +
  labs(title = "Composición de Categorías")

##Lollipop chart
data_plot <- Database %>%
  count(Animal_category) %>%
  mutate(prop = n / sum(n))

ggplot(data_plot, aes(x = reorder(Animal_category, prop), y = prop)) +
  # La línea del dulce
  geom_segment(aes(x = reorder(Animal_category, prop), 
                   xend = reorder(Animal_category, prop), # Usar reorder aquí también es clave
                   y = 0, yend = prop), 
               color = "grey") +
  # El círculo
  geom_point(size = 5, color = "#7570B3") +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), 
            hjust = -0.5, size = 3.5, family = "Arial Narrow") +
  coord_flip() + 
  # Usamos el valor máximo calculado del dataframe
  scale_y_continuous(labels = scales::percent_format(), 
                     limits = c(0, max(data_plot$prop) + 0.1)) +
  labs(x = NULL, y = "% de observaciones") +
  theme_minimal() +
  theme(text = element_text(family = "Arial Narrow"))

#Proporcional - integrando número de especies por categoría
#IMPROVED
library(ggplot2)
library(dplyr)

# Usamos tu dataframe ajustado
animal_adjusted %>%
  ggplot(aes(x = reorder(Animal_category, encounters_per_species), 
             y = encounters_per_species)) +
  # Línea de fondo para conectar los puntos (estilo Lollipop pero más grueso)
  geom_segment(aes(xend = Animal_category, y = 0, yend = encounters_per_species), 
               color = "grey90", size = 1.5) +
  # El punto: el tamaño depende de cuántas especies hay en la categoría
  geom_point(aes(size = n_species), color = "#1b7837", alpha = 0.8) +
  # Etiqueta con el valor exacto encima del punto
  geom_text(aes(label = round(encounters_per_species, 1)), 
            hjust = -0.8, family = "Arial Narrow", size = 3.5) +
  # Ajuste de tamaños de burbuja para que sean visibles
  scale_size_continuous(range = c(3, 10), name = "Nº of species") +
  coord_flip() +
  labs(
    title = "Relative encounter frequency",
    subtitle = "Mean sightings of each species within categories",
    x = NULL, 
    y = "Sightings / Nº species"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial Narrow", size=14),
    panel.grid.major = element_line(color = "gray95"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
ggsave("Encounter frequency.png", bg = "transparent")

#obsolete - simple barchart
species_count <- data.frame(
  Animal_category = c("Domestic animals", "Primates", "Rodents", "Bats",
                      "Other wild mammals", "Reptiles and amphibians", "Flies and ticks"),
  n_species = c(4, 4, 10, 4, 8, 17, 6)
)
animal_obs <- Database %>%
  count(Animal_category)
animal_adjusted <- animal_obs %>%
  left_join(species_count, by = "Animal_category") %>%
  mutate(encounters_per_species = n / n_species)

ggplot(animal_adjusted, aes(x = reorder(Animal_category, -encounters_per_species),
                            y = encounters_per_species)) +
  geom_bar(stat = "identity", fill = "#7570B3") +
  labs(x = "Animal category", y = "Encounters per species") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial Narrow"),
    axis.text.x = element_text(hjust = 0.5),
    panel.grid.major.y =  element_line(color = "gray90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
  )

#Top species list: Which species are most frequently encountered?
species_counts <- Database %>%
  count(ID_animal, sort = TRUE)
top_species <- Database %>%
  count(ID_animal, sort = TRUE) %>%
  slice_head(n = 20)
top_species

species_counts_mangrove <- Database %>%
  filter(ID_animal %in% mangrove_ids) %>%
  count(ID_animal, sort = TRUE)
top_species_mangrove <- Database %>%
  filter(ID_animal %in% mangrove_ids) %>%
  count(ID_animal, sort = TRUE) %>%
  slice_head(n = 10)

top_species <- top_species %>%
  mutate(Species = case_when(
    ID_animal == 3 ~ "Cat",
    ID_animal == 5 ~ "Long-tailed macaque",
    ID_animal == 4 ~ "Dog",
    ID_animal == 34 ~ "Plantain squirrel",
    ID_animal == 57 ~ "House fly",
    ID_animal == 50 ~ "Oriental latrine fly",
    ID_animal == 25 ~ "House gecko",
    ID_animal == 6 ~ "Pig-tailed macaque",
    ID_animal == 59 ~ "Banded bullfrog",
    ID_animal == 1 ~ "Chicken",
    TRUE ~ as.character(ID_animal)
  ))

top_species_plot <- top_species %>%
  mutate(Species = reorder(Species, n))
##lollipop
ggplot(top_species_plot, aes(x = Species, y = n)) +
  geom_segment(aes(xend = Species, y = 0, yend = n), 
               color = "grey85", size = 1.2) +
  # El punto (usamos un verde manglar fuerte)
  geom_point(size = 5, color = "#1b7837") +
  # Añadimos el número exacto al final de la línea
  geom_text(aes(label = n), 
            hjust = -0.6, 
            family = "Arial Narrow", 
            fontface = "bold",
            size = 4) +
  coord_flip() +
  # Ajustamos el límite superior para que el texto no se corte
  scale_y_continuous(expand = expansion(mult = c(0, .15))) +
  labs(
    title = "Top 10 Most Frequently Encountered Species",
    x = NULL,
    y = "Number of observations"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial Narrow"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.title = element_text(face = "bold", size = 14),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
ggsave("top_species.png", bg = "transparent", width = 8, height = 5)

#simple bar chart
ggplot(top_species, aes(x = reorder(Species, n), y = n)) +
  geom_bar(stat = "identity", fill = "#FFEC8B") +
  geom_text(aes(label = n), hjust = -0.2, size = 4,family = "Arial Narrow") +
  coord_flip() +
  labs(x = "Species", y = "Number of encounters") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial Narrow"),
    axis.text.y = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.y =  element_blank(),
    panel.grid.major.x = element_blank()
  )

##Place of observation
database_ <- Database %>%
  filter(Place_category!= "n/a")

#improved
place_data <- data.frame(
  Place = c("Nature areas", "Domestic", "Village", "Buffer zone"),
  Value = c(0.34, 0.31, 0.26, 0.09)
)

ggplot(place_data, aes(x = reorder(Place, Value), y = Value, color = Place)) +
  geom_segment(aes(xend = Place, y = 0, yend = Value), size = 1.2, color = "grey85") +
  geom_point(size = 5) +
  # Etiquetas de porcentaje
  geom_text(aes(label = scales::percent(Value, accuracy = 1)), 
            hjust = -0.5, family = "Arial Narrow", fontface = "bold") +
  scale_color_manual(values = c(
    "Nature areas" = "#1b7837", # Verde
    "Domestic" = "#bf812d",     # Ocre
    "Village" = "#8c510a",      # Café oscuro
    "Buffer zone" = "#67a9cf"    # Azul
  )) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, .2))) +
  labs(title = "Common Places of Observation", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 11, family = "Arial Narrow", face = "bold", color = "black"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
ggsave("Places of observation.png", bg = "transparent")

#place x animal category
order_domestic <- database_ %>%
  count(Animal_category, Place_category) %>%
  group_by(Animal_category) %>%
  mutate(prop = n / sum(n)) %>%
  filter(Place_category == "Domestic") %>%
  arrange(desc(prop)) %>%
  pull(Animal_category)

database_ <- database_ %>%
  mutate(Animal_category = factor(Animal_category, levels = rev(order_domestic)))

database_ %>%
  mutate(
    Place_category = factor(Place_category, 
                                  levels = c("Domestic", "Village","Buffer zone", "Nature areas"))
  ) %>%
  ggplot(aes(y = Animal_category, fill = Place_category)) +
  geom_bar(position = position_fill(reverse = TRUE), width = 0.7) + 
  scale_fill_manual(values = c(
    "Buffer zone" = "#67a9cf",
    "Nature areas" = "#1b7837",
    "Village" = "#8c510a", 
    "Domestic" = "#bf812d"
  )) +
  scale_x_continuous(labels = scales::percent_format(), expand = c(0,0)) +
  labs(
    title = "Location Profile by Animal Category",
    x = "Proportion of observations", 
    y = NULL, 
    fill = "Place"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial Narrow"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black", hjust = 0.5),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black"),
    plot.title = element_text(color = "black", face = "bold", size = 14, hjust = 0.5),
    
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
ggsave("Places of observation x animal category.png", bg = "transparent")

###ver porcentajes###
tabla_ancha <- tabla_porcentajes %>%
  # Seleccionamos solo las columnas que queremos en la tabla final
  select(Animal_category, Place_category, porcentaje) %>%
  # Hacemos que cada "Place_category" sea una nueva columna
  pivot_wider(
    names_from = Place_category, 
    values_from = porcentaje,
    values_fill = "0%" # Rellena con 0% si un animal no tiene registros en ese lugar
  )
print(tabla_ancha)

#obsolete
database_ %>%
  count(Place_category) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = reorder(Place_category, -prop), y = prop)) +
  geom_bar(stat = "identity", fill = "#8FBC8F") +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            vjust = -0.3, size = 4, family = "Arial Narrow") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(x = "Place of observation", y = "% of observations") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial Narrow"),
    axis.text.x = element_text(hjust = 0.5, size = 12),
    panel.grid.major.y =  element_line(color = "gray90"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
  )
#animal x place: Where are different animals being encountered?
animal_place_prop <- database_ %>%
  count(Animal_category, Place_category) %>%
  group_by(Animal_category) %>%
  mutate(prop = n / sum(n))
ggplot(animal_place_prop, aes(x = Place_category, y = Animal_category, fill = prop)) +
  geom_tile() +
  scale_fill_gradient(low = "#F5F5F5", high = "#698B69", labels = scales::percent) +
  labs(x = "Place of observation", y = "Animal category", fill = "% within animal") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial Narrow"),
    axis.text.x = element_text(hjust = 0.5),
    panel.grid = element_blank()
  )

##Type of contact
interaction_long <- Database %>%
  select(ID_participant, Contact, `By product`, Cooking) %>%
  distinct(ID_participant, .keep_all = TRUE) %>%
  pivot_longer(cols = -ID_participant, names_to = "Interaction", values_to = "Yes") %>%
  filter(Yes == 1)
interaction_long %>%
  count(Interaction) %>%
  mutate(prop = n / n_distinct(interaction_long$ID_participant)) %>%
  ggplot(aes(x = reorder(Interaction, -prop), y = prop)) +
  geom_bar(stat = "identity", fill = "#D95F02") +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            vjust = -0.3, size = 4, family = "Arial Narrow") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(x = "Type of interaction", y = "% of participants") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial Narrow"),
    axis.text.x = element_text(hjust = 0.5, size = 12),
    panel.grid.minor = element_blank()
  )

#interaction x animal cat
contact_animal <- Database %>%
  filter(Contact == 1) %>%
  distinct(ID_participant, Animal_category)

contact_animal %>%
  count(Animal_category) %>%
  mutate(prop = n / n_distinct(Database$ID_participant))

ggplot(contact_animal %>% count(Animal_category) %>%
         mutate(prop = n / n_distinct(risk_clean$ID_participant)),
       aes(x = reorder(Animal_category, -prop), y = prop)) +
  geom_bar(stat = "identity", fill = "#76EEC6") +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            vjust = -0.3, size = 4, family = "Arial Narrow") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(x = "Animal category", y = "% of participants with contact") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial Narrow"),
    axis.text.x = element_text(hjust = 0.5, size = 12),
    panel.grid.minor = element_blank()
  )

#by product x animal category
byprod_animal <- Database %>%
  filter(`By product` == 1) %>%
  distinct(ID_participant, Animal_category)

byprod_summary <- byprod_animal %>%
  count(Animal_category) %>%
  mutate(prop = n / n_distinct(Database$ID_participant))

ggplot(byprod_summary, aes(x = reorder(Animal_category, -prop), y = prop)) +
  geom_bar(stat = "identity", fill = "#76EEC6") +  # verde distinto 👀
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            vjust = -0.3, size = 4, family = "Arial Narrow") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(x = "Animal category", y = "% of participants (by-product exposure)") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial Narrow"),
    axis.text.x = element_text(hjust = 0.5, size = 12),
    panel.grid.minor = element_blank()
  )
##combine graphs
library(dplyr)
library(tidyr)

interaction_all <- Database %>%
  select(ID_participant, Animal_category, Contact, `By product`) %>%
  distinct() %>%
  pivot_longer(
    cols = c(Contact, `By product`),
    names_to = "Interaction",
    values_to = "Yes"
  ) %>%
  filter(Yes == 1)
interaction_summary <- interaction_all %>%
  distinct(ID_participant, Animal_category, Interaction) %>%
  count(Animal_category, Interaction) %>%
  mutate(prop = n / n_distinct(risk_clean$ID_participant))
order_contact <- interaction_summary %>%
  filter(Interaction == "Contact") %>%
  arrange(desc(prop)) %>%
  pull(Animal_category)
interaction_summary$Animal_category <- factor(
  interaction_summary$Animal_category,
  levels = order_contact
)

ggplot(interaction_summary,
       aes(x = Animal_category, y = prop, fill = Interaction)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3.5, family = "Arial Narrow") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = c(
    "Contact" = "#C1FFC1",
    "By product" = "#528B8B"
  )) +
  labs(title = "Interaction types by animal category",
    x = "Animal category",
       y = "% of participants",
       fill = "Interaction type") +
  theme_minimal() +
 theme(
  text = element_text(family = "Arial Narrow"),
  plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
  axis.text.x = element_text(size = 11),
  panel.grid.minor = element_blank(),
  panel.grid.major.y =  element_blank(),
  panel.grid.major.x = element_blank()
)

#improved type of interaction x animal category
interaction_data <- data.frame(
  Category = c("Domestic animals", "Flies and ticks", "Rodents", 
               "Reptiles and amphibians", "Primates", "Other wild mammals", "Bats"),
  Contact = c(67, 18, 2, 10, 8, 2, 1),
  By_product = c(33, 17, 11, 9, 4, 2, 1)
)

interaction_data %>%
  mutate(Category = reorder(Category, Contact)) %>% # Ordenamos por contacto
  ggplot(aes(y = Category)) +
  # Línea que une los dos puntos
  geom_segment(aes(x = By_product, xend = Contact, yend = Category), 
               color = "#e0e0e0", size = 1.5) +
  # Punto para 'By product' (Verde azulado/café)
  geom_point(aes(x = By_product, color = "By product"), size = 4) +
  # Punto para 'Contact' (Verde brillante)
  geom_point(aes(x = Contact, color = "Contact"), size = 4) +
  # Etiquetas de texto para los valores
  geom_text(aes(x = Contact, label = paste0(Contact, "%")), 
            hjust = -0.5, family = "Arial Narrow", fontface = "bold", size = 3.5) +
  geom_text(aes(x = By_product, label = paste0(By_product, "%")), 
            vjust = -1.2, family = "Arial Narrow", size = 3, color = "grey40", fontface = "bold") +
  # Colores con paleta de manglar
  scale_color_manual(values = c(
    "Contact" = "#1b7837",    # Verde fuerte
    "By product" = "#8c510a"   # Azul/Grisáceo (By-product)
  )) +
  scale_x_continuous(limits = c(0, 80), breaks = seq(0, 100, 25), labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Interaction Types by Animal Category",
    subtitle = "Direct contact vs. indirect (by-product) interactions",
    x = "Percentage of participants",
    y = NULL,
    color = "Interaction type"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial Narrow"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    axis.text.y = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.3),
    plot.subtitle = element_text(hjust = 0.3)
  )
ggsave("Interaction x animal category.png", bg = "transparent")






