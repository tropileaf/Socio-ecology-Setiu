library(readxl)
setwd("~/Downloads")
Database <- read_excel("/Users/marianagomez/Downloads/Database.xlsx")
View(Database)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

###RISK GRAPHS###
risk_long <- Database %>%
  separate_rows(Risk, sep = "; ")

table(risk_long$Risk)
prop.table(table(risk_long$Risk)) * 100

risk_clean <- risk_long %>%
  filter(Risk != "n/a")

prop.table(table(risk_clean$Risk)) * 100

risk_plot <- risk_clean %>%
  count(Risk) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  arrange(percentage)

ggplot(risk_plot, aes(x = reorder(Risk, percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "darkseagreen4") +
  coord_flip() +
  geom_text(aes(label = round(percentage, 1)), hjust = -0.1) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "Perceived risks from wildlife encounters",
    x = "",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial Narrow"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.y =  element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
  )
ggsave("Perceived risks.png", bg = "transparent")

## RISK X ANIMAL CAT ##
prop_animal_risk <- prop.table(
  table(risk_clean$Animal_category, risk_clean$Risk),
  margin = 1
)

plot_risk <- as.data.frame(prop_animal_risk) %>%
  rename(
    Animal_category = Var1,
    Risk = Var2
  )
#ver pocentajes
plot_risk %>%
  filter(Risk == "Disease") %>%
  mutate(Percent = round(Freq * 100, 1)) %>%
  select(Animal_category, Percent)
#fin
animal_order <- c(
  "Flies and ticks",
  "Reptiles and amphibians",
  "Bats",
  "Other wild mammals",
  "Rodents and shrews",
  "Primates",
  "Domestic animals"
)

plot_risk$Animal_category <- factor(
  plot_risk$Animal_category,
  levels = animal_order
)

plot_risk$Risk <- factor(
  plot_risk$Risk,
  levels = c("None", "Nuisance", "Other", "Disease", "Toxic", "Physical harm")
)

plot_risks <- ggplot(plot_risk, aes(x = Animal_category, y = Freq, fill = Risk)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  
  scale_fill_manual(values = c(
    "None" = "#4DBBD5",          # azul más suave
    "Nuisance" = "#B07AA1",      # morado más apagado
    "Other" = "#9E9E9E",         # gris neutro
    "Disease" = "#F39B7F",       # naranja suave (menos agresivo)
    "Toxic" = "#F7DC6F",         # amarillo más claro
    "Physical harm" = "#E64B35"  # rojo más claro pero distinto
  )) +
  
  labs(
    title = "B. Perceived risks across animal categories",
    x = "",
    y = "Proportion (%)"
  ) +
  
  theme_minimal() +
  theme(
    text = element_text(family = "Arial Narrow"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
ggsave("Perceived risks x animal category_final.png", bg = "transparent")

###PERCEPTION GRAPHS###
table(Database$Perception)
perception_long <- Database %>%
  separate_rows(Perception, sep = "; ")

table(perception_long$Perception)
perception_clean <- perception_long %>%
  mutate(Perception = case_when(
    Perception == "Pests" ~ "Pest",
    Perception == "n/a" ~ NA_character_,
    TRUE ~ Perception
  ))
perception_clean <- perception_clean %>%
  filter(!is.na(Perception))
perception_plot <- perception_clean %>%
  count(Perception) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  arrange(percentage)

ggplot(perception_plot, aes(x = reorder(Perception, percentage), y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  geom_text(aes(label = round(percentage, 1)), hjust = -0.1) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "Perceptions of wildlife",
    x = "",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial Narrow"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.y =  element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
  )
ggsave("Perceptions.png", bg = "transparent")

#against animal categories
table(perception_clean$Animal_category, perception_clean$Perception)
prop_animal_perception <- prop.table(
  table(perception_clean$Animal_category, perception_clean$Perception),
  margin = 1
)

animal_order <- c(
  "Flies and ticks",
  "Reptiles and amphibians",
  "Bats",
  "Other wild mammals",
  "Rodents and shrews",
  "Primates",
  "Domestic animals"
)

plot_data <- as.data.frame(prop_animal_perception)
plot_data <- plot_data %>%
  rename(
    Animal_category = Var1,
    Perception = Var2
  )
plot_data$Animal_category <- factor(
  plot_data$Animal_category,
  levels = animal_order
)
plot_data$Perception <- factor(
  plot_data$Perception,
  levels = c("Friendly", "Not dangerous", "Neutral", "Pest", "Dangerous")
)
plot_perception <- 
  ggplot(plot_data, aes(x = Animal_category, y = Freq, fill = Perception)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c(
    "Dangerous" = "#E64B35",      # mismo rojo que "Physical harm"
    "Pest" = "#B07AA1",           # mismo tono que "Nuisance"
    "Neutral" = "#9E9E9E",        # gris consistente
    "Not dangerous" = "#4DBBD5",  # mismo azul que "None"
    "Friendly" = "#00A087"        # verde bonito pero más elegante
  )) +
  labs(
    title = "A. Perception of wildlife across animal categories",
    x = "",
    y = "Proportion (%)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial Narrow"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.y =  element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
  )
ggsave("Perceptions x animal category_final.png", bg = "transparent")

combined_plot <- plot_perception / plot_risks+
  plot_layout(heights = c(1, 1)) &
  theme(plot.margin = margin(2, 5, 2, 5))
ggsave(
  "Figure1_perception_risk.png",
  combined_plot,
  width = 10,
  height = 10,
  dpi = 300,
  bg = "transparent"
)


