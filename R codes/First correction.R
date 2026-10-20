Database <- read_excel("/Users/marianagomez/Documents/TROPIMUNDO/Thesis/Database.xlsx")
#New categories
domestic_animals_ids <- c(
  1, 2, 3, 4
)

mangrove_ids <- c(
  5,  
  14,  
  42,  
  44,  
  47,  
  48,  
  23,  
  30,  
  32,  
  24,  
  36,  
  37,  
  49   )
other_primates_ids <- c(
  6, 9, 13)
other_rodents_ids <- c(
  7, 8, 11, 15,
  21, 29, 33, 34, 43,10,12,16,17,22,26)
other_bats_ids <- c(
  38, 39, 40)

other_mammals_ids <- c(
  45, 18, 46, 19, 20)
other_reptiles_ids <- c(
  25, 27, 28, 31,
  35, 41, 51,
  58, 59, 60)
ticks_flies_ids <- c(
  50, 52, 53, 54, 55, 57,56)

Database <- Database %>%
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
    )
  )

###LOCATION PROFILE BY ANIMAL CATEGORY##
Database <- Database %>%
  filter(Place_category!= "n/a")
Database <- Database %>%
  mutate(
    Place_category = recode(
      Place_category,
      "Domestic" = "Domestic",
      "Village" = "Peridomestic",
      "Buffer zone" = "Transitional",
      "Nature areas" = "Sylvatic"
    ))
Database <- Database %>%
  mutate(Place_category = factor(
    Place_category,
    levels = c(
      "Domestic",
      "Peridomestic",
      "Transitional",
      "Sylvatic"
    )
  ))
order_domestic <- Database %>%
  count(Functional_Group, Place_category) %>%
  group_by(Functional_Group) %>%
  mutate(prop = n / sum(n)) %>%
  filter(Place_category == "Domestic") %>%
  arrange(desc(prop)) %>%
  pull(Functional_Group)

Database <- Database %>%
  mutate(
    Functional_Group = factor(
      Functional_Group,
      levels = rev(order_domestic)
    ))
Database %>%
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
ggsave("Places of observation x animal categoryGOOD.png", bg = "transparent")

##PERCEPTIONS OF ANIMALS ACROSS ANIMAL CATEGORY##
#RISK DATA
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
harm_order <- plot_risk %>%
  filter(Risk == "Physical harm") %>%
  arrange((Freq)) %>%
  pull(Functional_Group)
plot_risk$Functional_Group <- factor(
  plot_risk$Functional_Group,
  levels = harm_order
)
plot_risk$Risk <- factor(
  plot_risk$Risk,
  levels = c(
    "None",
    "Other",
    "Toxic",
    "Nuisance",
    "Disease",
    "Physical harm"
  )
)
#RISK PLOT
plot_risks <- ggplot(
  plot_risk,
  aes(
    x = Functional_Group,
    y = Freq,
    fill = Risk
  )
) +
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
    title = "B. Perceived risks across animal categories",
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
  )+
guides(fill = guide_legend(reverse = TRUE))

#PERCEPTION DATA
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
    perception_clean$Perception
  ),
  margin = 1
)
plot_perception_data <- as.data.frame(prop_animal_perception) %>%
  rename(
    Functional_Group = Var1,
    Perception = Var2
  )
danger_order <- plot_perception_data %>%
  filter(Perception == "Dangerous") %>%
  arrange((Freq)) %>%
  pull(Functional_Group)
plot_perception_data$Functional_Group <- factor(
  plot_perception_data$Functional_Group,
  levels = danger_order
)
plot_perception_data$Perception <- factor(
  plot_perception_data$Perception,
  levels = c(
    "Friendly",
    "Not dangerous",
    "Neutral",
    "Pest",
    "Dangerous"
  )
)
#PERCEPTION PLOT
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
    "Dangerous" = "#A50026"
  )) +
  
  labs(
    title = "A. Perceptions across animal categories",
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
##combined##
combined_plot <- plot_perception / plot_risks +
  plot_layout(heights = c(1, 1)) &
  theme(
    plot.margin = margin(5, 8, 5, 8)
  )    
combined_plot
ggsave(
  "Figure1_perception_risk_UPDATED.png",
  combined_plot,
  dpi = 600,
  bg = "transparent")
