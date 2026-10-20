## SYNANTHROPIC VS RARE ANALYSIS ## H1
library(readxl)
library(FSA)
library(dplyr)
library(ggplot2)
library(ggimage)
library(ordinal)
library(emmeans)

Database <- read_excel("/Users/marianagomez/Downloads/Database.xlsx")
Database
# categories x IDs
domestic_ids <- c(1, 2, 3, 4)
syn_ids <- c(
  7, 11, 8, 21, 29, 39, 40,
  25, 27, 60, 59, 50, 52, 55
)
fac_ids <- c(
  5, 6, 34, 14, 33, 38, 44,
  45, 18, 47, 48, 46, 23,
  58, 53, 54
)
sylvatic_ids <- c(
  9, 13, 15, 43, 42, 19, 20,
  28, 30, 31, 32, 35, 51,
  24, 36, 37, 41, 49
)

niveles_tiempo <- c("No contact", "Very low", "Low", "Medium", "High", "Constant")
df_obj1 <- df_obj1 %>%
  mutate(
    Time_category2 = case_when(
      
      Time_category %in% c("Medium", "High", "Constant") ~ "High",

      TRUE ~ as.character(Time_category)
    ),

    Time_category2 = factor(
      Time_category2,
      levels = c(
        "No contact",
        "Very low",
        "Low",
        "High"
      ),
      ordered = TRUE
    )
  )
species_counts <- c(
  "Domestic" = 4,
  "Synanthropic" = 14,
  "Facultatively synanthropic" = 16,
  "Sylvatic" = 18
)
mentions_per_person_std <- mentions_per_person %>%
  mutate(
    Species_Count = species_counts[Human_Association],
    
    Mean_Mentions_Per_Species =
      Mentions / Species_Count
  )

##Analysis A: contact time x synan category - CLMM
df_obj1 <- Database %>%
  mutate(
    Human_Association = case_when(
      ID_animal %in% domestic_ids ~ "Domestic",
      ID_animal %in% syn_ids ~ "Synanthropic",
      ID_animal %in% fac_ids ~ "Facultatively synanthropic",
      ID_animal %in% sylvatic_ids ~ "Sylvatic",
      TRUE ~ NA_character_
    ),
    Human_Association = factor(
      Human_Association,
      levels = c(
        "Domestic",
        "Synanthropic",
        "Facultatively synanthropic",
        "Sylvatic"
      )
    ),
    
    Time_category2 = case_when(
      Time_category %in% c("Medium", "High") ~ "High",
      TRUE ~ as.character(Time_category)
    ),
    
    Time_category2 = factor(
      Time_category2,
      levels = c(
        "No contact",
        "Very low",
        "Low",
        "High",
        "Constant"
      ),
      ordered = TRUE
    )
  ) %>% 
  filter(!is.na(Human_Association))

model_h1 <- clmm(
  Time_category2 ~ Human_Association +
    (1 | ID_participant),
  data = df_obj1)
summary(model_h1)
#post hoc
emmeans_h1 <- emmeans(
  model_h1,
  pairwise ~ Human_Association,
  adjust = "BH"
)
emmeans_h1

##Analysis B: Frequency of mentions x synan category
df_mentions <- Database %>%
  mutate(
    Human_Association = case_when(
      ID_animal %in% domestic_ids ~ "Domestic",
      ID_animal %in% syn_ids ~ "Synanthropic",
      ID_animal %in% fac_ids ~ "Facultatively synanthropic",
      ID_animal %in% sylvatic_ids ~ "Sylvatic",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Human_Association))
mentions_per_person <- df_mentions %>%
  group_by(ID_participant, Human_Association) %>%
  summarise(
    Mentions = n(),
    .groups = "drop")

res_mentions_std <- kruskal.test(
  Mean_Mentions_Per_Species ~ Human_Association,
  data = mentions_per_person_std
)
print(res_mentions_std)

posthoc_mentions_std <- dunnTest(
  Mean_Mentions_Per_Species ~ Human_Association,
  data = mentions_per_person_std,
  method = "bh"
)
print(posthoc_mentions_std)

### GRAPH
mentions_per_person_std$Human_Association <- factor(
  mentions_per_person_std$Human_Association,
  levels = c(
    "Domestic",
    "Synanthropic",
    "Facultatively synanthropic",
    "Sylvatic"
  )
)

icons <- data.frame(
  Human_Association = c(
    "Domestic", "Domestic",
    "Synanthropic", "Synanthropic", "Synanthropic",
    "Facultatively synanthropic", "Facultatively synanthropic",
    "Sylvatic", "Sylvatic"
  ),
  
  image = c(
    "/Users/marianagomez/Desktop/cat.png", "/Users/marianagomez/Desktop/chicken.png",
    "/Users/marianagomez/Desktop/rat.png", "/Users/marianagomez/Desktop/Fly.png", "/Users/marianagomez/Desktop/megaderma.png",
    "/Users/marianagomez/Desktop/macaque.png", "/Users/marianagomez/Desktop/toad.png",
    "/Users/marianagomez/Desktop/bat.png", "/Users/marianagomez/Desktop/snake.png"
  ),
  y = c(
    1.08, 1.07,         # Alturas para Domestic
    1.01, 1.04, 1.08,   # Alturas para Synanthropic
    1.06, 1.01,         # Alturas para Facultatively
    1.04, 1.01          # Alturas para Sylvatic
  ),
  x_pos = c(
    1 - 0.2, 1 + 0.2,               # Domestic (centro es 1, movemos a la izq y der)
    2 - 0.2, 2, 2 + 0.22,            # Synanthropic (centro es 2, izq, centro, der)
    3 - 0.10, 3 + 0.15,               # Facultatively (centro es 3)
    4 - 0.15, 4 + 0.15                # Sylvatic (centro es 4)
  ),
  img_size = c(0.13, 0.10,
               0.09, 0.04, 0.14,
               0.15, 0.06,
               0.16, 0.22)
)

ggplot(mentions_per_person_std,
       aes(x = Human_Association,
           y = Mean_Mentions_Per_Species,
           fill = Human_Association)) +
  
  geom_boxplot(
    width = 0.6,
    outlier.shape = NA,
    alpha = 0.75
  ) +
  
  geom_jitter(
    width = 0.15,
    alpha = 0.35,
    size = 1.8,
    color = "black"
  ) +
  scale_fill_manual(values = c(
    "Domestic" = "#3374B0",
    "Synanthropic" = "#9ED3DF",
    "Facultatively synanthropic" = "#77AB76",
    "Sylvatic" = "#E7D674"
  )) +
  
  geom_image(
    data = subset(icons, Human_Association == "Domestic"),
    aes(x = x_pos, y = y, image = image,size = img_size),
    inherit.aes = FALSE
  ) +
  
  geom_image(
    data = subset(icons, Human_Association == "Synanthropic"),
    aes(x = x_pos, y = y, image = image, size = img_size),
    inherit.aes = FALSE
  ) +
  
  geom_image(
    data = subset(icons, Human_Association == "Facultatively synanthropic"),
    aes(x = x_pos, y = y, image = image, size = img_size),
    inherit.aes = FALSE
  ) +
  
  geom_image(
    data = subset(icons, Human_Association == "Sylvatic"),
    aes(x = x_pos, y = y, image = image,size = img_size),
    inherit.aes = FALSE
  ) +
  
  coord_cartesian(
    ylim = c(0, 1.1),
    clip = "off"
  ) +
  scale_size_identity() +
  labs(
    x = "Human-association category",
    y = "Mean encounter reports"
  ) +
  
  theme_tesis +
  
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 16)
  )

ggsave(
  "Figure3B_mentions2.png",
  width = 9,
  height = 6,
  dpi = 600,
  bg = "transparent"
)



