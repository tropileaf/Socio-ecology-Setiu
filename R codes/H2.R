theme_tesis <- theme_minimal(base_family = "Arial Narrow") +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11, color = "black"),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),,
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
colores_base <- c(
  azul = "#0072B2",
  azul_claro = "#56B4E9",
  verde = "#009E73",
  amarillo = "#F0E442",
  naranja = "#E69F00",
  rojo = "#D55E00",
  gris = "#999999"
)
################################################
install.packages("MASS")
install.packages("brant")
install.packages("ordinal")
install.packages("sjPlot")
install.packages("glmmTMB")
install.packages("readxl")
install.packages("corrplot")
install.packages("FSA")
install.packages("lme4")
install.packages("broom.mixed")
library(broom.mixed)
library(lme4)
library(FSA)
library(corrplot)
library(readxl)
library(glmmTMB)
library(performance)
library(sjPlot)
library(ordinal)
library(MASS)
library(brant)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(patchwork)

unique(df$Time_category)
levels(df$Time_category)
table(df$Occupation)
table(df$Age_range)
table(df$Gender)
table(df$Nationality)

##DEMOGRAPHICS X CONTACT TIME##
df <- read_xlsx("/Users/marianagomez/Downloads/Database.xlsx")
df[df == "n/a"] <- NA
df$Time_category <- factor(df$Time_category, 
                           levels = c("No contact", "Very low", "Low", "Medium", "High", "Constant"), 
                           ordered = TRUE)
df$Occupation <- as.factor(df$Occupation)
df$Age_range <- as.factor(df$Age_range)
df$Gender <- as.factor(df$Gender)
df$ID_participant <- as.factor(df$ID_participant)

#visitors as reference
df$Occupation <- relevel(df$Occupation, ref = "Visitor")
df$Age_range <- relevel (df$Age_range, ref = "20-29")
modelo_test <- clmm(Time_category ~ Occupation + Age_range + Gender + (1|ID_participant), 
                    data = df)
summary(modelo_test)

tabla_final <- as.data.frame(coef(summary(modelo_test)))
tabla_final$OR <- exp(tabla_final$Estimate)
tabla_final$Lower_CI <- exp(tabla_final$Estimate - 1.96 * tabla_final$`Std. Error`)
tabla_final$Upper_CI <- exp(tabla_final$Estimate + 1.96 * tabla_final$`Std. Error`)

#asumption of normality
ranef_data <- as.data.frame(ranef(modelo_test))
qqnorm(ranef_data[,1])
qqline(ranef_data[,1], col = "red")
#asumption of collinearity
check_collinearity(modelo_test)

#GRAPH
occupation_results <- data.frame(
  Occupation = c(
    "Fishing and related activities",
    "Homemakers",
    "Local students",
    "Other employment",
    "Wetland management and tourism"), 
  Estimate = c(
    1.24353,
    1.00196,
    -0.92855,
    0.86344,
    0.49062 ),
  SE = c(
    0.53214,
    0.53367,
    0.79513,
    0.50236,
    0.53920),
  pvalue = c(
    0.0194,
    0.0604,
    0.2429,
    0.0857,
    0.3629
  )
)

occupation_results <- occupation_results %>%
  mutate(
    OR = exp(Estimate),
    LowerCI = exp(Estimate - 1.96 * SE),
    UpperCI = exp(Estimate + 1.96 * SE),
    
    significance = case_when(
      pvalue < 0.001 ~ "***",
      pvalue < 0.01  ~ "**",
      pvalue < 0.05  ~ "*",
      pvalue < 0.1   ~ ".",
      TRUE ~ ""
    ))
occupation_results$Occupation <- factor(
  occupation_results$Occupation,
  levels = rev(occupation_results$Occupation))

#plot
ggplot(occupation_results, aes(x = OR, y = Occupation)) +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_errorbarh(aes(xmin = LowerCI, xmax = UpperCI),
                 height = 0.2,
                 linewidth = 0.8,
                 color = "#0072B2") +
  geom_point(size = 3,
             color = "#0072B2") +
  geom_text(aes(label = significance,
                x = UpperCI * 1.15),
            size = 7,
            color = "#0072B2") +
  scale_x_log10() +
  labs(
    x = "Odds ratio (reference = Visitors)",
    y = NULL
  ) +
  theme_minimal(base_family = "Arial Narrow") +
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
         plot.background = element_rect(fill = "transparent", color = NA),
         panel.grid.major.x = element_line(
           color = "grey80",
           linetype = "dashed"
         ),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.y = element_blank()
         )
ggsave( "Occupation x time contactGOOD.png", dpi = 600, bg = "transparent")

##DEMOGRAPHICS X TYPE OF INTERACTION##
#contact - GLMM
your_data_clean$Functional_Group <- as.factor(your_data_clean$Functional_Group)
your_data_clean$Functional_Group <- relevel(
  your_data_clean$Functional_Group,
  ref = "Domestic animals")
your_data_clean$Occupation <- relevel(
  your_data_clean$Occupation,
  ref = "Visitor"
)
your_model <- glmer(
  Contact ~ Occupation + Age_range + Gender +
    Functional_Group +
    (1 | ID_participant),
  family = binomial,
  data = your_data_clean,
  control = glmerControl(optimizer = "bobyqa"))
summary(your_model)

#by product - GLMM
names(your_data_clean)[names(your_data_clean) == "By product"] <- "By_product"
data_byprod <- your_data_clean %>%
  dplyr::select(
    By_product,
    Occupation,
    Age_range,
    Gender,
    Functional_Group,
    ID_participant
  ) %>%
  droplevels()

data_byprod$Functional_Group <- as.factor(data_byprod$Functional_Group)
data_byprod$Functional_Group <- relevel(
  data_byprod$Functional_Group,
  ref = "Domestic animals"
)
data_byprod$Occupation <- relevel(
  data_byprod$Occupation,
  ref = "Visitor"
)
data_byprod <- data_byprod %>%
  filter(!is.na(By_product))

data_byprod$By_product <- as.numeric(as.character(data_byprod$By_product))

model_byprod <- glmer(
  By_product ~ Occupation + Age_range + Gender +
    Functional_Group +
    (1 | ID_participant),
  family = binomial,
  data = data_byprod,
  control = glmerControl(optimizer = "bobyqa")
)
summary(model_byprod)

#Graph
prep_forest <- function(model, label, keep_groups) {
  tidy(model, effects = "fixed", conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      OR = exp(estimate),
      conf.low = exp(conf.low),
      conf.high = exp(conf.high),
      group = case_when(
        str_detect(term, "Occupation") ~ "Occupation",
        str_detect(term, "Functional_Group") ~ "Functional group",
        TRUE ~ "Other"
      )
    ) %>%
    filter(group %in% keep_groups) %>%
    mutate(
      term_clean = term %>%
        str_replace("Occupation", "") %>%
        str_replace("Functional_Group", "") %>%
        str_replace_all("_", " "),
      panel = label,
      signif = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        TRUE ~ ""
      )
    )
}

# Panel A: only functional groups
res_contact <- prep_forest(
  your_model,
  "A. Direct contact",
  keep_groups = c("Functional group")
)
# Panel B: occupation + functional groups
res_byprod <- prep_forest(
  model_byprod,
  "B. By-product exposure",
  keep_groups = c("Occupation", "Functional group")
)
results_all <- bind_rows(res_contact, res_byprod)
desired_order <- c(
  "Mangrove-associated fauna",
  "Other primates",
  "Other mammals",
  "Other bats",
  "Other rodents and shrews",
  "Other reptiles and amphibians",
  "Ticks and flies",
  
  "Fishing and related activities",
  "Homemakers",
  "Local students",
  "Other employment",
  "Wetland management and tourism")

results_all$term_clean <- factor(
  results_all$term_clean,
  levels = rev(desired_order))

ggplot(results_all,
       aes(x = OR, y = term_clean)) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_errorbarh(
    aes(xmin = conf.low,
        xmax = conf.high,
        color = group),
    height = 0.18,
    linewidth = 0.5
  ) +
geom_point(
  aes(color = group),
  size = 2.7
) +
  geom_text(
    aes(label = signif,
        x = conf.high * 1.35,
        color = group),
    size = 4.5,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c(
    "Functional group" = "#009E73",
    "Occupation" = "#0072B2"
  )) +
  facet_wrap(
    ~ panel,
    ncol = 2,
    scales = "free_y"
  ) +
  scale_x_log10(
    breaks = c(0.01, 0.1, 1, 10, 100),
    labels = c("0.01", "0.1", "1", "10", "100")
  ) +
  labs(
    x = "Odds ratio (log scale)",
    y = NULL,
    caption = "Reference groups: Visitors, Domestic animals, Females, 20–29 years"
  ) +
  theme_minimal(base_family = "Arial Narrow") +
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
         plot.background = element_rect(fill = "transparent", color = NA),
         panel.grid.major.x = element_line(
           color = "grey80",
           linetype = "dashed"
         ),
         panel.grid.minor.x = element_blank(),
         panel.grid.major.y = element_blank(),
         legend.position = "",
         strip.text = element_text(
           size = 16,
           face = "bold"
         ),
         plot.caption = element_text(
           hjust = 0.5,
           face = "italic",
           size = 12
         ))
ggsave("forest_panelsGOOD.png",bg = "transparent")

##DEMOGRAPHICS X PERCEPTION##
#couldn't use a single model for all 5 categories
df_perc <- Database %>%
  dplyr::select(
    ID_participant,
    Occupation,
    Age_range,
    Gender,
    Perception
  ) %>%
  filter(
    !is.na(Perception),
    !is.na(Occupation),
    !is.na(Age_range),
    !is.na(Gender)
  ) %>%
  mutate(
    Occupation = relevel(factor(Occupation), ref = "Visitor"),
    Age_range = relevel(factor(Age_range), ref = "20-29"),
    Gender = relevel(factor(Gender), ref = "Female"),
    
    is_pest = ifelse(Perception == "Pests", 1, 0),
    is_dangerous = ifelse(Perception == "Dangerous", 1, 0),
    is_friendly = ifelse(Perception == "Friendly", 1, 0),
    is_not_dangerous = ifelse(Perception == "Not dangerous",1,0),
    is_neutral = ifelse(Perception == "Neutral",1,0)
    )

modelo_pests <- glmmTMB(is_pest ~ Occupation + Age_range + Gender + 
                          (1|ID_participant), data = df_perc, 
                           family = binomial)
summary(modelo_pests)

modelo_dangerous <- glmmTMB(is_dangerous ~ Occupation + Age_range + Gender + 
                              (1|ID_participant), data = df_perc, 
                               family = binomial)
summary(modelo_dangerous)

modelo_friendly <- glmmTMB(is_friendly ~ Occupation + Age_range + Gender + 
                             (1|ID_participant),  data = df_perc, 
                           family = binomial)
summary(modelo_friendly)

modelo_not_dangerous <- glmmTMB(is_not_dangerous ~ Occupation + Age_range + 
                                  Gender + (1|ID_participant), data = df_perc, 
                                family = binomial)
summary(modelo_not_dangerous)

modelo_neutral <- glmmTMB(is_neutral ~ Occupation + Age_range + Gender + 
                            (1|ID_participant), data = df_perc, 
                          family = binomial)
summary(modelo_neutral)

#CONCLUSION GRAPH
mi_paleta_accesible <- c(
  "Friendly" = "#3374B0",
  "Not dangerous" = "#9ED3DF",
  "Pest" = "#77AB76",
  "Dangerous" = "#E7D674")
grafico_final_limpio <- 
  plot_models(
    modelo_dangerous,
    modelo_pests,
    modelo_friendly,
    modelo_not_dangerous,
    m.labels = c(
      "Dangerous",
      "Pest",
      "Friendly",
      "Not dangerous"
    ),
    colors = mi_paleta_accesible,
    rm.terms = c(
      "Age_range10-19",
      "Age_range30-39",
      "Age_range40-49",
      "Age_range50+",
      "GenderMale"
    ),
    spacing = 0.45,
    p.shape = FALSE,
    show.values = FALSE,
    show.p = FALSE,
    axis.lim = c(0.2, 8)
  )

plot_data <- grafico_final_limpio$data

plot_data$signif <- case_when(
  plot_data$p.value < 0.001 ~ "***",
  plot_data$p.value < 0.01 ~ "**",
  plot_data$p.value < 0.05 ~ "*",
  TRUE ~ ""
)

plot_data <- plot_data %>%
  filter(signif != "")

perception_complete <- 
  grafico_final_limpio +
  scale_y_log10(
    limits = c(0.2, 8),
    breaks = c(
      0.2,
      0.5,
      1,
      2,
      5
    ),
    labels = c(
      "0.2",
      "0.5",
      "1",
      "2",
      "5"
    ),
    oob = scales::squish
  ) +
  geom_text(
    data = plot_data,
    aes(
      y = estimate *1.1,
      x = as.numeric(term) + 0.25,
      label = signif,
      color = group
    ),
    size = 6,
    show.legend = FALSE
  ) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "gray40",
    linewidth = 0.5
  ) +
  labs(
    title = "",
    x = "",
    y = "Odds ratio (log scale)",
    caption = "Reference groups: Female | Visitor | 20–29 years"
  ) +
  theme_minimal(
    base_family = "Arial Narrow"
  ) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 1, b = 1),
    legend.box.margin = margin(t = 1, b = 1),
    
    plot.caption = element_text(
      hjust = 0.5,
      size = 12,
      face = "italic",
      margin = margin(t =1)
    ) ,
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(
      size = 14,
      color = "black"
    ),
    axis.text.y = element_text(
      size = 13,
      color = "black"
    ),
    axis.title = element_text(
      size = 15,
      color = "black"
    ),
    panel.grid.minor = element_blank(),
    plot.margin = margin(0, 0, 0, 0))
ggsave("Demographic x perceptionGOOD2.png", dpi = 600)

## RISK X DEMOGRAPHICS
df_risks_clean <- Database %>%
  dplyr::select(
    ID_participant,
    Occupation,
    Age_range,
    Gender,
    Risk
  ) %>%
  filter(
    !is.na(Risk),
    !is.na(Occupation),
    !is.na(Age_range),
    !is.na(Gender)
  ) %>%
  mutate(
    Occupation = relevel(factor(Occupation), ref = "Visitor"),
    Age_range = relevel(factor(Age_range), ref = "20-29"),
    Gender = relevel(factor(Gender), ref = "Female"),
    
    is_none = ifelse(Risk == "None", 1, 0),
    is_other = ifelse(Risk == "Other", 1, 0),
    is_toxic = ifelse(Risk == "Toxic", 1, 0),
    is_nuisance = ifelse(Risk == "Nuisance",1,0),
    is_disease = ifelse(Risk == "Disease",1,0),
    is_physicalharm = ifelse(Risk == "Physical harm", 1,0)
  )

df_risks_clean$Risk <- sapply(
  strsplit(as.character(df_risks_clean$Risk), ";"),
  function(x) trimws(x[1]))
df_risks_clean <- df_risks_clean %>%
  filter(!is.na(Risk))
tabla_riesgos <- table(df_risks_clean$Occupation, df_risks_clean$Risk)

fisher_resultado <- fisher.test(tabla_riesgos, simulate.p.value = TRUE)
print(fisher_resultado)
residuos_matriz <- chisq.test(tabla_riesgos)$residuals

#plot
residuals_df <- as.data.frame(as.table(residuos_matriz))
colnames(residuals_df) <- c(
  "Occupation",
  "Risk",
  "Residual"
)

residuals_plot <- ggplot(
  residuals_df,
  aes(
    x = Risk,
    y = Occupation,
    fill = Residual
  )
) +
  geom_tile(
    color = "white",
    linewidth = 0.8
  ) +
  geom_text(
    aes(label = round(Residual, 2)),
    size = 3.5,
    family = "Arial Narrow",
    fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "#A6CEE3",
    mid = "white",
    high = "#1A237E",
    midpoint = 0,
    name = "Pearson residual"
  ) +
  scale_y_discrete(
    limits = rev(c(
      "Visitor",
      "Fishing and related activities",
      "Homemakers",
      "Local students",
      "Other employment",
      "Wetland management and tourism"
    ))
  ) +
  theme_minimal(
    base_family = "Arial Narrow"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(,
      hjust = 0.5,
      size = 10,
      color = "black"
    ),
    axis.text.y = element_text(
      size = 13,
      color = "black"
    ),
    legend.position = "right",
    plot.margin = margin(1,1,1,1)
  ) +
  
  labs(
    x = NULL,
    y = NULL)
ggsave("Residuals.png")

combined_plot <- perception_complete / residuals_plot  +
  plot_layout(heights = c(2, 1)) &
  theme(
    plot.margin = margin(1, 1, 1, 1)  )
ggsave("Figure6GOOD.png", dpi = 600)


