# Load necessary libraries ------------------------------------------------
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra) 
library(knitr)
library(kableExtra)
library(broom)
library(knitr)
library(stargazer)
library(rio)
library(lmtest)
library(sandwich)

# Read the data -----------------------------------------------------------
df <- read_excel("C:/Users/User/Desktop/osterloh/schilling/data.xlsx")

# cleaning ----------------------------------------------------------------

df <- df[-1, ]%>% 
  select(Austrittsjahr, Jahrgang, Gender, Sample, Stellenantritt, Nationality, Nationality_co, 
         Branche, PriorPosition,	PriorWechselUnternehmen,	prior_code, tenure_prior, 
         prior_position_code, prior_status, schilling_status, highest_degree, 
         ausbildungsort, fieldofstudy) %>%
  mutate_all(as.character) %>%
  mutate_all(~ recode(., "NA" = NA_character_, "N/A" = NA_character_))




# Sample 1 ----------------------------------------------------------------


df_lang <- df %>% 
  filter(Sample == 0 | Sample == 1) %>%
  drop_na(Nationality, Nationality_co, Stellenantritt) %>% 
  mutate(tenure = as.numeric(Austrittsjahr) - as.numeric(Stellenantritt)) %>% 
  mutate(Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male"))) %>% 
           mutate(language = case_when(
             Nationality %in% c("Schweiz", "Osterreich", "Deutschland", 
                                "China / Deutschland", "Schweiz / Brazil") ~ "German",
             Nationality %in% c("Frankreich", "Belgien") ~ "French",
             Nationality %in% c("USA", "Grossbritannien", "Australien", "Irland", "Kanada", 
                                "Singapur", "Philippinen / USA") ~ "English",
             TRUE ~ "Other" )) %>% 
    mutate(tenure_group = case_when(
    tenure >= 1 & tenure <= 3 ~ "1-3 years",
    tenure >= 4 & tenure <= 6 ~ "4-6 years",
    tenure >= 7 & tenure <= 9 ~ "7-9 years",
    tenure >= 10 & tenure <= 12 ~ "10-12 years",
    tenure >= 13 & tenure <= 27 ~ "13+years")) %>% 
  mutate(tenure_group = factor(tenure_group, levels = c("1-3 years", "4-6 years", 
                                                        "7-9 years", "10-12 years", "13+years")))
         
         


# Creating tenure_group as a factor with specified levels in the correct order
df_lang <- df_lang %>%
  mutate(tenure_group = case_when(
    tenure >= 1 & tenure <= 3 ~ "1-3 years",
    tenure >= 4 & tenure <= 6 ~ "4-6 years",
    tenure >= 7 & tenure <= 9 ~ "7-9 years",
    tenure >= 10 & tenure <= 12 ~ "10-12 years",
    tenure >= 13 & tenure <= 27 ~ "13+years"
  )) %>%
  mutate(tenure_group = factor(tenure_group, levels = c("1-3 years", "4-6 years", "7-9 years", "10-12 years", "13+years")))

# Calculating percentages for the plot by language, gender, and tenure group
df_lang_perc <- df_lang %>%
  group_by(language, Gender, tenure_group) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Creating the stacked bar plot with percentages for language, gender, and tenure group
ggplot(df_lang_perc, aes(x = interaction(language, Gender), y = percentage, fill = tenure_group)) + 
  geom_bar(stat = "identity", position = "stack") + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 3) +  # Adding percentage labels
  labs(
    title = "Distribution of Tenure Groups by Language and Gender",
    x = "Language and Gender",
    y = "Percentage",
    fill = "Tenure Group"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +  # Adjusting color palette for tenure groups
  scale_fill_manual(values = c("Female" = "#FF3333", "Male" = "#3333FF")) +  # Red for women, blue for men
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )






