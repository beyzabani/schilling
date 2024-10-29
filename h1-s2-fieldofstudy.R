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
  select(Austrittsjahr, Jahrgang, Gender, Sample, Stellenantritt, Nationality, Nationality_co, Branche, 
         PriorPosition,	PriorWechselUnternehmen,	prior_code, tenure_prior, prior_position_code, 
         prior_status, schilling_status, highest_degree, ausbildungsort, fieldofstudy) %>%
  mutate_all(as.character) %>%
  mutate_all(~ recode(., "NA" = NA_character_, "N/A" = NA_character_))

# Add new variables
#df <- df %>%
 # drop_na(Stellenantritt) %>%
  #mutate(tenure = as.numeric(Austrittsjahr) - as.numeric(Stellenantritt))

# Sample 1 ----------------------------------------------------------------
df_field <- df %>%
  filter(Sample == 0 | Sample == 1) %>%
  drop_na(Nationality_co, fieldofstudy) %>%
  mutate(
    field = case_when(
      fieldofstudy %in% c(0, 1, 2, 3, 4, 5) ~ "Economics and Management",
      fieldofstudy %in% c(6, 7, 8) ~ "STEM",
      fieldofstudy %in% c(9, 10, 11, 12) ~ "Other",
      TRUE ~ NA_character_
    ),
    Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male")),
    Nationality_co = factor(Nationality_co, levels = c(0, 1), labels = c("Schweiz", "Foreign"))
  ) %>%
  group_by(Gender, Nationality_co, field) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  group_by(Gender, Nationality_co) %>%
  mutate(Percentage = round(100 * Count / sum(Count), 1)) %>%
  mutate(Label = paste0(Percentage, "% (", Count, ")"))

# Plotting
plot1 <- ggplot(df_field, aes(x = Gender, y = Percentage, fill = field)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_wrap(~ Nationality_co) +
  geom_text(aes(label = Label), position = position_dodge(width = 0.8), vjust = -0.5) +
  labs(x = "Gender", y = "Percentage (%)", fill = "Field of Study") +
  ggtitle("Sample 1: Distribution of Fields of Study by Gender and Nationality") +  # Adding title here
  theme_minimal() +  theme(legend.title = element_text())

plot1

# Sample 2 ----------------------------------------------------------------
df_field2 <- df %>%
  filter(Sample == 0 | Sample == 2) %>%
  drop_na(Nationality_co, fieldofstudy) %>%
  mutate(
    field = case_when(
      fieldofstudy %in% c(0, 1, 2, 3, 4, 5) ~ "Economics and Management",
      fieldofstudy %in% c(6, 7, 8) ~ "STEM",
      fieldofstudy %in% c(9, 10, 11, 12) ~ "Other",
      TRUE ~ NA_character_
    ),
    Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male")),
    Nationality_co = factor(Nationality_co, levels = c(0, 1), labels = c("Schweiz", "Foreign"))
  ) %>%
  group_by(Gender, Nationality_co, field) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  group_by(Gender, Nationality_co) %>%
  mutate(Percentage = round(100 * Count / sum(Count), 1)) %>%
  mutate(Label = paste0(Percentage, "% (", Count, ")"))

# Plotting
plot2 <- ggplot(df_field2, aes(x = Gender, y = Percentage, fill = field)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_wrap(~ Nationality_co) +
  geom_text(aes(label = Label), position = position_dodge(width = 0.8), vjust = -0.5) +
  labs(x = "Gender", y = "Percentage (%)", fill = "Field of Study") +
  ggtitle("Sample 2: Distribution of Fields of Study by Gender and Nationality") +  # Adding title here
  theme_minimal() +  theme(legend.title = element_text())

plot2


# Sample All ----------------------------------------------------------------
df_field_all <- df %>%
  drop_na(Nationality_co, fieldofstudy) %>%
  mutate(
    field = case_when(
      fieldofstudy %in% c(0, 1, 2, 3, 4, 5) ~ "Economics and Management",
      fieldofstudy %in% c(6, 7, 8) ~ "STEM",
      fieldofstudy %in% c(9, 10, 11, 12) ~ "Other",
      TRUE ~ NA_character_
    ),
    Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male")),
    Nationality_co = factor(Nationality_co, levels = c(0, 1), labels = c("Schweiz", "Foreign"))
  ) %>%
  group_by(Gender, Nationality_co, field) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  group_by(Gender, Nationality_co) %>%
  mutate(Percentage = round(100 * Count / sum(Count), 1)) %>%
  mutate(Label = paste0(Percentage, "% (", Count, ")"))

# Plotting
plot_all <- ggplot(df_field_all, aes(x = Gender, y = Percentage, fill = field)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_wrap(~ Nationality_co) +
  geom_text(aes(label = Label), position = position_dodge(width = 0.8), vjust = -0.5) +
  labs(x = "Gender", y = "Percentage (%)", fill = "Field of Study") +
  ggtitle("Sample All: Distribution of Fields of Study by Gender and Nationality") +  # Adding title here
  theme_minimal() +  theme(legend.title = element_text())

plot_all