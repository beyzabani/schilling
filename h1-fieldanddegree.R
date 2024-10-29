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
  select(Austrittsjahr, Jahrgang, Gender, Sample, Stellenantritt, Nationality, Nationality_co, Branche, PriorPosition,	PriorWechselUnternehmen,	prior_code, tenure_prior, prior_position_code, prior_status, schilling_status, highest_degree, ausbildungsort, fieldofstudy) %>%
  mutate_all(as.character) %>%
  mutate_all(~ recode(., "NA" = NA_character_, "N/A" = NA_character_))

# Add new variables
df <- df %>%
  drop_na(Stellenantritt) %>%
  mutate(tenure = as.numeric(Austrittsjahr) - as.numeric(Stellenantritt))


df_field <- df %>% 
  filter(Sample == 0 | Sample == 1) %>%
  drop_na(fieldofstudy) %>% 
  mutate(field = case_when(
    fieldofstudy %in% c(0, 1, 2, 3, 4, 5) ~ "Economics and Management",
    fieldofstudy %in% c(6, 7, 8) ~ "STEM",
    fieldofstudy %in% c(9, 10, 11, 12) ~ "Other",
    TRUE ~ NA_character_
  )) %>% 
  mutate(Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male")))



# Summary statistics by Gender and Field of Study, including count
summary_stats_field <- df_field %>%
  group_by(Gender, field) %>%
  summarize(
    count = n(),  # Count of observations
    mean_tenure = round(mean(tenure, na.rm = TRUE), 1),
    sd_tenure = round(sd(tenure, na.rm = TRUE), 1)
  )

# Summary statistics table with count
summary_stats_field %>%
  kable(
    caption = "Sample 1: Average Tenure by Gender and Field of Study with Count",
    col.names = c("Gender", "Field of Study", "Count", "Average Tenure", "SD Tenure"),
    align = "c"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  )

