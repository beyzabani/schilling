# Load necessary libraries
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)
library(knitr)
library(kableExtra)

# Read the data
df <- read_excel("C:/Users/User/Desktop/osterloh/schilling/data.xlsx")

# Clean the data
df <- df[-1, ] %>%
  select(Austrittsjahr, Jahrgang, Gender, Sample, Stellenantritt, Nationality, Nationality_co) %>%
  mutate_all(as.character) %>%
  mutate_all(~ recode(., "NA" = NA_character_, "N/A" = NA_character_))

# Add new variables
df <- df %>%
  drop_na(Stellenantritt) %>%
  mutate(tenure = as.numeric(Austrittsjahr) - as.numeric(Stellenantritt))
#sum(is.na(df$tenure))
# Filter and process data for Sample 2
dfs1 <- df %>%
  filter(Sample == 0 | Sample == 2) %>%
  drop_na(Nationality) %>%
  mutate(Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male")),
         Nationality_co = factor(Nationality_co, levels = c(0, 1), labels = c("Schweiz", "Foreign")))

# Summarize statistics by gender and nationality, including count for Sample 2
summary_stats1 <- dfs1 %>%
  group_by(Gender, Nationality_co) %>%
  summarize(
    count = n(),  # Count of observations
    mean_tenure = round(mean(tenure, na.rm = TRUE), 1),
    sd_tenure = round(sd(tenure, na.rm = TRUE), 1)
  )

# Summary statistics table with count for Sample 2
summary_stats1 %>%
  kable(
    caption = "Sample 2: Average Tenure by Gender and Nationality",
    col.names = c("Gender", "Nationality", "Count", "Average Tenure", "SD Tenure"),
    align = "c"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  )

