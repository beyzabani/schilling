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


df_degree <- df %>% 
  filter(Sample == 0 | Sample == 2) %>%
  drop_na(highest_degree) %>% 
  mutate(Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male")),
         highest_degree = factor(highest_degree, levels = c(1, 2, 3, 4), 
                                 labels = c("Bachelor's degree", "Master's degree/MBA", "PhD", "Other")))



# Summary statistics by Gender and Highest Degree, including count
summary_stats_degree <- df_degree %>%
  group_by(Gender, highest_degree) %>%
  summarize(
    count = n(),  # Count of observations
    mean_tenure = round(mean(tenure, na.rm = TRUE), 1),  # Mean tenure
    sd_tenure = round(sd(tenure, na.rm = TRUE), 1)  # Standard deviation of tenure
  )

# Summary statistics table with count
summary_stats_degree %>%
  kable(
    caption = "Sample 2: Average Tenure by Gender and Highest Degree",
    col.names = c("Gender", "Highest Degree", "Count", "Average Tenure", "SD Tenure"),
    align = "c"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  )

