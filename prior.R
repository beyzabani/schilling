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
         Branche, PriorPosition,	PriorWechselUnternehmen,	prior_code, tenure_prior, prior_position_code, 
         prior_status, schilling_status, highest_degree, ausbildungsort, fieldofstudy) %>%
  mutate_all(as.character) %>%
  mutate_all(~ recode(., "NA" = NA_character_, "N/A" = NA_character_))


# Sample 1 ----------------------------------------------------------------

df_prior <- df %>% 
  filter(Sample == 0 | Sample == 1) %>%
  drop_na(tenure_prior, Nationality_co) %>% 
  mutate(Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male"))) %>% 
  mutate(Nationality_co = factor(Nationality_co, levels = c(0, 1), labels = c("Schweiz", "Foreign")))
         #mutate(tenure_prior = as.numeric(tenure_prior)) %>%  
          # mutate(tenure_prior = ifelse(tenure_prior == "", NA, tenure_prior)) %>%
           #mutate(tenure_prior_group = factor(case_when(
            # tenure_prior >= 0 & tenure_prior <= 2 ~ "0-2 years",
             #tenure_prior >= 3 & tenure_prior <= 5 ~ "3-5 years",
             #tenure_prior >= 6 & tenure_prior <= 10 ~ "6-10 years",
             #tenure_prior >= 11 & tenure_prior <= 30 ~ "11+ years"),
             #levels = c("0-2 years", "3-5 years", "6-10 years", "11+ years")))


# Assuming df_prior is already cleaned as per your earlier code

# Summary statistics by Gender, Nationality, and Prior Tenure, including count
summary_stats_prior <- df_prior %>%
  group_by(Gender, Nationality_co) %>%
  summarize(
    count = n(),  # Count of observations
    mean_tenure_prior = round(mean(as.numeric(tenure_prior), na.rm = TRUE), 1),  # Mean tenure_prior
    sd_tenure_prior = round(sd(as.numeric(tenure_prior), na.rm = TRUE), 1)  # Standard deviation of tenure_prior
  )

# Summary statistics table with count
summary_stats_prior %>%
  kable(
    caption = "Sample 1: Average Prior Tenure by Gender and Nationality",
    col.names = c("Gender", "Nationality", "Count", "Average Prior Tenure", "SD Prior Tenure"),
    align = "c"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  )


# Sample 2 ----------------------------------------------------------------

df_prior2 <- df %>% 
  filter(Sample == 0 | Sample == 2) %>%
  drop_na(tenure_prior, Nationality_co) %>% 
  mutate(Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male"))) %>% 
  mutate(Nationality_co = factor(Nationality_co, levels = c(0, 1), labels = c("Schweiz", "Foreign")))
#mutate(tenure_prior = as.numeric(tenure_prior)) %>%  
# mutate(tenure_prior = ifelse(tenure_prior == "", NA, tenure_prior)) %>%
#mutate(tenure_prior_group = factor(case_when(
# tenure_prior >= 0 & tenure_prior <= 2 ~ "0-2 years",
#tenure_prior >= 3 & tenure_prior <= 5 ~ "3-5 years",
#tenure_prior >= 6 & tenure_prior <= 10 ~ "6-10 years",
#tenure_prior >= 11 & tenure_prior <= 30 ~ "11+ years"),
#levels = c("0-2 years", "3-5 years", "6-10 years", "11+ years")))


# Assuming df_prior is already cleaned as per your earlier code

# Summary statistics by Gender, Nationality, and Prior Tenure, including count
summary_stats_prior2 <- df_prior2 %>%
  group_by(Gender, Nationality_co) %>%
  summarize(
    count = n(),  # Count of observations
    mean_tenure_prior = round(mean(as.numeric(tenure_prior), na.rm = TRUE), 1),  # Mean tenure_prior
    sd_tenure_prior = round(sd(as.numeric(tenure_prior), na.rm = TRUE), 1)  # Standard deviation of tenure_prior
  )

# Summary statistics table with count
summary_stats_prior2 %>%
  kable(
    caption = "Sample 2: Average Prior Tenure by Gender and Nationality",
    col.names = c("Gender", "Nationality", "Count", "Average Prior Tenure", "SD Prior Tenure"),
    align = "c"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  )


df_foreigners <- df %>% 
  filter(Sample == 0 | Sample == 1) %>%
  drop_na( Nationality_co) %>% 
  mutate(Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male"))) %>% 
  mutate(Nationality_co = factor(Nationality_co, levels = c(0, 1), labels = c("Schweiz", "Foreign")))
#mutate(tenure_prior = as.numeric(tenure_prior)) %>%  
# mutate(tenure_prior = ifelse(tenure_prior == "", NA, tenure_prior)) %>%
#mutate(tenure_prior_group = factor(case_when(
# tenure_prior >= 0 & tenure_prior <= 2 ~ "0-2 years",
#tenure_prior >= 3 & tenure_prior <= 5 ~ "3-5 years",
#tenure_prior >= 6 & tenure_prior <= 10 ~ "6-10 years",
#tenure_prior >= 11 & tenure_prior <= 30 ~ "11+ years"),
#levels = c("0-2 years", "3-5 years", "6-10 years", "11+ years")))
