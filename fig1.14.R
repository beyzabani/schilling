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
         ausbildungsort, fieldofstudy, foreigners_next) %>%
  mutate_all(as.character) %>%
  mutate_all(~ recode(., "NA" = NA_character_, "N/A" = NA_character_))

table(df$Nationality_co)
table(df$foreigners_next)

sum(is.na(df$Nationality_co))
sum(is.na(df$foreigners_next))
# Sample 1 ----------------------------------------------------------------


# Filter, mutate, and categorize ------------------------------------------
df_prior <- df %>% 
  filter(Sample == 0 | Sample == 1) %>%
  drop_na(prior_code, Nationality_co) %>%
  mutate(Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male"))) %>%
  mutate(company_change = factor(case_when(
    prior_code %in% c(0, 1, 2, 4) ~ "Company Change", 
    prior_code == 3 ~ "Stayed in Same Company" ))) %>%
  mutate(nationality_group = factor(case_when(
    Nationality_co == 0 ~ "Schweiz",
    TRUE ~ "Foreigner"))) %>%
  filter(company_change == "Company Change")  # Filter only those who changed company

# Group by gender and nationality, calculate count and percentage ---------
df_summary <- df_prior %>%
  group_by(Gender, nationality_group) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(Gender) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

# Create the plot ---------------------------------------------------------
plot_gender_nationality <- ggplot(df_summary, aes(x = nationality_group, y = percentage, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(percentage, "% (", count, ")")),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  labs(title = "Sample 1: Company Change - Gender and Nationality",
       x = "Nationality Group",
       y = "Percentage",
       fill = "Gender") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(legend.title = element_text())

# Print the plot
print(plot_gender_nationality)


# Sample 2 ----------------------------------------------------------------


# Filter, mutate, and categorize ------------------------------------------
df_prior2 <- df %>% 
  filter(Sample == 0 | Sample == 2) %>%
  drop_na(prior_code, Nationality_co) %>%
  mutate(Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male"))) %>%
  mutate(company_change = factor(case_when(
    prior_code %in% c(0, 1, 2, 4) ~ "Company Change", 
    prior_code == 3 ~ "Stayed in Same Company" ))) %>%
  mutate(nationality_group = factor(case_when(
    Nationality_co == 0 ~ "Schweiz",
    TRUE ~ "Foreigner"))) %>%
  filter(company_change == "Company Change")  # Filter only those who changed company

# Group by gender and nationality, calculate count and percentage ---------
df_summary2 <- df_prior2 %>%
  group_by(Gender, nationality_group) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(Gender) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

# Create the plot ---------------------------------------------------------
plot_gender_nationality2 <- ggplot(df_summary2, aes(x = nationality_group, y = percentage, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(percentage, "% (", count, ")")),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  labs(title = "Sample 2: Company Change - Gender and Nationality",
       x = "Nationality Group",
       y = "Percentage",
       fill = "Gender") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(legend.title = element_text())

# Print the plot
print(plot_gender_nationality2)
