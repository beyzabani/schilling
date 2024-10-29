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

# Sample 1 ----------------------------------------------------------------


df_nation <- df %>% 
  filter(Sample == 0 | Sample == 1) %>%
  drop_na(foreigners_next) %>% 
  mutate(Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male"))) %>% 
    mutate(foreigners = factor(case_when(
      foreigners_next == 0 ~ "in der Schweiz",                
      foreigners_next %in% c(1, 2, 3) ~ "im Ausland",         
      foreigners_next == 4 ~ "Schweizer"  )))
  

# Group by gender and highest degree, and calculate count and percentage
df_summary <- df_nation %>%
  group_by(Gender, foreigners) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(Gender) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

# Create a plot with foreigners on x-axis, gender as fill (Female: red, Male: blue), and percentage on y-axis
plot_gender_foreigners <- ggplot(df_summary, aes(x = foreigners, y = percentage, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(percentage, "% (", count, ")")),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  labs(title = "Sample 1: Percentage of Foreigners by Gender",
       fill = "Gender") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = c("Female" = "red", "Male" = "blue")) +  # Set Female as red and Male as blue
  theme_minimal() +
  theme(legend.title = element_text())

# Print the plot
print(plot_gender_foreigners)


# Sample 2 ----------------------------------------------------------------


df_nation2 <- df %>% 
  filter(Sample == 0 | Sample == 2) %>%
  drop_na(foreigners_next) %>% 
  mutate(Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male"))) %>% 
  mutate(foreigners = factor(case_when(
    foreigners_next == 0 ~ "in der Schweiz",                
    foreigners_next %in% c(1, 2, 3) ~ "im Ausland",         
    foreigners_next == 4 ~ "Schweizer"  )))


# Group by gender and highest degree, and calculate count and percentage
df_summary2 <- df_nation2 %>%
  group_by(Gender, foreigners) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(Gender) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

# Create a plot with foreigners on x-axis, gender as fill (Female: red, Male: blue), and percentage on y-axis
plot_gender_foreigners2 <- ggplot(df_summary2, aes(x = foreigners, y = percentage, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(percentage, "% (", count, ")")),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  labs(title = "Sample 2: Percentage of Foreigners by Gender",
       fill = "Gender") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = c("Female" = "red", "Male" = "blue")) +  # Set Female as red and Male as blue
  theme_minimal() +
  theme(legend.title = element_text())

# Print the plot
print(plot_gender_foreigners2)


# All ----------------------------------------------------------------


df_nation_all <- df %>% 
  drop_na(foreigners_next) %>% 
  mutate(Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male"))) %>% 
  mutate(foreigners = factor(case_when(
    foreigners_next == 0 ~ "in der Schweiz",                
    foreigners_next %in% c(1, 2, 3) ~ "im Ausland",         
    foreigners_next == 4 ~ "Schweizer"  )))


# Group by gender and highest degree, and calculate count and percentage
df_summary_all <- df_nation_all %>%
  group_by(Gender, foreigners) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(Gender) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

# Create a plot with foreigners on x-axis, gender as fill (Female: red, Male: blue), and percentage on y-axis
plot_gender_foreigners_all <- ggplot(df_summary_all, aes(x = foreigners, y = percentage, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(percentage, "% (", count, ")")),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  labs(title = "Both Samples: Percentage of Foreigners by Gender",
       fill = "Gender") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = c("Female" = "red", "Male" = "blue")) +  # Set Female as red and Male as blue
  theme_minimal() +
  theme(legend.title = element_text())

# Print the plot
print(plot_gender_foreigners_all)
