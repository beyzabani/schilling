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
  select(Austrittsjahr, Jahrgang, Gender, Sample, Stellenantritt, Nationality, 
         Nationality_co, Branche, total_employees, wechsel_unternehmen_after, 
         wechselposition_after, fieldofstudy, highest_degree) %>%
  mutate_all(as.character) %>%
  mutate_all(~ recode(., "NA" = NA_character_, "N/A" = NA_character_))
#table(dfs1$wechsel_unternehmen_after)

dfs1 <- df %>% 
  filter(Sample == 0 | Sample == 2) %>%
  drop_na(Jahrgang, Austrittsjahr, wechsel_unternehmen_after) %>%
  mutate(age_exit = as.numeric(Austrittsjahr) - as.numeric(Jahrgang)) %>% 
  mutate(age_exit_group = case_when(
    age_exit >= 28 & age_exit <= 44 ~ "younger than 45",
    age_exit >= 45 & age_exit <= 54 ~ "45-54 years old",
    age_exit >= 55 & age_exit <= 73 ~ "55+ years old",
    TRUE ~ NA_character_)) %>%  
  mutate(age_exit_group = factor(age_exit_group, 
                                  levels = c("younger than 45", "45-54 years old", "55+ years old"))) %>% 
  mutate(Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male"))) %>% 
  
  mutate(company_change = case_when(
    wechsel_unternehmen_after != 3 ~ "wechsel des unternehmens",
    wechsel_unternehmen_after == 3 ~ "keine Veranderung im Unternehmen"
  ))

#table(dfs1$company_change)
  

# Create a summary table based on company change and gender
summary_table <- dfs1 %>%
  group_by(company_change, Gender) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

# Display the summary table
summary_table %>%
  kable() %>%
  kable_styling(full_width = F)


# Calculate counts and percentages
dfs1_summary <- dfs1 %>%
  group_by(age_exit_group, Gender, company_change) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(company_change) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

# Create a plot with counts and percentages on top of bars
plot <- ggplot(dfs1_summary, aes(x = age_exit_group, y = count, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(count, " (", percentage, "%)")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  facet_wrap(~ company_change) +
  labs(title = "Age Exit by Gender and Company Change", 
       x = "Age Exit") +
  theme_minimal()

# Display the plot
print(plot)

#table(df$wechselposition_after)


#men, chaning company
# Filter who changed companies and focus on the 45-54 and 55+ age groups
df_filtered <- dfs1 %>%
  filter(company_change == "wechsel des unternehmens",
         age_exit_group %in% c("45-54 years old", "55+ years old")) %>% 
  mutate(Nationality_co = factor(Nationality_co, 
                                 levels = c(0, 1), 
                                 labels = c("Schweiz", "Foreign")))

# Group by nationality, age exit group, and company change
df_summary <- df_filtered %>%
  group_by(Nationality_co, age_exit_group, company_change) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(company_change) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

# Create a plot with counts and percentages on top of bars
plot_nationality <- ggplot(df_summary, aes(x = age_exit_group, y = count, fill = Nationality_co)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(count, " (", percentage, "%)")),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  facet_wrap(~ company_change) +
  labs(title = "Nationality of Changing Companies by Age Group (45-54 & 55+)", 
       x = "Age Exit Group", 
       y = "Count", 
       fill = "Nationality") +
  theme_minimal()

# Display the plot
print(plot_nationality)

# Filter who changed companies and focus on the 45-54 and 55+ age groups
df_filtered <- dfs1 %>%
  filter(company_change == "wechsel des unternehmens",
         age_exit_group %in% c("45-54 years old", "55+ years old")) %>% 
  mutate(Nationality_co = factor(Nationality_co, 
                                 levels = c(0, 1), 
                                 labels = c("Schweiz", "Foreign")))

# Group by nationality, age exit group, gender, and company change
df_summary <- df_filtered %>%
  group_by(Nationality_co, Gender, age_exit_group, company_change) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(company_change) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

# Create a plot with counts and percentages on top of bars, and with custom colors
plot_nationality_gender <- ggplot(df_summary, aes(x = age_exit_group, y = count, 
                                                  fill = interaction(Nationality_co, Gender))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(count, " (", percentage, "%)")),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  facet_wrap(~ company_change) +
  labs(title = "Nationality and Gender of Changing Companies by Age Group (45-54 & 55+)", 
       x = "Age Exit Group", 
       y = "Count", 
       fill = "Nationality & Gender") +
  scale_fill_manual(values = c("Schweiz.Male" = "#1f77b4", "Schweiz.Female" = "#ff7f0e",
                               "Foreign.Male" = "#2ca02c", "Foreign.Female" = "#d62728")) +  # Custom colors for each combination
  theme_minimal()

# Display the plot
print(plot_nationality_gender)




#Tenure and age at exit 
dfs2 <- df %>% 
  filter(Sample == 0 | Sample == 2) %>%
  drop_na(Jahrgang, Austrittsjahr, Stellenantritt) %>%
  mutate(tenure = as.numeric(Austrittsjahr) - as.numeric(Stellenantritt)) %>% 
  mutate(age_exit = as.numeric(Austrittsjahr) - as.numeric(Jahrgang)) %>% 
  mutate(age_exit_group = case_when(
    age_exit >= 28 & age_exit <= 44 ~ "younger than 45",
    age_exit >= 45 & age_exit <= 54 ~ "45-54 years old",
    age_exit >= 55 & age_exit <= 73 ~ "55+ years old",
    TRUE ~ NA_character_)) %>%  
  mutate(age_exit_group = factor(age_exit_group, 
                                 levels = c("younger than 45", "45-54 years old", "55+ years old"))) %>% 
  mutate(Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male")))


# Summary statistics by Gender and Age Exit Group
summary_stats_gender_age <- dfs2 %>%
  group_by(Gender, age_exit_group) %>%
  summarize(
    count = n(),  # Count of observations
    mean_tenure = round(mean(tenure, na.rm = TRUE), 1),  # Mean tenure
    sd_tenure = round(sd(tenure, na.rm = TRUE), 1)  # Standard deviation of tenure
  )

# Summary statistics table for Gender and Age Exit Group with count, average tenure, and SD tenure
summary_stats_gender_age %>%
  kable(
    caption = "Sample 2: Tenure by Gender and Age Exit Group",
    col.names = c("Gender", "Age Exit Group", "Count", "Average Tenure", "SD Tenure"),
    align = "c"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  )
