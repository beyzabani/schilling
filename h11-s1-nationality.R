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
         Nationality_co, Branche, total_employees) %>%
  mutate_all(as.character) %>%
  mutate_all(~ recode(., "NA" = NA_character_, "N/A" = NA_character_))


# Add new variables
df <- df %>%
  drop_na(Stellenantritt) %>%
  mutate(tenure = as.numeric(Austrittsjahr) - as.numeric(Stellenantritt))


df_employee <- df %>% 
  filter(Sample == 0 | Sample == 1) %>%
  drop_na(total_employees, Nationality_co) %>% 
  mutate(Nationality_co = factor(Nationality_co, 
                                 levels = c(0, 1), 
                                 labels = c("Schweiz", "Foreign"))) %>% 
  mutate(total_employees = as.numeric(total_employees)) %>% 
  mutate(employee_group = case_when(
    total_employees >= 0 & total_employees <= 10000 ~ "<10000",
    total_employees >= 10001 & total_employees <= 50000 ~ "10001-50000",
    total_employees >= 50001 ~ "50001+",
    TRUE ~ NA_character_)) %>%  
  mutate(employee_group = factor(employee_group, 
                                 levels = c("<10000", "10001-50000", "50001+"))) %>% 
  mutate(Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male")))

# Summary statistics by Gender, Nationality_co, and Employee Group
summary_stats_gender_nationality <- df_employee %>%
  group_by(Gender, Nationality_co, employee_group) %>%
  summarize(
    count = n(),  # Count of observations
    mean_tenure = round(mean(tenure, na.rm = TRUE), 1),  # Mean tenure
    sd_tenure = round(sd(tenure, na.rm = TRUE), 1)  # Standard deviation of tenure
  )

# Summary statistics table for Gender, Nationality_co with count, average tenure, and SD tenure
summary_stats_gender_nationality %>%
  kable(
    caption = "Sample 1: Average Tenure by Gender, Nationality, and Employee Group",
    col.names = c("Gender", "Nationality", "Employee Group", "Count", "Average Tenure", "SD Tenure"),
    align = "c"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  )


# Perform a t-test to compare the mean tenure between males and females
t_test_gender <- t.test(tenure ~ Gender, data = df_employee)

# Print the t-test result for gender
print(t_test_gender)


# Filter dataset to include two employee groups for comparison
df_employee_filtered <- df_employee %>%
  filter(employee_group %in% c("<10000", "50001+"))

# Perform a t-test to compare the mean tenure between employee groups
t_test_employee_group <- t.test(tenure ~ employee_group, data = df_employee_filtered)

# Print the t-test result for employee group
print(t_test_employee_group)



# Interpretation of Gender T-Test -----------------------------------------
# Welch Two Sample t-test
# t = -4.285, df = 110.75, p-value = 3.912e-05
# The t-statistic (-4.285) indicates a significant difference between the average tenure of males and females.
# The p-value (3.912e-05) is much smaller than 0.05, meaning we reject the null hypothesis and conclude
# that there is a statistically significant difference in tenure between genders.
# The 95% confidence interval (-4.52, -1.66) does not include 0, further supporting the conclusion.
# Mean tenure for females: 4.75 years, mean tenure for males: 7.84 years.
# Conclusion: Males have a significantly longer tenure on average compared to females.

# Filter dataset to include two employee groups for comparison ------------
df_employee_filtered <- df_employee %>%
  filter(employee_group %in% c("<10000", "50001+"))

# Perform a t-test to compare the mean tenure between employee groups -----
t_test_employee_group <- t.test(tenure ~ employee_group, data = df_employee_filtered)

# Print the t-test result for employee group
print(t_test_employee_group)

# Interpretation of Employee Group T-Test ---------------------------------
# Welch Two Sample t-test
# t = -0.18056, df = 88.77, p-value = 0.8571
# The t-statistic (-0.18) shows that there is very little difference between the mean tenure of employees
# in companies with <10000 employees and those with 50001+ employees.
# The p-value (0.8571) is much larger than 0.05, so we fail to reject the null hypothesis, meaning
# there is no statistically significant difference in tenure between these two employee groups.
# The 95% confidence interval (-1.91, 1.59) includes 0, supporting the conclusion that there is no significant difference.
# Mean tenure for the 50001+ group: 6.06 years, mean tenure for the <10000 group: 6.22 years.
# Conclusion: There is no significant difference in mean tenure between employees in small (<10000) and large (50001+) companies.
