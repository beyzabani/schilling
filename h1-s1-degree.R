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

# Add new variables
#df <- df %>%
 # drop_na(Stellenantritt) %>%
  #mutate(tenure = as.numeric(Austrittsjahr) - as.numeric(Stellenantritt))


df_degree <- df %>% 
  filter(Sample == 0 | Sample == 1) %>%
  drop_na(highest_degree) %>% 
  mutate(Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male")),
         highest_degree = factor(highest_degree, levels = c(1, 2, 3, 4), 
                                 labels = c("Bachelor's degree", "Master's degree/MBA", "PhD", "Other")))



# Summary statistics by Gender and Highest Degree, including count
#summary_stats_degree <- df_degree %>%
 # group_by(Gender, highest_degree) %>%
  #summarize(
   # count = n(),  # Count of observations
    #mean_tenure = round(mean(tenure, na.rm = TRUE), 1),  # Mean tenure
    #sd_tenure = round(sd(tenure, na.rm = TRUE), 1)  # Standard deviation of tenure)

# Summary statistics table with count
#summary_stats_degree %>%
 # kable(
  #  caption = "Sample 1: Average Tenure by Gender and Highest Degree",
   # col.names = c("Gender", "Highest Degree", "Count", "Average Tenure", "SD Tenure"),
    #align = "c") %>%
  #kable_styling(
 #   bootstrap_options = c("striped", "hover", "condensed", "responsive"),
   # full_width = FALSE,
    #position = "center")




# Group by gender and highest degree, and calculate count and percentage
df_summary <- df_degree %>%
  group_by(Gender, highest_degree) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(Gender) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

# Create a plot with gender on x-axis, degree on y-axis, percentage, and custom labels
plot_gender_degree <- ggplot(df_summary, aes(x = Gender, y = percentage, fill = highest_degree)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(percentage, "% (", count, ")")),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  labs(title = "Sample 1: Percentage of Highest Degree by Gender",
       x = "Gender",
       y = "Percentage",
       fill = "Highest Degree") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(legend.title = element_text())

# Print the plot
print(plot_gender_degree)



# ALL ---------------------------------------------------------------------


df_all <- df %>% 
  drop_na(highest_degree) %>% 
  mutate(Gender = factor(Gender, levels = c(0, 1), labels = c("Female", "Male")),
         highest_degree = factor(highest_degree, levels = c(1, 2, 3, 4), 
                                 labels = c("Bachelor's degree", "Master's degree/MBA", "PhD", "Other")))


# Group by gender and highest degree, and calculate count and percentage
df_summary_all <- df_all %>%
  group_by(Gender, highest_degree) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(Gender) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))

# Create a plot with gender on x-axis, degree on y-axis, percentage, and custom labels
plot_gender_all <- ggplot(df_summary_all, aes(x = Gender, y = percentage, fill = highest_degree)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(percentage, "% (", count, ")")),
            position = position_dodge(width = 0.9),
            vjust = -0.5) +
  labs(title = "Both Samples: Percentage of Highest Degree by Gender",
       x = "Gender",
       y = "Percentage",
       fill = "Highest Degree") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(legend.title = element_text())

# Print the plot
print(plot_gender_all)




##########t-test
# Perform a t-test to compare the mean tenure between males and females
t_test_gender <- t.test(tenure ~ Gender, data = df_degree)

# Print the t-test result
print(t_test_gender)


# Filter the dataset to only include two degree categories
df_degree_filtered <- df_degree %>%
  filter(highest_degree %in% c("Master's degree/MBA", "PhD"))

# Perform a t-test to compare the mean tenure between Master's and PhD holders
t_test_degree <- t.test(tenure ~ highest_degree, data = df_degree_filtered)

# Print the t-test result
print(t_test_degree)






# Perform a t-test to compare the mean tenure between males and females
t_test_gender <- t.test(tenure ~ Gender, data = df_degree)

# Print the t-test result for gender
print(t_test_gender)

# Interpretation of the t-test for gender:
# Welch Two Sample t-test
#
# t = -4.1108, df = 107.99, p-value = 7.708e-05
# The t-statistic of -4.1108 indicates that there is a meaningful difference
# between the average tenure of females and males.
# The p-value (7.708e-05) is very small, far below 0.05, which means that 
# we reject the null hypothesis and conclude that there is a significant difference
# in the average tenure between males and females.
#
# 95% Confidence Interval:
# The confidence interval (-4.36 to -1.52) does not include 0, which supports the
# conclusion that there is a significant difference between the two groups.
#
# Mean in group Female: 4.74 years
# Mean in group Male: 7.68 years
# Males have a longer average tenure than females, with an average difference of
# around 3 years.

# Perform a t-test to compare the mean tenure between Master's degree/MBA and PhD holders
df_degree_filtered <- df_degree %>%
  filter(highest_degree %in% c("Master's degree/MBA", "PhD"))

t_test_degree <- t.test(tenure ~ highest_degree, data = df_degree_filtered)

# Print the t-test result for degree levels
print(t_test_degree)

# Interpretation of the t-test for degree level:
# Welch Two Sample t-test
#
# t = 0.46272, df = 48.319, p-value = 0.6456
# The t-statistic is close to 0, indicating no meaningful difference between
# the average tenure of those with a Master's degree/MBA and those with a PhD.
# The p-value (0.6456) is much larger than 0.05, so we fail to reject the null hypothesis.
# This means that there is no statistically significant difference in tenure between
# the two groups.
#
# 95% Confidence Interval:
# The confidence interval (-1.18 to 1.88) includes 0, which supports the conclusion
# that there is no significant difference between the groups.
#
# Mean in group Master's degree/MBA: 5.95 years
# Mean in group PhD: 5.60 years
# The average difference is small (around 0.35 years), and not statistically significant.

