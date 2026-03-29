# Statistical analysis — inferential tests (t-tests & confidence intervals)
# Comparison 1: mean quality by wine type (red vs white)
# Comparison 2: mean alcohol by quality tier (Low vs High; Medium excluded)

suppressPackageStartupMessages({
  library(dplyr)
})

# load processed data (same object name as cleaning / EDA scripts)
if (file.exists("data/processed/cleaned_wine_data.RData")) {
  load("data/processed/cleaned_wine_data.RData")
}

# Research Question 1: Do red and white wines differ in mean expert-rated quality?
# Null Hypothesis: mu_red = mu_white
# Alternative Hypothesis: mu_red != mu_white

cat("---Comparison 1: Quality score by wine type (red vs white)---")
# Load number of entries for each wine type
wine_data %>%
  count(type, name = "n") %>%
  print()
# Load mean and sd values for each wine type
wine_data %>%
  group_by(type) %>%
  summarise(
    n = n(),
    mean_quality = mean(quality),
    sd_quality = sd(quality),
    .groups = "drop"
  ) %>%
  print()

# t.test() to investigate whether there is a difference in quality between wine types
tt_quality_type <- t.test(quality ~ type, data = wine_data, var.equal = FALSE)
print(tt_quality_type)
# p-value << significance level of 0.05 hence there is sufficient evidence at 
#significance level of 0.05 to reject H0, the mean quality of the two groups differ.


# --- Comparison 2 -----------------------------------------------------------
# RQ: Among Low vs High quality wines, does mean alcohol differ?
# H0: mu_Low = mu_High   |   H1: mu_Low != mu_High

# Extract wine data that only contain low ad high quality wine
wine_low_high <- wine_data %>%
  filter(quality_group %in% c("Low", "High")) %>%
  mutate(quality_group = factor(quality_group, levels = c("Low", "High")))

cat("Comparison 2: Alcohol (%) by quality group (Low vs High only)\n")
# Load count of wine in each quality group
wine_low_high %>%
  count(quality_group, name = "n") %>%
  print()

# Load mean and sd of each quality group
wine_low_high %>%
  group_by(quality_group) %>%
  summarise(
    n = n(),
    mean_alcohol = mean(alcohol),
    sd_alcohol = sd(alcohol),
    .groups = "drop"
  ) %>%
  print()

# t.test() to investigate whether there is a difference in alcohol for different quality alcohol
tt_alcohol_qg <- t.test(alcohol ~ quality_group, data = wine_low_high, var.equal = FALSE)
print(tt_alcohol_qg)
# since p-value << 0.05 significance level, there is sufficient evidence at 
#significance level of 0.05 to reject H0, the mean alcohol of the two groups differ.

