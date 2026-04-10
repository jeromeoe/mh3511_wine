# One Way ANOVA
# Test: does the mean alcohol content differ significantly across quality groups?

suppressPackageStartupMessages({
  library(dplyr)
  library(car)
  library(effectsize)
})

# load processed data
load("data/processed/cleaned_wine_data.RData")

cat("RQ: Does the mean alcohol content differ significantly across the different quality groups (L M H)?\n\n")

cat("1. Check for normality\n")
# The Shapiro-Wilk test has a limitation of 5000 samples.
# Our dataset size is > 5000, so we will take a random sample of 5000 for the test to avoid errors.
set.seed(555)
sample_data <- wine_data |> sample_n(min(n(), 5000))

anova_model_sample <- aov(alcohol ~ quality_group, data = sample_data)
shapiro_result <- shapiro.test(residuals(anova_model_sample))
print(shapiro_result)

# We use CLT to confirm Normality instead of Shapiro-Wilk because our sample size is too large to properly use SW

cat("1.5 Visualizing Assumptions (Plots saved to reports/figures/)\n")
# Q-Q plot does not have sample size limits, making this a better visual check for normality
png("reports/figures/05_qqplot_residuals.png", width = 800, height = 600)
# Use residuals of the full dataset for the visual check
full_model <- aov(alcohol ~ quality_group, data = wine_data)
qqPlot(residuals(full_model), main = "Q-Q Plot of ANOVA Residuals")
dev.off()

# Boxplot to visually check homogeneity of variance across groups
png("reports/figures/05_boxplot_alcohol_quality.png", width = 800, height = 600)
boxplot(alcohol ~ quality_group,
  data = wine_data,
  main = "Alcohol Content by Quality Group",
  xlab = "Quality Group", ylab = "Alcohol Content",
  col = c("lightblue", "lightgreen", "lightpink")
)
dev.off()
cat("Plots generated.\n\n")

cat("2. Check for homogeneity of variance\n")
# Levene's Test
levene_res <- leveneTest(alcohol ~ quality_group, data = wine_data)
print(levene_res)
cat("If Levene's test p-value < 0.05, it indicates unequal variances. ANOVA is robust against this if group sizes are somewhat similar, or Welch's ANOVA could be considered as an alternative.\n\n")

cat("3. One Way ANOVA (with dynamic selection)\n")
levene_p_val <- levene_res$`Pr(>F)`[1]

if (levene_p_val < 0.05) {
  cat("Result: Levene's test p-value < 0.05 (Unequal variances).\n=> Proceeding with Welch's ANOVA.\n")
  welch_model <- oneway.test(alcohol ~ quality_group, data = wine_data, var.equal = FALSE)
  print(welch_model)
} else {
  cat("Result: Levene's test p-value >= 0.05 (Equal variances).\n=> Proceeding with Standard ANOVA.\n")
  standard_model <- aov(alcohol ~ quality_group, data = wine_data)
  print(summary(standard_model))
}

anova_model <- aov(alcohol ~ quality_group, data = wine_data)
cat("\n")

cat("4. Effect Size (Eta-Squared)\n")


eta_sq <- eta_squared(anova_model)
print(eta_sq)
cat("Eta-squared (η²) indicates the proportion of variance in alcohol content explained by the quality group. A higher value indicates a stronger practical significance.\n\n")

cat("5. Post-Hoc Testing (Tukey's HSD)\n") # Or pairwise.t.test works too
# Check which specific groups differ from each other
tukey_res <- TukeyHSD(anova_model)
print(tukey_res)

# We can examine the p-value in the ANOVA summary object to confirm structural significance.
# Interpret Eta-Squared for magnitude of differences.
# Look at the 'p adj' column in Tukey output: groups with p < 0.05 differ significantly. Check the 'diff' column to see which group has higher/lower means.
