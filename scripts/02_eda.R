# Exploratory Data Analysis
# create summary statistics and visualisations for key variables separated by wine type and quality

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(GGally)
})

# load processed data
load("data/processed/cleaned_wine_data.RData")

# summary statistics
cat("---Summary Stats---\n")
summary(wine_data)

# Ensure figures directory exists
if (!dir.exists("reports/figures")) {
  dir.create("reports/figures", recursive = TRUE)
}

cat("\nGenerating EDA Figures\n")

# 1. Distribution of Alcohol Content by Type and Quality (Histogram)
p1 <- ggplot(wine_data, aes(x = alcohol, fill = type)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.7, position = "identity") +
  facet_grid(type ~ quality_group) +
  theme_bw() +
  labs(
    title = "Distribution of Alcohol Content by Type and Quality",
    x = "Alcohol Content (%)", y = "Frequency"
  ) +
  scale_fill_manual(values = c("red" = "#800020", "white" = "#F2EDC9"))

ggsave("reports/figures/alcohol_histogram_split.png", plot = p1, width = 10, height = 6)

# 2. Alcohol Content vs Quality Group across Wine Types (Boxplot)
p2 <- ggplot(wine_data, aes(x = quality_group, y = alcohol, fill = type)) +
  geom_boxplot(alpha = 0.8) +
  theme_bw() +
  labs(
    title = "Alcohol Content vs Quality Group across Wine Types",
    x = "Quality Group", y = "Alcohol Content (%)"
  ) +
  scale_fill_manual(values = c("red" = "#800020", "white" = "#F2EDC9"))

ggsave("reports/figures/alcohol_boxplot_split.png", plot = p2, width = 8, height = 6)

# 3. Scatterplot Matrix of Key Chemical Properties
# Select a subset of variables to keep the plot readable
key_vars <- wine_data %>% select(alcohol, fixed.acidity, citric.acid, volatile.acidity, quality_group, type)
p3 <- ggpairs(key_vars,
  columns = 1:4,
  aes(color = type, alpha = 0.5),
  lower = list(continuous = wrap("points", size = 0.5)),
  title = "Scatterplot Matrix of Key Chemical Properties"
) +
  theme_bw()

ggsave("reports/figures/key_variables_pairplot.png", plot = p3, width = 12, height = 10)
