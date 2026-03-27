#Data Cleaning
#loads raw wine data, checks outliers + class imbalance, bins variables by quality & exports clean dataset

suppressPackageStartupMessages({ #load libraries
  library(dplyr)
  library(tidyr)
})

#load data
if (file.exists("data/raw/wine-quality-white-and-red.csv")) {
  raw_data_path <- "data/raw/wine-quality-white-and-red.csv"    
} else if (file.exists("../data/raw/wine-quality-white-and-red.csv")) {
  raw_data_path <- "../data/raw/wine-quality-white-and-red.csv" 
} else {
  stop("Make sure data is present.")
}

wine_data <- read.csv(raw_data_path)

cat("---Dataset Loaded---\n")
cat("Dimensions:", dim(wine_data)[1], "rows and", dim(wine_data)[2], "columns\n\n")

#class imbalance context 
cat("---Class Imbalance---\n")
type_counts <- table(wine_data$type)
print(type_counts)
cat("Note: There is a class imbalance with", type_counts["white"], "white wines and", type_counts["red"], "red wines.\n")

#data cleaning
wine_data <- wine_data %>% drop_na()
wine_data <- distinct(wine_data)
cat("Dataset dimensions after dropping NAs and duplicates:", dim(wine_data)[1], "rows.\n\n")

#outlier detection (using IQR bounds)
# we will identify & not delete outliers in the 'alcohol' column as it is our primary response variable for ANOVA
cat("---Outlier Detection (Alcohol)---\n")
Q1 <- quantile(wine_data$alcohol, 0.25)
Q3 <- quantile(wine_data$alcohol, 0.75)
IQR_alcohol <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_alcohol
upper_bound <- Q3 + 1.5 * IQR_alcohol

outliers <- wine_data %>% filter(alcohol < lower_bound | alcohol > upper_bound)
cat("Number of outliers in 'alcohol' based on 1.5 * IQR:", nrow(outliers), "\n")
cat("These outliers will be kept in the dataset to reflect natural variance.\n\n")

#feature engineering: binning quality
# standard quality scores range from 3 to 9 for some reason so we contextualise them
# we will bin them into 3 categories: Low (3-5), Medium (6), High (7-9)
wine_data <- wine_data %>%
  mutate(quality_group = case_when(
    quality <= 5 ~ "Low",
    quality == 6 ~ "Medium",
    quality >= 7 ~ "High"
  )) %>%
  mutate(
    type = as.factor(type),
    quality_group = factor(quality_group, levels = c("Low", "Medium", "High"))
  )

cat("---Quality Group Distribution---\n")
print(table(wine_data$quality_group))
cat("\n")

#export processed data
# ensure processed directory exists
if(!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}

#save as csv
write.csv(wine_data, "data/processed/cleaned_wine_data.csv", row.names = FALSE)
#save as RData to preserve factor levels
save(wine_data, file = "data/processed/cleaned_wine_data.RData")

cat("---Export Complete---\n")
cat("Cleaned data saved to 'data/processed/cleaned_wine_data.csv' and 'cleaned_wine_data.RData'\n")
