load("data/processed/cleaned_wine_data.RData")

par(mfrow=c(3,4)) #allows 12 graphs on one image instead of 1

numeric.cols = colnames(wine_data[2:13]) #selecting columns with numbers
for (col in numeric.cols){
  attribute = wine_data[[col]] # selecting one particular column
  #plotting q-q plots for the respective columns
  qqnorm(attribute, main = paste('Q-Q Plot:', col))
  qqline(attribute, col ='red',lwd = 2)
}
ggsave("reports/figures/QQ_plot.png")

for (col in numeric.cols) {
  attribute = wine_data[[col]]
  # Repeat sampling 1000 times and store the sample means
  sample_means <- replicate(
    n = 1000,                  # number of repetitions
    expr = mean(sample(attribute, size = 30, replace = TRUE))  # sample mean of size 30
  )
  
  # Plot Q-Q plot of the sample means
  qqnorm(sample_means, main = paste("Q-Q Plot of sample means:", col),cex.main = 0.8) #change title font size
  qqline(sample_means, col = "red", lwd = 2)
}
ggsave("reports/figures/QQ_plot_Sample_Means.png")

par(mfrow=c(1,1)) # resets plot dimensions


