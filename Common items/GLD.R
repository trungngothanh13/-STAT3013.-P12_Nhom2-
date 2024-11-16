install.packages("ggplot2")
library(ggplot2)
library(dplyr)

file_gc <- "C:/Users/Dell/OneDrive/Documents/GitHub/-STAT3013.-P12_Nhom2-/dataset/cleaned_GLD.csv"
data_gc <- read.csv(file_gc)

head(data_gc)

analyze_data <- function(data, dataset_name) {
  if (!"Close" %in% colnames(data)) {
    stop("Cột 'Close' không tồn tại trong dataset.")
  }
  
  col_data <- as.numeric(data$Close)
  
  # Calculate statistics
  mean_val <- mean(col_data, na.rm = TRUE)
  median_val <- median(col_data, na.rm = TRUE)
  mode_val <- as.numeric(names(sort(table(col_data), decreasing = TRUE)[1]))
  range_val <- diff(range(col_data, na.rm = TRUE))
  variance_val <- var(col_data, na.rm = TRUE)
  std_dev_val <- sd(col_data, na.rm = TRUE)
  
  # Print statistics
  cat("\nThống kê cho cột: Close\n")
  cat("Mean: ", mean_val, "\n")
  cat("Median: ", median_val, "\n")
  cat("Mode: ", mode_val, "\n")
  cat("Range: ", range_val, "\n")
  cat("Variance: ", variance_val, "\n")
  cat("Standard Deviation: ", std_dev_val, "\n")
  
  # Create and print the histogram
  p1 <- ggplot(data, aes(x = Close)) +
    geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
    labs(title = paste(dataset_name, "- Close Histogram")) +
    theme_minimal()
  print(p1)
  
  # Create and print the box plot
  p2 <- ggplot(data, aes(x = "", y = Close)) +
    geom_boxplot(fill = "green", alpha = 0.7) +
    labs(title = paste(dataset_name, "- Close Box Plot")) +
    theme_minimal()
  print(p2)
  
  # Create and print the scatter plot if "Date" column exists
  if ("Date" %in% colnames(data)) {
    p3 <- ggplot(data, aes(x = Date, y = Close)) +
      geom_line(color = "red") +
      labs(title = paste(dataset_name, "- Close Scatter Plot")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme_minimal()
    print(p3)
  }
}

# Run the function and generate the visualizations
results_gc <- analyze_data(data_gc, "GLD")
