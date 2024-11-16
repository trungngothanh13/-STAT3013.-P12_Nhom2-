install.packages("ggplot2")
library(ggplot2)
library(dplyr)


file_gc <- "C:/Users/Dell/OneDrive/Documents/GitHub/-STAT3013.-P12_Nhom2-/dataset/cleaned_GLD.csv"


data_gc <- read.csv(file_gc)


head(data_gc)


analyze_data <- function(data, dataset_name) {
  numerical_cols <- data %>% select(where(is.numeric))
  results <- list()
  
  for (col_name in colnames(numerical_cols)) {
    col_data <- numerical_cols[[col_name]]
    
    mean_val <- mean(col_data, na.rm = TRUE)
    median_val <- median(col_data, na.rm = TRUE)
    mode_val <- as.numeric(names(sort(table(col_data), decreasing = TRUE)[1]))  # Mode
    
    
    range_val <- diff(range(col_data, na.rm = TRUE))  # Range
    variance_val <- var(col_data, na.rm = TRUE)
    std_dev_val <- sd(col_data, na.rm = TRUE)
    
    
    stats <- list(
      mean = mean_val,
      median = median_val,
      mode = mode_val,
      range = range_val,
      variance = variance_val,
      std_dev = std_dev_val
    )
    
    results[[col_name]] <- stats
    
    
    cat("\nThống kê cho cột:", col_name, "\n")
    cat("Mean: ", mean_val, "\n")
    cat("Median: ", median_val, "\n")
    cat("Mode: ", mode_val, "\n")
    cat("Range: ", range_val, "\n")
    cat("Variance: ", variance_val, "\n")
    cat("Standard Deviation: ", std_dev_val, "\n")
    
    
    print(ggplot(data, aes_string(x = col_name)) +
            geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
            labs(title = paste(dataset_name, "-", col_name, "Histogram")) +
            theme_minimal())
    
    
    print(ggplot(data, aes_string(x = "", y = col_name)) +
            geom_boxplot(fill = "green", alpha = 0.7) +
            labs(title = paste(dataset_name, "-", col_name, "Box Plot")) +
            theme_minimal())
    
    if ("Date" %in% colnames(data)) {
      print(ggplot(data, aes_string(x = "Date", y = col_name)) +
              geom_line(color = "red") +
              labs(title = paste(dataset_name, "-", col_name, "Scatter Plot")) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              theme_minimal())
    }
  }
  
  return(results)
}


results_gc <- analyze_data(data_gc, "GLD")


print(results_gc)

