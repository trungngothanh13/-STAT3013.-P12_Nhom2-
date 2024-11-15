
install.packages("ggplot2")
library(ggplot2)
library(dplyr)

file_gc <- "C:/Users/Dell/OneDrive/Documents/GitHub/-STAT3013.-P12_Nhom2-/dataset/cleaned_gc=f.csv"

data_gc <- read.csv(file_gc)


head(data_gc)


analyze_data <- function(data, dataset_name) {
  numerical_cols <- data %>% select(where(is.numeric))
  results <- list()
  
  for (col_name in colnames(numerical_cols)) {
    col_data <- numerical_cols[[col_name]]
    
    # Tính toán các thống kê
    stats <- list(
      mean = mean(col_data, na.rm = TRUE),
      median = median(col_data, na.rm = TRUE),
      mode = as.numeric(names(sort(table(col_data), decreasing = TRUE)[1])),
      range = diff(range(col_data, na.rm = TRUE)),
      variance = var(col_data, na.rm = TRUE),
      std_dev = sd(col_data, na.rm = TRUE)
    )
    results[[col_name]] <- stats
    
    # Vẽ đồ thị
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

head(data_gc)

results_gc <- analyze_data(data_gc, "GC=f")

print(results_gc)

