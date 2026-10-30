
# ==========================================================
# PROJECT TITLE: RETAIL DATA ANALYSIS
# Language: R Programming
# ==========================================================

# 1. SET WORKING DIRECTORY
setwd("C:/Users/HP/OneDrive/Desktop/Retail Data Analysis")

# 2. LOAD REQUIRED LIBRARIES
# Install packages only if they are not already installed
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# 3. LOAD DATASET
retail_data <- read.csv("Sample - Superstore.csv")

# 4. DISPLAY DATA
cat("\n--- FIRST SIX ROWS ---\n")
print(head(retail_data))

cat("\n--- DATASET DIMENSIONS ---\n")
print(dim(retail_data))

cat("\n--- COLUMN NAMES ---\n")
print(colnames(retail_data))

cat("\n--- DATA STRUCTURE ---\n")
str(retail_data)

# 5. DATA CLEANING
cat("\n--- MISSING VALUES ---\n")
print(colSums(is.na(retail_data)))

cat("\n--- DUPLICATE ROWS ---\n")
print(sum(duplicated(retail_data)))

# Remove duplicate rows
retail_data <- retail_data[!duplicated(retail_data), ]

# 6. SUMMARY STATISTICS
cat("\n--- SUMMARY STATISTICS ---\n")
print(summary(retail_data))

# 7. TOTAL SALES AND PROFIT
total_sales <- sum(retail_data$Sales, na.rm = TRUE)
total_profit <- sum(retail_data$Profit, na.rm = TRUE)
total_orders <- length(unique(retail_data$Order.ID))

cat("\n--- BUSINESS OVERVIEW ---\n")
cat("Total Sales:", round(total_sales, 2), "\n")
cat("Total Profit:", round(total_profit, 2), "\n")
cat("Unique Orders:", total_orders, "\n")

# 8. SALES BY CATEGORY
category_sales <- aggregate(
  Sales ~ Category,
  data = retail_data,
  FUN = sum,
  na.rm = TRUE
)

category_sales <- category_sales[
  order(category_sales$Sales, decreasing = TRUE),
]

cat("\n--- SALES BY CATEGORY ---\n")
print(category_sales)

# 9. PROFIT BY CATEGORY
category_profit <- aggregate(
  Profit ~ Category,
  data = retail_data,
  FUN = sum,
  na.rm = TRUE
)

cat("\n--- PROFIT BY CATEGORY ---\n")
print(category_profit)

# 10. TOP 10 PRODUCTS BY SALES
product_sales <- aggregate(
  Sales ~ Product.Name,
  data = retail_data,
  FUN = sum,
  na.rm = TRUE
)

product_sales <- product_sales[
  order(product_sales$Sales, decreasing = TRUE),
]

cat("\n--- TOP 10 PRODUCTS BY SALES ---\n")
print(head(product_sales, 10))

# 11. SALES BY REGION
region_sales <- aggregate(
  Sales ~ Region,
  data = retail_data,
  FUN = sum,
  na.rm = TRUE
)

cat("\n--- SALES BY REGION ---\n")
print(region_sales)

# 12. BAR CHART: SALES BY CATEGORY
p1 <- ggplot(category_sales,
             aes(x = Category, y = Sales, fill = Category)) +
  geom_col() +
  labs(
    title = "Sales by Product Category",
    x = "Category",
    y = "Total Sales"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p1)

# 13. BAR CHART: PROFIT BY CATEGORY
p2 <- ggplot(category_profit,
             aes(x = Category, y = Profit, fill = Category)) +
  geom_col() +
  labs(
    title = "Profit by Product Category",
    x = "Category",
    y = "Total Profit"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p2)

# 14. BAR CHART: SALES BY REGION
p3 <- ggplot(region_sales,
             aes(x = Region, y = Sales, fill = Region)) +
  geom_col() +
  labs(
    title = "Sales by Region",
    x = "Region",
    y = "Total Sales"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p3)

# 15. MONTHLY SALES ANALYSIS
retail_data$Order.Date <- as.Date(
  retail_data$Order.Date,
  format = "%m/%d/%Y"
)

retail_data$Month <- format(
  retail_data$Order.Date,
  "%Y-%m"
)

monthly_sales <- aggregate(
  Sales ~ Month,
  data = retail_data,
  FUN = sum,
  na.rm = TRUE
)

monthly_sales <- monthly_sales[order(monthly_sales$Month), ]

cat("\n--- MONTHLY SALES ---\n")
print(monthly_sales)

# 16. MONTHLY SALES TREND GRAPH
p4 <- ggplot(monthly_sales,
             aes(x = Month, y = Sales, group = 1)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Monthly Sales Trend",
    x = "Month",
    y = "Total Sales"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(p4)

# 17. SALES FORECASTING
if (nrow(monthly_sales) >= 3) {
  sales_ts <- ts(monthly_sales$Sales, frequency = 12)
  
  # Use a simple trend model for forecasting
  time_index <- seq_along(monthly_sales$Sales)
  trend_model <- lm(monthly_sales$Sales ~ time_index)
  
  future_index <- (length(time_index) + 1):
    (length(time_index) + 6)
  
  future_sales <- predict(
    trend_model,
    newdata = data.frame(time_index = future_index),
    interval = "prediction"
  )
  
  forecast_result <- data.frame(
    Period = paste("Future", 1:6),
    Predicted_Sales = round(future_sales[, "fit"], 2),
    Lower_Bound = round(future_sales[, "lwr"], 2),
    Upper_Bound = round(future_sales[, "upr"], 2)
  )
  
  cat("\n--- SALES FORECAST ---\n")
  print(forecast_result)
  
  forecast_plot_data <- data.frame(
    Period = factor(
      c(paste("Month", seq_along(monthly_sales$Sales)),
        paste("Forecast", 1:6)),
      levels = c(
        paste("Month", seq_along(monthly_sales$Sales)),
        paste("Forecast", 1:6)
      )
    ),
    Sales = c(
      monthly_sales$Sales,
      as.numeric(future_sales[, "fit"])
    ),
    Type = c(
      rep("Actual", nrow(monthly_sales)),
      rep("Forecast", 6)
    )
  )
  
  p5 <- ggplot(forecast_plot_data,
               aes(x = Period, y = Sales, color = Type, group = 1)) +
    geom_line() +
    geom_point() +
    labs(
      title = "Actual and Forecast Sales",
      x = "Period",
      y = "Sales"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  print(p5)
  
} else {
  cat("\nNot enough monthly observations for forecasting.\n")
}

# 18. SAVE CLEANED DATASET
write.csv(
  retail_data,
  "cleaned_retail_data.csv",
  row.names = FALSE
)

cat("\nProject analysis completed!\n")
cat("Cleaned dataset saved as cleaned_retail_data.csv\n")

