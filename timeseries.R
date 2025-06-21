# Load necessary libraries
library(prophet)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly) 

# Read data from  Excel file
excel_file_path <- "Daily Sales.xlsx"
df <- read_excel(excel_file_path, sheet = 1)

# Prophet requires a data frame with 'ds' (Date) and 'y' (value) columns.
df <- df %>%
  rename(ds = Date, y = Sales) %>%  
  mutate(ds = as.Date(ds)) %>%      
  filter(!is.na(ds), !is.na(y)) %>% 
  arrange(ds)                       

# Display the first few rows to confirm the data is loaded correctly
print("Data head:")
print(head(df))

# Create a data frame of past and future holidays for Canada and Quebec.
holidays <- data.frame(
  holiday = c(rep('New Year Day', 6), rep('Good Friday', 6), rep('National Patriots Day', 6), 
              rep('Saint-Jean-Baptiste Day', 6), rep('Canada Day', 6), rep('Labour Day', 6),
              rep('Thanksgiving', 6), rep('Christmas Day', 6)),
  ds = as.Date(c(
    # New Year's Day
    '2021-01-01', '2022-01-01', '2023-01-01', '2024-01-01', '2025-01-01', '2026-01-01',
    # Good Friday
    '2021-04-02', '2022-04-15', '2023-04-07', '2024-03-29', '2025-04-18', '2026-04-03',
    # National Patriots' Day (Quebec)
    '2021-05-24', '2022-05-23', '2023-05-22', '2024-05-20', '2025-05-19', '2026-05-18',
    # Saint-Jean-Baptiste Day (Quebec)
    '2021-06-24', '2022-06-24', '2023-06-24', '2024-06-24', '2025-06-24', '2026-06-24',
    # Canada Day
    '2021-07-01', '2022-07-01', '2023-07-01', '2024-07-01', '2025-07-01', '2026-07-01',
    # Labour Day
    '2021-09-06', '2022-09-05', '2023-09-04', '2024-09-02', '2025-09-01', '2026-09-07',
    # Thanksgiving
    '2021-10-11', '2022-10-10', '2023-10-09', '2024-10-14', '2025-10-13', '2026-10-12',
    # Christmas Day
    '2021-12-25', '2022-12-25', '2023-12-25', '2024-12-25', '2025-12-25', '2026-12-25'
  )),
  lower_window = 0,
  upper_window = 0
)

print("Holidays data frame:")
print(holidays)

# --- 4. Model Fitting and Forecasting ---

# The Prophet model is fitted to the historical data, including holidays.
m <- prophet(df, holidays = holidays)

# We then generate a forecast for the next 365 days.
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)

print("Tail of Forecast:")
print(tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]))


# We calculate the Mean Absolute Error (MAE) to evaluate how well the model fits the historical data. MAE is a robust metric that is not affected by zero-sales days.
comparison_df <- forecast %>%
  select(ds, yhat) %>%
  left_join(df, by = "ds")

# Filter for the historical period where we have actual sales
comparison_df <- comparison_df %>%
  filter(!is.na(y))

# Calculate MAE
mae <- mean(abs(comparison_df$y - comparison_df$yhat), na.rm = TRUE)

# Print the result in a formatted string
cat(sprintf("\nModel Fit (In-Sample) MAE: %.2f\n", mae))
cat("This means the model's predictions on the historical data were, on average, off by this many drinks.\n\n")


# Main Forecast Plot 
print(
  plot(m, forecast) +
    add_changepoints_to_plot(m) +
    ggtitle("Drink Demand Forecast (Next Year)") +
    xlab("Date") +
    ylab("Drinks Sold") +
    theme_minimal()
)

# Forecast Components Plot 
print(prophet_plot_components(m, forecast))

# Create an interactive Forecast Plot 
print(ggplotly(plot(m, forecast)))

# Exporting Results in CSV format
forecast_to_save <- forecast %>%
  select(ds, yhat, yhat_lower, yhat_upper)

write.csv(forecast_to_save, "drink_demand_forecast.csv", row.names = FALSE)


