# Restaurant Drink Demand Forecasting using Prophet in R

## Project Overview

This project uses the R programming language and the `prophet` library, developed by Meta, to forecast the daily drink demand for a bar. The model accounts for complex seasonal patterns, including weekly and yearly trends, as well as the specific effects of public holidays in Quebec, Canada.

The primary goal is to create a reliable forecast that can assist with operational planning, such as inventory management. This repository contains a self-contained R script that generates its own realistic, synthetic data for demonstration purposes, making the analysis fully reproducible without exposing sensitive sales data.

---

## Features

* **365-Day Forecasting:** Generates a daily forecast for one full year into the future.
* **Complex Seasonality:** Automatically models and decomposes weekly and yearly sales cycles.
* **Custom Holiday Modeling:** Incorporates a custom list of Canadian and Quebec-specific public holidays to improve accuracy on irregular days.
* **Model Accuracy Metrics:** Calculates the in-sample **Mean Absolute Error (MAE)** to provide a clear measure of the model's fit on the historical data.
* **Rich Visualizations:**
    * Creates a primary forecast plot showing historical data, the future forecast, and uncertainty intervals.
    * Generates a components plot to visualize the underlying trend, holiday effects, and seasonal patterns.
    * Produces an interactive forecast plot using `plotly`.
* **Data Export:** Saves the final forecast results to a clean `drink_demand_forecast.csv` file for use in other applications or reports.

---
![Screenshot 2025-06-21 004712](https://github.com/user-attachments/assets/45fc1cd6-07d3-407b-b8ae-d537368428af)
![Screenshot 2025-06-21 004747](https://github.com/user-attachments/assets/c7c12196-e64f-4898-93bc-82527ec0dc72)
