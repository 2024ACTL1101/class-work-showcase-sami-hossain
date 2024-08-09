### CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```{r load-data}
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```{r data}
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:
$$
 E(R_i) = R_f + \beta_i (E(R_m) - R_f) 
$$
Where:

- \( E(R_i) \) is the expected return on the capital asset,
- \( R_f \) is the risk-free rate,
- \( \beta_i \) is the beta of the security, which represents the systematic risk of the security,
- \( E(R_m) \) is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```{r return}
df$amd_return <- as.numeric(0)
df$gspc_return <- as.numeric(0)

previous_price_amd <- df$AMD[1]
previous_price_gspc <-df$GSPC[1]
for (i in 2:nrow(df)) {
  df$amd_return[i] = (df$AMD[i] - previous_price_amd)/previous_price_amd * 100
  df$gspc_return[i] = (df$GSPC[i] - previous_price_gspc)/previous_price_gspc * 100
  previous_price_amd = df$AMD[i]
  previous_price_gspc = df$GSPC[i]
}
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```{r riskfree}
df$rf_return <- as.numeric(0)

for (i in 1:nrow(df)) {
  df$rf_return[i] <- ((1 + df$RF[i]/100)^(1/360) - 1) * 100
}
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```{r excess return}
df$amd_excess <- as.numeric(0)
df$gspc_excess <- as.numeric(0)

for (i in 2:nrow(df)) {
  df$amd_excess[i] <- df$amd_return[i] - df$rf_return[i]
  df$gspc_excess[i] <- df$gspc_return[i] - df$rf_return[i]
}
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```{r lm}
model <- lm(formula = amd_excess ~ gspc_excess, data = df)
summary(model)
```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**
The \(\beta\) derived from the data is determined to be 1.57, which can be interpreted as the estimated increase in the AMD excess return for every 1 unit increase in the S&P500 excess return. 

Since \(\beta\) is greater than 1, specifically 1.57, the AMD stock is considered to be 57% more volatile than the market.


#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```{r plot}
plot <- ggplot(df, aes(x = gspc_excess, y = amd_excess)) +
    geom_point() +
    geom_smooth(method = "lm", col = "blue") +
    labs(title = "Relationship between AMD and the market",
        x = "S&P500 Excess Return",
        y = "AMD Excess Return")

print(plot)
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.

*Hint: Calculate the daily standard error of the forecast ($s_f$), and assume that the annual standard error for prediction is $s_f \times \sqrt{252}$. Use the simple return average method to convert daily stock returns to annual returns if needed.*

$$
s_f = s_e \sqrt{1+\frac{1}{n}+\frac{(X_f-\bar{X})^2}{\sum_{i=1}^{n}(X_i-\bar{X})^2}}
$$

**Answer:**

```{r pi}
# Step 1: Convert Annual Returns to Daily Returns
sp500_daily <- 13.3/252
rf_daily <- ((1 + 5/100)^(1/360) - 1) * 100

# Step 2: Calculate Forecast AMD Excess Return
amd_daily_excess_forecast <- 0.11032 + 1.57 * (sp500_daily - rf_daily)
print(amd_daily_excess_forecast)

# Step 3: Calculate the Daily Standard Error of the Forecast
se <- 2.566
amd_excess_mean <- mean(df$amd_excess)
amd_excess_var <- var(df$amd_excess)
n <- nrow(df)

sef <- se * sqrt(1 + 1/n + (amd_daily_excess_forecast - amd_excess_mean)^2 / amd_excess_var)
print(sef)

# Step 4: Calculate the Annual Standard Error for Prediction
annual_sef <- sef * sqrt(252)
print(annual_sef)

# Step 5: Calculate Critical Value
critical_value <- qt(0.95, df = 1257)

# Step 6: Calculate Prediction Interval for Annual Excess Return
annual_excess_forecast <- amd_daily_excess_forecast * 252
print(annual_excess_forecast)

annual_pred_interval_lower_excess <- annual_excess_forecast - critical_value * annual_sef
annual_pred_interval_upper_excess <- annual_excess_forecast + critical_value * annual_sef
print(annual_pred_interval_upper_excess)
print(annual_pred_interval_lower_excess)

# Step 7: Calculate Annual Return
annual_rf <- 5  # Annual risk-free rate in percentage

annual_pred_interval_lower <- annual_pred_interval_lower_excess + annual_rf
annual_pred_interval_upper <- annual_pred_interval_upper_excess + annual_rf

print(annual_pred_interval_upper)
print(annual_pred_interval_lower)
```
With 90% certainty we can conclude that the annual return for AMD is between 115.39822% and -18.7597%, given that the risk free rate is 5% and the annual expected return for the market is 13.3%. From the prediction interval calculated, it is clear that AMD has a high level of uncertainty and volatility.

