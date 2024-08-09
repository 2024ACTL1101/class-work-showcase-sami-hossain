### Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```{r load-data}

# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```


##Plotting the Data
Plot the closing prices over time to visualize the price movement.
```{r plot}
plot(amd_df$date, amd_df$close,'l')
```


## Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```{r trading}
# Function which executes trading algorithm 1
trading_algorithm_1 <- function(amd_df) {
  amd_df$trade_type <- as.character("") # Keeps Track of Whether Stocks are Bought or Sold
  amd_df$costs_proceeds <- as.numeric(0)   # Keeps Track of Cash Outflow and Inflow
  amd_df$accumulated_shares <- as.numeric(0)  # Keeps track of Accumulated Shares
  
  # Initialize variables for trading logic
  previous_price <- 0
  share_size <- 100
  accumulated_shares <- 0
  
  for (i in 1:nrow(amd_df)) {
    # Setting Condition for Selling
    if (i + 1 > nrow(amd_df)) {
      amd_df$trade_type[i] <- 'sell'
      amd_df$costs_proceeds[i] <- amd_df$close[i] * accumulated_shares
      next
    }
    
    # Setting Conditions for Buying
    if (previous_price == 0) {
      amd_df$trade_type[i] <- 'buy'
    } else if (amd_df$close[i] < previous_price) {
      amd_df$trade_type[i] <- 'buy'
    }
    
    # Executing the buy
    if (amd_df$trade_type[i] == 'buy') {
      amd_df$costs_proceeds[i] <- -100 * amd_df$close[i]
      accumulated_shares <- accumulated_shares + 100
    }
    
    # Keeping track of total shares accumulated and the price
    amd_df$accumulated_shares[i] <- accumulated_shares
    previous_price = amd_df$close[i]
  }
  
  return(amd_df)
}

results_1 <- trading_algorithm_1(amd_df)
```


## Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```{r period}
# Define dates 
start_date <- as.Date('2023-01-01') 
end_date <- as.Date('2023-07-01') 

amd_df_filtered <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date, ]
```


## Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```{r}
# Function which calculates the profit or loss
profit_and_loss <- function(results) {
  profit <- sum(results$costs_proceeds)
  return(profit)
}

# Function which calculates the return on investment
return_on_investment <- function (profit, results) {
  capital_invested <- sum(results$costs_proceeds[results$costs_proceeds < 0])
  roi <- profit / capital_invested * -1
  return(roi)
}

profit_1 <- profit_and_loss(results_1)
roi_1 <- return_on_investment(profit_1, results_1)
```

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```{r option}
profit_taking_strategy <- function(amd_df, profit_take_percentage) {
  amd_df$trade_type <- as.character("") # Keeps Track of Whether Stocks are Bought or Sold
  amd_df$costs_proceeds <- as.numeric(0)   # Keeps Track of Cash Outflow and Inflow
  amd_df$accumulated_shares <- as.numeric(0)  # Keeps track of Accumulated Shares
  
  # Initialize variables for trading logic
  previous_price <- 0
  share_size <- 100
  accumulated_shares <- 0
  average_purchase_price <- 0
  total_cost <- 0
  
  for (i in 1:nrow(amd_df)) {
    # Calculate average purchase price
    if (accumulated_shares > 0) {
      average_purchase_price <- total_cost / accumulated_shares
    } else {
      average_purchase_price <- 0
    }
    
    # Setting Conditions for Selling
    if (i + 1 > nrow(amd_df)) {
      amd_df$trade_type[i] <- 'sell'
      amd_df$costs_proceeds[i] <- amd_df$close[i] * accumulated_shares
      next
    } else if (amd_df$close[i] > average_purchase_price * (1 + profit_take_percentage)
                && accumulated_shares > 0) {
      amd_df$trade_type[i] <- 'sell'
      sell_shares <- accumulated_shares * 0.5
      amd_df$costs_proceeds[i] <- amd_df$close[i] * sell_shares
      
      # Updating the Total Cost and Accumulated Shares
      total_cost <- total_cost - (average_purchase_price * sell_shares)
      accumulated_shares <- accumulated_shares - sell_shares
    }
    
    # Setting Conditions for Buying
    if (previous_price == 0) {
      amd_df$trade_type[i] <- 'buy'
    } else if (amd_df$close[i] < previous_price) {
      amd_df$trade_type[i] <- 'buy'
    }
    
    # Executing the Buy
    if (amd_df$trade_type[i] == 'buy') {
      amd_df$costs_proceeds[i] <- -100 * amd_df$close[i]
      accumulated_shares <- accumulated_shares + 100
      total_cost <- total_cost + -1 * amd_df$costs_proceeds[i]
    }
    
    # Updating Accumulated Shares on the Data Frame and Previous Price
    amd_df$accumulated_shares[i] <- accumulated_shares
    previous_price = amd_df$close[i]
  }
  
  return(amd_df)
}
```


## Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```{r}
results_2 <- profit_taking_strategy(amd_df_filtered, 0.2)
profit_2 <- profit_and_loss(results_2)
roi_2 <- return_on_investment(profit_2, results_2)

results_3 <- trading_algorithm_1(amd_df_filtered)
profit_3 <- profit_and_loss(results_3)
roi_3 <- return_on_investment(profit_3, results_3)
```
Analysing My Findings: 
On January 5, 2023, AMD CEO Lisa Su unveiled the new MI300 chip at CES in Las Vegas. This chip was the first to integrate a CPU, GPU, and memory into a single design, making it highly effective for AI training. During the first two quarters of 2023, AMD's stock prices saw a bullish rise, reflecting strong investor sentiment in artificial intelligence. 

Consequently, Trading Algorithm 1, which used a simple buy-and-hold strategy, outperformed the profit-taking strategy as it did not sell shares prematurely and held onto them until the end of the trading period. Trading Algorithm 1 earned approximately earned $53,000 more and had an ROI of 23% while the second strategy had an ROI of 13% for that trading period.


Sample Discussion: On Wednesday, December 6, 2023, AMD CEO Lisa Su discussed a new graphics processor designed for AI servers, with Microsoft and Meta as committed users. The rise in AMD shares on the following Thursday suggests that investors believe in the chipmaker's upward potential and market expectations; My first strategy earned X dollars more than second strategy on this day, therefore providing a better ROI.





