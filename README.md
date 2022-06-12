# GDP-time-series-analysis
Modeling GDP with Exogenous Variables

## Objective: 
The primary objective of this project is to analyze the explanatory powers of various economic indicators on real GDP of the United States. This analysis of the GDP has implications for economists around the world as many of the same indicators are published across countries. GDP is often used as a measure of comparison across countries. Additionally, GDP is used as a determining factor for a recession. The real GDP is recorded on a quarterly basis, which is less frequent than the exogenous indicators we used. Our initial expectation is that the monthly values for these independent variables will be able to accurately explain the real GDP for the quarter that the values are contained within. In order to solve this periodicity difference we will use a simple interpolation for the monthly GDP values. The factors we believe will accurately explain the real GDP which we are testing in this analysis are the Consumer Price Index (CPI), Producer Price Index (PPI), Purchasing Manager Index (PMI), unemployment rate, 10-year T-bond returns, retail loan rates, retail sales, housing sales, oil prices, S&P 500, NASDAQ, and VIX. These indicators measure various aspects of the economy and should have different levels of impact on GDP through time. It is likely that the explanatory power of these variables will change through time. We will run these regressions on the historical data using a sequence of 5 years windows in order to determine the change in the Beta’s of the variables across time.

## Data used:
Consumer Price Index (CPI): Even though GDP is already adjusted for inflation, we believe that CPI might still be significant in explaining GDP returns at different time periods.

Producer Price Index (PPI): Increasing producer prices serve as a leading indicator for inflation in that the increased costs will be passed down to consumers.

Purchasing Manager’s Index (PMI): Since PMI represents the manufacturing sector, when PMI increases, it means that there is growth in the manufacturing sector, which in turn would positively impact GDP.

Brent Oil: Oil prices have a vital impact of GDP. Since for most countries import and export has a huge impact of GDP growth rate.

VIX: We believe that there is a negative correlation between GDP and VIX. During times of economic downfall and uncertainty VIX tends to rise and during good times (bull period) VIX tends to be low.

NASDAQ: As an equity market with an emphasis on the technology sector, if the NASDAQ increases, it generally reflects good economic conditions, and therefore it should have a positive relationship with GDP.

S&P500: The equity markets often reflect economic conditions broadly and will decrease in crisis periods and increase in growth periods. The same relationship is expected for GDP.

Retail Sales: The sales of retail products are an input into GDP. For this reason, it is likely that Retail sales will have a positive relationship with GDP.

Housing Index: Similarly to retail sales, housing index is also an input of GDP. Therefore, it also has a positive relationship with GDP.

Unemployment: We believe that there will be a negative correlation between GDP and unemployment since as more people are unemployed, it generally reflects bad economic conditions, and therefore a decrease in GDP.

Prime Loan: Prime Loan Rate is the interest rate at which banks lend to customers. An increase in the bank prime loan rate will in turn decrease the purchasing power of people. This, in turn, will negatively affect GDP.

T-Bond (10y): The 10-year t-bond is expected to have a positive relationship with GDP because rates often increase in a strong economy and decrease in a bad economy as the FED implements monetary policy.

## Conclusion
In this project, various time series analysis techniques were utilized to analyze macroeconomic factors and their relationship to the United States real GDP across time. Based on the nature of the data obtained, proper time series data exploration and pre-processing techniques like decomposition, stationarity transformation, skewness, and auto-correlation tests were used to ensure the model assumptions were met on the broadest possible level; ARIMA model with exogenous variables (ARIMAX)
model was chosen to fulfill the research objective. Candidate model specifications were tested iteratively for model adequacy based on residual analysis. Three final models with identical specifications were then fitted to the data sequentially. Model results allowed us to uncover the temporal changes in the relationship between a particular macroeconomic variable and GDP. Furthermore, some indirect, hidden channels of effects of variables were discovered statistically by comparing results across the 3 models, which provide direction for further economic research aiming to examine the logic behind these observations.
