Manual and Batch Forecasting for M3 Competition Data

Overview
This repository provides the complete implementation and analysis of manual and batch forecasting models using time series data from the M3 competition. The project evaluates the performance of three forecasting models: regression, Exponential Smoothing (ETS), and Auto-Regressive Integrated Moving Averages (ARIMA). The analysis reveals a slight preference for ETS models in batch forecasting scenarios. The repository includes all necessary code, data, and documentation to reproduce the forecasting models and their results.

Executive Summary
This report evaluates the effectiveness of regression, ETS, and ARIMA models for business forecasting using time series data from the M3 competition. The findings indicate that while each model captures data patterns to a significant extent, there is a slight preference for ETS models in batch forecasting scenarios. Despite the need for minor refinements due to residual anomalies, these models are instrumental for businesses in strategic decision-making and resource optimization. Continuous enhancement and application of these forecasting methods can provide a competitive edge in the marketplace.

Introduction
A forecast is a prognostication of future occurrences or events. Forecasting, a subset of business analytics, encompasses the integration of historical data analysis, statistical modelling, and predictive analytics. The use of an all-encompassing strategy is what enables the attainment of accurate predictions. Most forecasting problems need the utilisation of time series data, which can be defined as a sequential collection of data on a variable of interest that is organised in a chronological or time-oriented manner.

Analyzing time-oriented data and predicting future values of a time series are crucial challenges encountered by analysts across several fields. Forecasting plays a crucial role in guiding strategic decision-making by helping businesses match their actions with future market conditions. Accurate forecasting may optimize resource usage, leading to cost reduction and higher profit margins. Forecasting not only aids in evaluating and minimizing risks but also facilitates anticipatory analysis of customer preferences, thereby allowing the development of tailored products or services.

Manual Modelling
Data Exploration
For the manual modelling part, we explored the monthly time series with series ID: 1910 from the M3 competition, which corresponds to the last digit of the student ID. The time series data represents the shipments of glass containers from 1981 to 1992. The first 126 observations (January 1981 to June 1991) were used as in-sample data to build forecasting models. The remaining observations (July 1991 to December 1992) were used for out-of-sample forecasting to evaluate model effectiveness.

Regression
We built multiple regression models starting with trend indicators and iteratively adding seasonal indicators to find the best-fitting model. The final regression model, which included both trend and seasonality, achieved an adjusted R² value of 0.7616, explaining 76% of the variance in the target variable.

Exponential Smoothing (ETS)
We used the ets() function in R to build ETS models, iterating over various model parameters to find the best fit. The model with parameters "MNM" (multiplicative error, no trend, multiplicative seasonality) provided the best AICc value of -141.89.

Auto-Regressive Integrated Moving Averages (ARIMA)
The ARIMA models were built by iteratively adjusting parameters to minimize the AICc value. The best model had parameters order = c(1,1,2), seasonal = c(1,1,1) with an AICc of 1566.73.

Batch Modelling
Model Selection Strategy
We implemented a model selection strategy using cross-validation with rolling origin to select the best model (ETS or ARIMA) for each of the 100 time series. The strategy involved calculating the mean absolute percentage error (MAPE) for each model and selecting the model with the lower MAPE.

Cross Validation with Rolling Origin for Time Series
Cross-validation with rolling origin was employed to ensure that the test data was chronologically aligned with the training data. This method helped in preventing biased estimation of the true error.

Error Measure Selection
We used three error measures: MAPE, MPE, and MASE. MAPE was preferred due to its simplicity and effectiveness in situations where all data points are positive and significantly larger than zero.

Model Selection Result
The ETS model was selected 59 times, while the ARIMA model was selected 49 times. The differences in MAPEs were not substantial, indicating comparable performance between the two models. Combining forecasts from both models could potentially yield even better predictions.

Benchmarking
We compared the selected models' forecasts with actual values from the out-of-sample data using MAPE, MPE, and MASE. Benchmarking was done against seasonal naïve, simple moving averages, simple exponential smoothing, ETS, and ARIMA models.

Categorisation of Data
The 100 time series were categorized into Micro, Macro, and Industry. Analysis of these categories provided insights into the data characteristics and model performance across different types.

Trend and Season Analysis
We analyzed the trend and seasonality of each time series using the best-fitting ETS models. The series were categorized based on the presence of trend and seasonality, and their respective error measures were analyzed.

Horizon Analysis
Error measures for short-term, medium-term, and long-term horizons were analyzed to check the variation in model performance over different forecasting periods. The analysis showed no major shifts in MAPEs across different horizons.

Conclusion
Forecasting is critical for strategic business decision-making, as demonstrated by the comprehensive analysis of regression, ETS, and ARIMA models. Despite the complexity of forecasting, the models presented in this study can effectively predict future seasonal patterns and trends. The findings illustrate the predictive capability of ETS and ARIMA models, despite the possibility of model enhancement as indicated by residual statistical tests. Organizations can enhance the precision of their predictions, optimize resource distribution, mitigate risks, and adapt their products to align with consumer preferences through the meticulous application of these models. In a dynamic business environment, forecasting is vital to the success and competitiveness of an organization.