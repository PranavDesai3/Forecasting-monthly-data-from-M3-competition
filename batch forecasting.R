library(forecast)
library(smooth)
library(Mcomp)
library(tseries)
library(parallel)
library(foreach)
library(doSNOW)
library(dplyr)
library(doParallel)

#loading the entire M3 time series data.
entire_timeseries <- lapply(M3, I)

#removing N from the start of the time series names to make it only numeric.
timeseries_numbers_separated <- sub("^N", "", names(entire_timeseries))

#setting the range from which the time series need to be taken
timeseries_range <- timeseries_numbers_separated > 1500 & timeseries_numbers_separated <= 2500

#storing only the timeseries which are ending with 9.
data <- entire_timeseries[timeseries_range][grep("9$", names(entire_timeseries[timeseries_range]))] 
length(data)

#parallel computing set up
#Detect the number of core and define cluster
cores <- detectCores()
cl <- makeCluster(cores-3, type = "SOCK")
registerDoSNOW(cl)

#Setting up variables for Cross-validation, model selection, and error calculation for selected model
horizon <- 18
MAPEs_ETS_CV <- MAPEs_Arima_CV <- data.frame()
MAPE_Choice <- MPE_Choice <- MASE_Choice <- data.frame()
preferred_model <-c()
ets_characteristics <- matrix(NA, 100, 5)
arima_characteristics <- matrix(NA, 100, 7)

#checking the length of all the timeseries that we have to forecast
for(i in 1:length(data)){
  print(data[[i]]$n)
}

#we can observe that some timeseries are very short and some are long which is
#why we have to set our starting point of the series dynamically making sure 
#we have enough observartions to implement cross validation

for(i in 1:length(data)){
    
    print(paste("Timeseries in process: ", i))
    print(paste("Timeseries number: ", data[[i]]$sn))
    
    horizon <- 18
    yin <- data[[i]]$x
    yout <- data[[i]]$xx
    ytlen = data[[i]]$n - horizon

    #checking the test data length of series before deciding the origin
    if(ytlen < 40){
      ytstart <- 25 
    }else if(ytlen < 80){
      ytstart <- 49
    }else{
      ytstart <- 61
    }
    
    #setting the origins
    origins <- ytstart:ytlen
    print(ytstart:ytlen)
    
    Cross_Validation <- foreach(origin=origins, .combine = 'cbind', .packages = 'forecast') %dopar% {
      yt <- head(yin, origin)
      yv <- yin[(origin+1):(origin+horizon)]
      
      #ETS
      fit_ets_cv <- ets(yt)
      fcs_ets_cv <- forecast(fit_ets_cv, h=horizon)$mean
      
      MAPEs_ETS <- mean(abs(yv - fcs_ets_cv)/yv)*100
      
      #ARIMA
      fit_arima_cv <- auto.arima(yt, method = "CSS")
      fcs_arima_cv <- forecast(fit_arima_cv, h=horizon)$mean
      MAPEs_ARIMA <- mean(abs(yv - fcs_arima_cv)/yv)*100
      
      list(MAPEs_ETS, MAPEs_ARIMA)
    }
    
    MAPEs_ETS_CV <- bind_rows(MAPEs_ETS_CV,Cross_Validation[1,])
    MAPEs_Arima_CV <- bind_rows(MAPEs_Arima_CV, Cross_Validation[2,])
    
    #Calculating Naive method's MAE for the scaling denominator of MASE
    fit_naive <- naive(yin, h = horizon)
    naive_fcs <- forecast(fit_naive)$mean
    
    #compare the MAPE of ETS and ARIMA model
    if (rowMeans(MAPEs_ETS_CV[i,],na.rm = TRUE) > rowMeans(MAPEs_Arima_CV[i,],na.rm = TRUE)){
      fit <- auto.arima(yin)
      #Select ARIMA
      preferred_model = c(preferred_model,"ARIMA")
      
      #Storing characteristics of ARIMA fit
      #check the characteristics of data for further forecast evaluations
      arima_characteristics[i,1] <- data[[i]]$type
      #check the time series characteristics (order) for further forecast evaluations
      arima_characteristics[i,2] <- as.numeric(fit$arma[1])
      arima_characteristics[i,3] <- as.numeric(fit$arma[6])
      arima_characteristics[i,4] <- as.numeric(fit$arma[2])
      #check the time series characteristics (seasonal) for further forecast evaluations
      arima_characteristics[i,5] <- as.numeric(fit$arma[3])
      arima_characteristics[i,6] <- as.numeric(fit$arma[7])
      arima_characteristics[i,7] <- as.numeric(fit$arma[4])
      
      #create ARIMA forecast for out-sample-data and calculate the accuracy of each horizon
      fcs <- forecast(fit, h=horizon)$mean
      MAPE_Choice[i, 1:horizon]<- 100 *mean((abs(yout - fcs)/abs(yout)))
      MPE_Choice[i, 1:horizon]<- 100 *mean(((yout - fcs)/(yout)))
      MASE_Choice[i, 1:horizon]<- (abs(yout - fcs))/mean(abs(yout - fcs))
      
    }else{
      fit <- ets(yin)
      #Select ETS
      preferred_model = c(preferred_model,"ETS")
      
      #Storing characteristics of ETS fit
      #--------------add error component to this matrix-------------
      #check the characteristics of data for further forecast evaluations
      ets_characteristics[i,1] <- data[[i]]$type
      #check the time series characteristics (error) for further forecast evaluations
      ets_characteristics[i,2] <- fit$component[1] #Error -> Additive or Multiplicative
      #check the time series characteristics(trend/ seasonality) for further forecast evaluations
      ets_characteristics[i,3] <- fit$component[2] #Trend
      ets_characteristics[i,4] <- fit$component[3] #Seasonality
      ets_characteristics[i,5] <- as.logical(fit$component[4])  #Damped
      
      #create ETS forecast for out-sample-data and calculate the accuracy of each horizon
      fcs <- forecast(fit, h=horizon)$mean
      MAPE_Choice[i,1:horizon]<- 100 *mean((abs(yout - fcs)/abs(yout)))
      MPE_Choice[i,1:horizon]<- 100 *mean(((yout - fcs)/(yout)))
      MASE_Choice[i, 1:horizon]<- (abs(yout - fcs))/mean(abs(yout - naive_fcs))
    }
    
}
stopCluster(cl)
#check how many ETS or ARIMA models were selected for the 100 time series
table(preferred_model)

#-----------MAPE plot (cross Validation output)---------------------------------
#ETS generally has better performance forecast compared to ARIMA, and in this run we have got 41 ARIMA models compared to 59 ETS
plot(1:48,type = "n",
     main = "Average training set MAPE of 100 time series",
     xlab="Rolling Origins", ylab="MAPE %",
     ylim=c(0,40))
legend("topright",legend=c("ETS","ARIMA"),col= c("red", "blue"),lty=1, cex = 0.5)
lines(1:48, colMeans(MAPEs_ETS_CV, na.rm = TRUE), type="l",col= "red")
lines(1:48, colMeans(MAPEs_Arima_CV, na.rm = TRUE), type="l",col= "blue")

#----------Benchmark methods----------------------------------------------------
horizon = 18
MAPE_snaive_fcs <- MAPE_sma_fcs <- MAPE_ses_fcs <- MAPE_ets_fcs <- MAPE_autoarima_fcs <- matrix(NA, nrow = length(data), ncol = horizon)
MPE_snaive_fcs <- MPE_sma_fcs <- MPE_ses_fcs <- MPE_ets_fcs <-MPE_autoarima_fcs <- matrix(NA, nrow = length(data), ncol = horizon)
MASE_snaive_fcs <- MASE_sma_fcs<- MASE_ses_fcs <- MASE_ets_fcs <- MASE_autoarima_fcs <- matrix(NA,nrow = length(data), ncol = horizon)

for(i in 1:length(data)){
  print(paste("Timeseries in process: ", i))
  print(paste("Timeseries number: ", data[[i]]$sn))
  
    yin <- data[[i]]$x
    yout <- data[[i]]$xx
   
    #Naive method
    naivefit <- naive(yin, h = horizon)
    naive_fcs <- forecast(naivefit)$mean
    
    #Seasonal Naive method
    snaivefit <- snaive(yin, h = horizon)
    snaive_fcs <- forecast(snaivefit)$mean
    
    #Moving Average method with k = 12
    smafit <- sma(yin, order = 12, h= horizon)
    sma_fcs <- smafit$forecast
    
    #Simple Exponential Smoothing
    sesfit <- ses(yin, h = horizon)
    ses_fcs <- forecast(sesfit)$mean
    
    #ETS
    etsfit <- ets(yin)
    ets_fcs <- forecast(etsfit, h = horizon)$mean
    
    #auto-ARIMA
    autoarimafit <- auto.arima(yin)
    autoarima_fcs <- forecast(autoarimafit, h = horizon)$mean
    
    # Calculate MAPE and MPE
    MAPE_snaive_fcs[i, ] <- 100 * mean(((abs(yout - snaive_fcs) / abs(yout))))
    MPE_snaive_fcs[i, ] <- 100 * mean((((yout - snaive_fcs) / yout)))
    MASE_snaive_fcs[i, ] <- (abs(yout - snaive_fcs))/mean(abs(yout - naive_fcs))
    
    MAPE_sma_fcs[i, ] <- 100 * mean(((abs(yout - sma_fcs) / abs(yout))))
    MPE_sma_fcs[i, ] <- 100 * mean((((yout - sma_fcs) / yout)))
    MASE_sma_fcs[i, ] <- (abs(yout - sma_fcs))/mean(abs(yout - naive_fcs))
    
    MAPE_ses_fcs[i, ] <- 100 * mean(((abs(yout - ses_fcs) / abs(yout))))
    MPE_ses_fcs[i, ] <- 100 * mean((((yout - ses_fcs) / yout)))
    MASE_ses_fcs[i, ] <- (abs(yout - ses_fcs))/mean(abs(yout - naive_fcs))
    
    MAPE_ets_fcs[i, ] <- 100 * mean(((abs(yout - ets_fcs) / abs(yout))))
    MPE_ets_fcs[i, ] <- 100 * mean((((yout - ets_fcs) / yout)))
    MASE_ets_fcs[i, ] <- (abs(yout - ets_fcs))/mean(abs(yout - naive_fcs))
    
    MAPE_autoarima_fcs[i, ] <- 100 * mean(((abs(yout - autoarima_fcs) / abs(yout))))
    MPE_autoarima_fcs[i, ] <- 100 * mean((((yout - autoarima_fcs) / yout)))
    MASE_autoarima_fcs[i, ] <- (abs(yout - autoarima_fcs))/mean(abs(yout - naive_fcs))
    
}

#-----------------------------------------------------------------------------
#taking averages for each series
averageMAPE_Choice <- rowMeans(MAPE_Choice)
averageMPE_Choice <- rowMeans(MPE_Choice)
averageMASE_Choice <- rowMeans(MASE_Choice)
averageMAPE_snaive <- rowMeans(MAPE_snaive_fcs)
averageMPE_snaive <- rowMeans(MPE_snaive_fcs)
averageMASE_snaive <- rowMeans(MASE_snaive_fcs)
averageMAPE_sma <- rowMeans(MAPE_sma_fcs)
averageMPE_sma <- rowMeans(MPE_sma_fcs)
averageMASE_sma <- rowMeans(MASE_sma_fcs)
averageMAPE_ses <- rowMeans(MAPE_ses_fcs)
averageMPE_ses <- rowMeans(MPE_ses_fcs)
averageMASE_ses <- rowMeans(MASE_ses_fcs)
averageMAPE_ets <- rowMeans(MAPE_ets_fcs)
averageMPE_ets <- rowMeans(MPE_ets_fcs)
averageMASE_ets <- rowMeans(MASE_ets_fcs)
averageMAPE_autoarima <- rowMeans(MAPE_autoarima_fcs)
averageMPE_autoarima <- rowMeans(MPE_autoarima_fcs)
averageMASE_autoarima <- rowMeans(MASE_autoarima_fcs)

#------------Categorisation------------------------------------------------------------------
# Initialize lists to store error measures for different categories and models
error_measures <- list(
  MAPE = list(),
  MPE = list(),
  MASE = list()
)

models <- c("Choice", "snaive", "sma", "ses", "ets", "autoarima")  # Define models

for (i in 1:length(data)) {
  category <- data[[i]]$type  # Extract the type of the time series
  if (!(category %in% names(error_measures$MAPE))) {
    error_measures$MAPE[[category]] <- list()
    error_measures$MPE[[category]] <- list()
    error_measures$MASE[[category]] <- list()
  }
  
  for (model in models) {

      error_measures$MAPE[[category]][[model]][i] <- get(paste0("averageMAPE_", model))[i]
      error_measures$MPE[[category]][[model]][i] <- get(paste0("averageMPE_", model))[i]
      error_measures$MASE[[category]][[model]][i] <- get(paste0("averageMASE_", model))[i]
    
  }
}

#Analyse the MAPE, MPE, and MASE values for each category for different models
# Initialize matrices to store means
num_models <- length(models)
categories <- c("MICRO", "INDUSTRY", "MACRO")
num_categories <- length(categories)

MAPE_means <- matrix(NA, nrow = num_models, ncol = num_categories)
MPE_means <- matrix(NA, nrow = num_models, ncol = num_categories)
MASE_means <- matrix(NA, nrow = num_models, ncol = num_categories)


# Loop through models and categories to calculate means
for (i in 1:num_models) {
  for (j in 1:num_categories) {
    model <- models[i]
    category <- categories[j]
    
    # Calculate mean for MAPE
    MAPE_means[i, j] <- mean(error_measures$MAPE[[category]][[model]], na.rm = TRUE)
    
    # Calculate mean for MPE
    MPE_means[i, j] <- mean(error_measures$MPE[[category]][[model]], na.rm = TRUE)
    
    # Calculate mean for MASE
    MASE_means[i, j] <- mean(error_measures$MASE[[category]][[model]], na.rm = TRUE)
  }
}
#-------------------------------------------------------------------------------
#---------Horizon analysis------------------------------------------------------
horizons_list <- c(6,12,18)
error_measures_horizon <- list()

for (horizon in horizons_list) {
  print(paste("Horizon: ", horizon))
  
  horizon_choice_fc <- horizon_snaive_fc <- horizon_sma_fc <- horizon_ses_fc <- horizon_ets_fc <-   horizon_autoarima_fc <- list()
  
  for (i in 1:length(data)) {
    print(paste("Time series: ", i))
    yin <- data[[i]]$x
    yout <- data[[i]]$xx
    
    if (preferred_model[i] == "ETS") {
      best_fit <- ets(yin, model = paste(ets_characteristics[i,2],
                                         ets_characteristics[i,3],
                                         ets_characteristics[i,4],
                                         sep = ""),
                      damped = as.logical(ets_characteristics[i,5]))
      
      horizon_fc <- forecast(best_fit, h = horizon)$mean
    }else{
      
      best_fit <- Arima(yin, order = c(as.numeric(arima_characteristics[i,2]),
                                       as.numeric(arima_characteristics[i,3]),
                                       as.numeric(arima_characteristics[i,4])),
                        seasonal = c(as.numeric(arima_characteristics[i,5]),
                                     as.numeric(arima_characteristics[i,6]),
                                     as.numeric(arima_characteristics[i,7])),
                        method = "CSS")
      
      horizon_fc <- forecast(best_fit, h = horizon)$mean
    }
    
    #Setting all the forecasts of different horizons
    horizon_choice_fc[[i]] <- horizon_fc
    horizon_snaive_fc[[i]] <- forecast(snaive(yin, h = horizon))$mean
    horizon_sma_fc[[i]] <- forecast(sma(yin, order = 12, h = horizon))$mean
    horizon_ses_fc[[i]] <- forecast(ses(yin, h = horizon))$mean
    horizon_ets_fc[[i]] <- forecast(ets(yin), h = horizon)$mean
    horizon_autoarima_fc[[i]] <- forecast(auto.arima(yin, method = "CSS"), h = horizon)$mean
    
  }
  
  horizon_all_models <- list(horizon_choice_fc, horizon_snaive_fc, horizon_sma_fc, horizon_ses_fc, horizon_ets_fc, horizon_autoarima_fc)
  
  horizon_fc_MAPE <- horizon_fc_MASE <- list()
  
  for (i in 1:length(horizon_all_models)) {
    
    horizon_fc_MAPE[[i]] <- horizon_fc_MASE[[i]] <- numeric(length(data))
    
    for (j in 1:length(data)) {
      yout <- data[[j]]$xx
      
      horizon_fc_MAPE[[i]][j] <- 100 * mean((abs(yout - horizon_all_models[[i]][[j]])/abs(yout)))
      horizon_fc_MASE[[i]][j] <- mean(abs(yout - horizon_all_models[[i]][[j]]))/mean(abs(yout - horizon_all_models[[2]][[j]]))
    }
    
  }
  
  error_measures_horizon[[paste0("horizon", horizon)]] <- list(MAPE = horizon_fc_MAPE, MASE = horizon_fc_MASE)
  
}
error_measures_horizon$horizon6$MASE

#----------------------------------------------------------------------------------------------
#------Trend and Seasonality Analysis----------------------------------------------------------
trend_seasonality_char <- matrix(NA, 100, 3)
#creating a list of all the trend and seasonality conditions
tsconditions <- list(c(TRUE, TRUE, FALSE),    #Trend+Seasonality
                     c(TRUE, TRUE, TRUE),     #Damped Trend+Seasonality
                     c(TRUE, FALSE, TRUE),    #Damped Trend only
                     c(FALSE, FALSE, FALSE),   #Trend only
                     c(FALSE, TRUE, FALSE),   #Seasonality only
                     c(FALSE, FALSE, FALSE))  #No Trend/Seasonality

ts_characteristic_table <- matrix(NA,nrow = 6,ncol = 6)
colnames(ts_characteristic_table) <- c("Preferred Model",
                                       "Seasonal Naive",
                                       "Simple Moving Average",
                                       "Simple Exponential Smoothing",
                                       "ETS",
                                       "Auto ARIMA")
rownames(ts_characteristic_table) <- c("Trend + Seasonality",
                                       "Damped Trend + seasonality",
                                       "Damped trend",
                                       "Trend only",
                                       "Seasonality only",
                                       "No Trend, No Seasonality")

for (i in 1:length(data)){
  print(paste("Time series: ", i))
  yin <- data[[i]]$x
  fit_ets <- ets(yin)
  trend_seasonality_char[i,1] <- fit_ets$component[2] != "N" # Trend
  trend_seasonality_char[i,2] <- fit_ets$component[3] != "N" # Seasonality
  trend_seasonality_char[i,3] <- as.logical(fit_ets$component[4]) # Damped
}

for (k in 1:length(tsconditions)){
    condition <- tsconditions[[k]]
    filter_data <- 
      trend_seasonality_char[,1] == condition[1] & 
      trend_seasonality_char[,2] == condition[2] & 
      trend_seasonality_char[,3] == condition[3]
    
    MAPE_Choice_filter <- MAPE_Choice[filter_data, ]
    MAPE_snaive_filter <- MAPE_snaive_fcs[filter_data, ]
    MAPE_sma_filter <- MAPE_sma_fcs[filter_data, ]
    MAPE_ses_filter <- MAPE_ses_fcs[filter_data, ]
    MAPE_ets_filter <- MAPE_ets_fcs[filter_data, ]
    MAPE_autoarima_filter <- MAPE_autoarima_fcs[filter_data, ]
    
    ts_characteristic_table[1,k] <- mean(rowMeans(MAPE_Choice_filter, na.rm = TRUE))
    ts_characteristic_table[2,k] <- mean(rowMeans(MAPE_snaive_filter, na.rm = TRUE))
    ts_characteristic_table[3,k] <- mean(rowMeans(MAPE_sma_filter, na.rm = TRUE))
    ts_characteristic_table[4,k] <- mean(rowMeans(MAPE_ses_filter, na.rm = TRUE))
    ts_characteristic_table[5,k] <- mean(rowMeans(MAPE_ets_filter, na.rm = TRUE))
    ts_characteristic_table[6,k] <- mean(rowMeans(MAPE_autoarima_filter, na.rm = TRUE))
  }

#---------------------------------------------------------------------------------------------------