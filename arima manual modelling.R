library(forecast)
library(fpp)
yin <- M3[[1910]]$x
yin
yout <- M3[[1910]]$xx
yout

#step 1 - plot the graph and check for any unsual observations.
plot(yin)

transformedseries <- log(yin)
plot(transformedseries)

tsdisplay(yin, main="without any differencing")
#we can clearing the seasonality from the ACF plot

#step 2 - Data is not stationary so we will make it stationary. There is seasonality as well as a bit of downward trend so possibly 2 order of differencing.
seasonaldiff_data <- diff(yin, 12)
tsdisplay(diff(yin, 12))

#conductin unit tests to check if data is now stationary.
adf.test(diff(yin, 12)) #stationary result
kpss.test(diff(yin, 12)) #non-stationary result
#we can observe that kpss test is not returning high enough p-value, so we cannot call the stationary with just seasonal differencing.
#There is a significant spike at lag 9 in PACF graph and also a spike at lag 12 which suggest a seasonal AR(1) component and non-seasonal AR(9) component.

#we will do the second level of differencing to make the data stationary.
secondorderdiff <- diff(diff(yin, 12))
tsdisplay(diff(diff(yin, 12)))
#conducting unit tests to check if the data is now stationary.
adf.test(diff(diff(yin, 12)))#stationary
kpss.test(diff(diff(yin, 12)))#stationary

#double check if we have applied correct amount of differencing
nsdiffs(yin)
ndiffs(diff(yin, 12))


steps <- cbind("Original data" = yin,
               "Seasonal differences" = diff(yin, 12),
               "First & seasonal diffs" = diff(diff(yin, 12)))
autoplot(steps, facets=TRUE) +
  xlab("Time") +
  ggtitle("Glass containers shipments (1981-1991)")

#we will start fitting the model now. there is a significant spike at lag 1 2 and 4 for in the PACF model so we can start with a non seasonal AR(2), also there spike at lag 12 in both ACF and PACF graph suggest seasonal AR(1).

fit1 <- Arima(yin, order = c(2,1,0), seasonal = c(1,1,0))
summary(fit1)
#AICc = 1592.14
checkresiduals(fit1) #not white noise
Box.test(fit1$residuals, lag = 36, fitdf = 3,type = "Ljung-Box")


fit2 <- Arima(yin, order = c(2,1,0), seasonal = c(2,1,0))
summary(fit2)
#AICc = 1590.06, AICc slightly better
checkresiduals(fit2)

Box.test(fit2$residuals, lag = 36, fitdf = 4,type = "Ljung-Box")
tsdisplay(fit2$residuals)#not white noise



#Try to form a mixed model with some variations, it seems that model improved by adding MA terms
fit3 <- Arima(yin, order = c(2,1,1), seasonal = c(2,1,0))
summary(fit3)
#AICc = 1581.72, - AICc improved significantly.
checkresiduals(fit3)
Box.test(fit3$residuals, lag = 36, fitdf = 5,type = "Ljung-Box")
tsdisplay(fit3$residuals)

#adding MA term to the seasonal component as well
fit4 <- Arima(yin, order = c(2,1,1), seasonal = c(2,1,1))
summary(fit4)
#AICc = 1572.64,  - AICc improved significatly
checkresiduals(fit4)#not white noise
Box.test(fit4$residuals, lag = 36, fitdf = 6,type = "Ljung-Box")
tsdisplay(fit4$residuals)

#reducing the AR component to check if we can achieve a better model
fit5 <- Arima(yin, order = c(1,1,1), seasonal = c(2,1,1))
summary(fit5)
#AICc = 1571.47, AICc improved slightly
checkresiduals(fit5)#not white noise
Box.test(fit5$residuals, lag = 36, fitdf = 5,type = "Ljung-Box")
tsdisplay(fit5$residuals)  
  
#reducing AR component from seasonal to check if we can achieve a better model
fit6 <- Arima(yin, order = c(1,1,1), seasonal = c(1,1,1))
summary(fit6)
#AICc = 1569.38, AICc improved slightly
checkresiduals(fit6)#not white noise
tsdisplay(fit6$residuals)  
Box.test(residuals(fit6),lag = 36, fitdf = 4,type = "Ljung-Box")


#since adding the MA component the AICc had improved signifinctly , lets try adding that to the model now.

fit7 <- Arima(yin, order = c(1,1,2), seasonal = c(1,1,1))#best model
summary(fit7)
#AICc = 1566.73, AICc improved slightly
checkresiduals(fit7)# white noise
ggtsdisplay(residuals(fit7))
tsdisplay(fit7$residuals)
Box.test(residuals(fit7),lag = 36, fitdf = 5,type = "Ljung-Box") #p-value = 0.0001496 < 0.05; not white noise

#lets try adding MA term to the seasonal part now.
fit8 <- Arima(yin, order = c(1,1,2), seasonal = c(1,1,2))
summary(fit8)
#AICc = 1569, - AICc decreased, not a good thing to add MA component in seasonal.
checkresiduals(fit8)#not white noise
Box.test(residuals(fit8),lag = 36, fitdf = 6,type = "Ljung-Box")
tsdisplay(fit8$residuals)

#reducing the AR term further to check if the model can be improved.
fit9 <- Arima(yin, order = c(0,1,2), seasonal = c(1,1,1))
summary(fit9)
#AICc = 1571.52, AICc went up again so its not a good choice to reduce AR component in order.

#lets try if AR component in seasonal can be reduced.
fit10 <- Arima(yin, order = c(1,1,2), seasonal = c(0,1,1))
summary(fit10)
#AICc = 1577.08, AICc went up significantly so its not a good choice to reduce AR component in order.

#From all the above models, we can infer that model 7 has given us the best AICc and hence can be chosen for forecasting.
plot(yin,xlim=c(1980, 1992+3/4), ylim=c(1500,8000))
plot(forecast(fit7, h=18, level = c(80,90,95,99)))
lines(yout, lty="dashed")

#-----------------------------------------------------------------------------------------------------
fit <- Arima(log(yin), order = c(0,1,1), seasonal = c(1,1,1))
summary(fit)
#still does not satisfy
checkresiduals(fit)
Box.test(residuals(fit),lag = 36, fitdf = 3,type = "Ljung-Box")
