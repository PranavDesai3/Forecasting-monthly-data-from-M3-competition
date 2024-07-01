library(forecast)
library(Mcomp)

yin <- M3[[1910]]$x
yin
plot(yin, xlab = "Years", ylab = "Number of shipments", main = "Monthly Glass Containers Shipments from 1981 to 1991")

transformedseries <- log(yin)
plot(transformedseries, xlab = "Years", ylab = "Log Transformed Data", main = "Transformed Monthly Glass Containers Shipments Data from 1981 to 1991")

#Seasonal plot
ggseasonplot(transformedseries, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("data") +
  ggtitle("Seasonal plot: Glass containers shipments ")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


#1
fitAAA <- ets(transformedseries, model = "AAA", damped = FALSE)
summary(fitAAA)

#2
fitAAdA <- ets(transformedseries, model = "AAA", damped = TRUE)
summary(fitAAdA)
#AICc is better than previous model but is still higher than ANA model so we will not use this model

#3
fitANA <- ets(transformedseries, model = "ANA", damped = FALSE)
summary(fitANA)
#AICc is significantly better
plot(fitANA)

fitANA$fitted #forecast estimation, forecast value = last level value
plot(forecast(fitANA, h=18, level = c(80,90,95,99)))

plot(transformedseries, lwd = 2, main = "Actual observations and the fitted values of ETS model ANA")
legend("topright", c("Actual observations", "Fitted values"), lty=1, col = c(1,2),cex = 0.7)
lines(fitANA$fitted, col="red")
checkresiduals(fitANA)
#forecast is good enough

#4
fitMNA <- ets(transformedseries, model = "MNA", damped = FALSE)
summary(fitMNA) #Best model as per the AICc

plot(fitMNA)

fitMNA$fitted #forecast estimation, forecast value = last level value
plot(forecast(fitMNA, h=18, level = c(80,90,95,99)))

plot(transformedseries, lwd = 2, main = "Actual observations and the fitted values of ETS model 1")
legend("topright", c("Actual observations", "Fitted values"), lty=1, col = c(1,2),cex = 0.7)
lines(fitMNA$fitted, col="red")
#best forecast till now

#5
fitMAA <- ets(transformedseries, model = "MAA", damped = FALSE)
summary(fitMAA)

#6
fitMAdA <- ets(transformedseries, model = "MAA", damped = TRUE)
summary(fitMAdA)

#7
fitMNM <- ets(transformedseries, model = "MNM", damped = FALSE)
summary(fitMNM)

#8
fitMAM <- ets(transformedseries, model = "MAM", damped = FALSE)
summary(fitMAM)

#9
fitMAdM <- ets(transformedseries, model = "MAM", damped = TRUE)
summary(fitMAdM)

#10
fitMMM <- ets(transformedseries, model = "MMM", damped = FALSE)
summary(fitMMM)

#11
fitMMdM <- ets(transformedseries, model = "MMM", damped = TRUE)
summary(fitMMdM)

#12
fitMNN <- ets(transformedseries, model = "MNN", damped = FALSE)
summary(fitMNN)
#AICc is very poor so we can ignore this model

#13
fitMAN <- ets(transformedseries, model = "MAN", damped = FALSE)
summary(fitMAN)

#14
fitMAdN <- ets(transformedseries, model = "MAN", damped = TRUE)
summary(fitMAdN)

#15
fitMMN <- ets(transformedseries, model = "MMN", damped = FALSE)
summary(fitMMN)

#16
fitMMdN <- ets(transformedseries, model = "MMN", damped = TRUE)
summary(fitMMdN)

#17
fitAAdN <- ets(transformedseries, model = "AAN", damped = TRUE)
summary(fitAAdN)

#18
fitAAN <- ets(transformedseries,model = "AAN", damped = FALSE)
summary(fitAAN)

#19
fitANN <- ets(transformedseries, model = "ANN", damped = FALSE)
summary(fitANN)
plot(fitANN)
fitANN$components[1]
fitANN$fitted #forecast estimation, forecast value = last level value
plot(forecast(fitANN, h=18, level = c(80,90,95,99)))

plot(transformedseries, lwd = 2, main = "Actual observations and the fitted values of ETS model 1")
legend("topright", c("Actual observations", "Fitted values"), lty=1, col = c(1,2),cex = 0.7)
lines(fitANN$fitted, col="red")
#pretty bad forecast

#forecasting using model 17 - MNA
plot(forecast(fitMNA, h=18, level = c(80,90,95,99)))
lines(fitMNA$fitted, col="red")

qqnorm(fitMNM$residuals)
qqline(fitMNM$residuals, col = "red")

#Plot residuals---------------------------------------------------------------
cbind('Residuals' = residuals(fitMNM),
      'Forecast errors' = residuals(fitMNM,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")

checkresiduals(fitMNM)

#forecast---------------------------------------------------------------------
plot(yin,xlim=c(1980, 1992+3/4), ylim=c(1500,8000))
plot(forecast(fitMNM, level = c(80,90,95,99), h=18))
