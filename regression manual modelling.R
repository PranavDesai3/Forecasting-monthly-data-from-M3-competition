library(forecast)
library(Mcomp)
library(tseries)
library(ggplot2)
library(lmtest)

M3[[1910]]$description
str(M3[[1910]])

yin <- M3[[1910]]$x
yout <- M3[[1910]]$xx
plot(yin, xlab = "Years", ylab = "Number of shipments", main = "Monthly Glass Containers Shipments from 1981 to 1991")
length(yin)

#performing log transformation to smooth out the variance
transformedseries <- log(yin)
plot(transformedseries, xlab = "Years", ylab = "Log Transformed Data", main = "Transformed Monthly Glass Containers Shipments Data from 1981 to 1991")

ggseasonplot(transformedseries) +
  ggtitle(paste("Seasonal Plot:", M3[[1910]]$description))

ggsubseriesplot(transformedseries) +
  ggtitle(paste("Sub series plot:", M3[[1910]]$description))


decomp <- decompose(transformedseries)
plot(decomp)


#fitting for trend indicator.
fitTrend <- tslm(transformedseries ~ trend)
summary(fitTrend)
#adjusted R square = 0.09421; trend is significant but the adjusted R square value is very low compared to trend which again proves that the trend is not linear.

#fitting seasonality indicator
fitSeason <- tslm(transformedseries ~ season) 
summary(fitSeason)
#all seasons are showing significant p-values. the adjusted R sqaure is also quite high.

#adding season to trend to make it multiple regression
fitTrendandSeason <- tslm(transformedseries ~ trend + season)
summary(fitTrendandSeason)

#we are able to explain over 76% relation with this. All the seasons are showing significant p-values.


plot(transformedseries, lwd = 2, main = "Actual observations and the fitted values of trend and season combined")
legend("topright", c("Actual observations", "Fitted values"), lty=1, col = c("black","skyblue"),cex = 0.5)
lines(fitTrendandSeason$fitted, col = "skyblue")


# Forecast
#Forecast the future 18 months with the fitted model
plot(forecast(fitTrendandSeason, h=18,level = c(80,90,95,99)))
lines(log(yout), lty=2, lwd=2)

#-----------------------------------------------------------------------------
#Residual Analysis for Linearity
cbind(Fitted = fitTrendandSeason$fitted,Residuals=fitTrendandSeason$residuals) %>%
  as.data.frame() %>%
  ggplot(aes(x=Fitted, y=Residuals)) + geom_point()+
  labs(title="Plot of fitted values VS residuals")+
  theme(plot.title = element_text(hjust = 0.5))

#Residual Analysis for Normality 
shapiro.test(fitTrendandSeason$residuals) #p-value = 0.0006169 < 0.05, does not follow normality

#Residual Analysis for Equal Variance 
bptest(fitTrendandSeason) #p-value = 0.05121 > 0.05, does have constant variance

#Residual Analysis for Independence
dwtest(fitTrendandSeason)
#DW = 1.2298, p-value = 1.594e-05 

checkresiduals(fitTrendandSeason)
