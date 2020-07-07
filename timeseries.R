library(forecast)
library(fUnitRoots)
data <- gas
# Australian monthly gas production
tail(data)

#plotting the data to see if there is trend
plot(data)

#checking if the data is stationary or non stationary by ADF test
#if p-value > 0.05, data is non-stationary else stationary.
#if data is non-stationary, change to stationary
adfTest(data)

#changing data to stationary
#differencing the data
plot(diff(data))

#checking again the p-value
adfTest(diff(data))

#running AR - I - MA test
ARIMAfit <- auto.arima(log10(data), approximation = TRUE, trace=TRUE)
summary(ARIMAfit)

#predicting the future values for three years i.e 36 month
pred <- predict(ARIMAfit, n.ahead = 36)
pred

#integral. showing the values
10^(pred$pred)

plot(forecast(ARIMAfit, h=36))

