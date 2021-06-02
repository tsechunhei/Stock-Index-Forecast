library(forecast)
library(tseries)
library(xts)

df = read.csv("Jakarta.csv")
tdf = ts(df$Ln.return)
auto.arima(tdf, seasonal = FALSE, test='adf', ic='aic')
Arima(tdf, order=c(2,0,0))
Arima(tdf, order=c(0,0,2))
Arima(tdf, order=c(2,0,2)) 
Arima(tdf, order=c(0,0,3))
adf.test(tdf)

df2 = read.csv('IBOVESPA.csv')
tdf2 = ts(df2$Ln.return)
auto.arima(tdf2, seasonal = FALSE, test='adf', ic='aic')
Arima(tdf2, order=c(1,0,0), include.constant = TRUE)
Arima(tdf2, order=c(0,0,1))
Arima(tdf2, order=c(1,0,1))
checkresiduals(Arima(tdf2, order=c(1,0,0)))
checkresiduals(Arima(tdf2, order=c(1,0,1)))
adf.test(tdf2)


arima(tdf2, order =c(1,0,0))


test1 = auto.arima(tdf2, seasonal = FALSE, test='adf', ic='aic')
forecast1
forecast1 = Arima(tdf2, order=c(0,0,1))
forecast2 = Arima(tdf, order=c(2,0,0))
forecast(forecast1)
forecast(test1)
forecast(forecast2)



df3 = read.csv('portfolio.csv')
tdf3 = ts(df3$Port.Ln.return)
test2 = auto.arima(tdf3, seasonal = FALSE, test='adf', ic='aic')
test2
Box.test(test2$residuals)
