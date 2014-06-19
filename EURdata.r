library("ggplot2", lib.loc="C:/Users/zala/Documents/R/win-library/3.1")
library("googleVis", lib.loc="C:/Users/zala/Documents/R/win-library/3.1")
library("grid", lib.loc="C:/Program Files/R/R-3.1.0/library")
library("lattice", lib.loc="C:/Program Files/R/R-3.1.0/library")
library("latticeExtra", lib.loc="C:/Users/zala/Documents/R/win-library/3.1")
library("quantmod", lib.loc="C:/Users/zala/Documents/R/win-library/3.1")
library("scales", lib.loc="C:/Users/zala/Documents/R/win-library/3.1")
library("splines", lib.loc="C:/Program Files/R/R-3.1.0/library")
library("stats4", lib.loc="C:/Program Files/R/R-3.1.0/library")
library("timeDate", lib.loc="C:/Users/zala/Documents/R/win-library/3.1")
library("timeSeries", lib.loc="C:/Users/zala/Documents/R/win-library/3.1")
library("forecast", lib.loc="C:/Users/zala/Documents/R/win-library/3.1")
library("fUnitRoots", lib.loc="C:/Users/zala/Documents/R/win-library/3.1")
library("urca", lib.loc="C:/Users/zala/Documents/R/win-library/3.1")
library("astsa", lib.loc="C:/Users/zala/Documents/R/win-library/3.1")
library("fGarch", lib.loc="C:/Users/zala/Documents/R/win-library/3.1")
library("FinTS", lib.loc="C:/Users/zala/Documents/R/win-library/3.1")

data = read.table("EURdata_noDates.txt", sep = "\t", header = TRUE)
sample <- data[1:5,1:4]
n <- length(data[,4])


# EURAB6E1Y #################################################################################################################

data$EURAB6E1Y.ICAP <- as.POSIXct(data$EURAB6E1Y.ICAP, format="%Y %m %d")
data$X.2 <- as.numeric(levels(data$X.2))[data$X.2]

EURAB6E1Y_ALL <- data[1:4]
#class(EURAB6E1Y_ALL[,1])
#class(EURAB6E1Y_ALL[,2])
#class(EURAB6E1Y_ALL)
#typeof(EURAB6E1Y_ALL)

# BID, ASK, MID in separate graphs (not automated dating)
#EURAB6E1Y_TABLE <- EURAB6E1Y_ALL[2:4]
#EURAB6E1Y_MTS <- ts(EURAB6E1Y_TABLE, start = c(2006,90), frequency=280, names = c("ask", "bid", "mid"))
#plot(EURAB6E1Y_MTS)

EURAB6E1Y_MID <- EURAB6E1Y_ALL[4]
EURAB6E1Y_MID_XTS <- na.omit(xts(EURAB6E1Y_MID, order.by=EURAB6E1Y_ALL$EURAB6E1Y.ICAP))
plot(EURAB6E1Y_MID_XTS)

EURAB6E1Y_XTS <- na.omit(xts(EURAB6E1Y_ALL[2:4], order.by=EURAB6E1Y_ALL$EURAB6E1Y.ICAP))
plot(EURAB6E1Y_XTS)
tsRainbow <- rainbow(3)
plot.zoo(EURAB6E1Y_XTS, main="EURAB6E1Y ASK, BID, MID RATES", plot.type="single", col=tsRainbow)


# EURAB6E3Y #################################################################################################################

data$EURAB6E3Y.ICAP <- as.POSIXct(data$EURAB6E3Y.ICAP, format="%Y %m %d")
data$X.17 <- as.numeric(levels(data$X.17))[data$X.17]

EURAB6E3Y_ALL <- data[21:24]
EURAB6E3Y_MID <- EURAB6E3Y_ALL[4]
EURAB6E3Y_MID_XTS <- na.omit(xts(EURAB6E3Y_MID, order.by=EURAB6E3Y_ALL$EURAB6E3Y.ICAP))
plot(EURAB6E3Y_MID_XTS)

EURAB6E3Y_XTS <- na.omit(xts(EURAB6E3Y_ALL[2:4], order.by=EURAB6E3Y_ALL$EURAB6E3Y.ICAP))
# using xts package
plot(EURAB6E3Y_XTS[,3], xlab="time", ylab="swap rate", main = "EURAB6E3Y ASK, BID, MID RATES", major.ticks="years", minor.ticks=FALSE)
lines(EURAB6E3Y_XTS[,1], col="red")
lines(EURAB6E3Y_XTS[,2], col="green")
# using zoo package
lag <- as.numeric(format(min(time(EURAB6E3Y_XTS)), "%m")) * 22 + as.numeric(format(min(time(EURAB6E3Y_XTS)),"%d"))  #for plotting grid
ind <- seq(lag, length(EURAB6E3Y_XTS)+270, by=261)  #for plotting grid

Y <- paste(as.character(as.numeric(format(min(time(EURAB6E3Y_XTS)), "%Y")) + 1), "01 01") #the year when observations started
lag2 <- length(EURAB6E3Y_XTS[,1])-which.max(data$EURAB6E3Y.ICAP < as.POSIXct(Y, format="%Y %m %d")) 
ind <- seq(lag2, length(EURAB6E3Y_XTS)+400, by=260)  #for plotting grid
tsRainbow <- rainbow(3) #setting colors :)
plot.zoo(EURAB6E3Y_XTS, main="EURAB6E3Y ASK, BID, MID RATES", plot.type="single", col=tsRainbow)
abline(h = -1:5, v=time(EURAB6E3Y_XTS)[ind], col = "lightgray", lty = 3)


# EURAB6E5Y #################################################################################################################

data$EURAB6E5Y.ICAP <- as.POSIXct(data$EURAB6E5Y.ICAP, format="%Y %m %d")
data$X.23 <- as.numeric(levels(data$X.23))[data$X.23]

EURAB6E5Y_ALL <- data[29:32]
EURAB6E5Y_MID <- EURAB6E5Y_ALL[4]


# EURAB6E8Y #################################################################################################################

data$EURAB6E8Y.ICAP <- as.POSIXct(data$EURAB6E8Y.ICAP, format="%Y %m %d")
data$X.32 <- as.numeric(levels(data$X.32))[data$X.32]

EURAB6E8Y_ALL <- data[41:44]
EURAB6E8Y_MID <- EURAB6E8Y_ALL[4]


# EURAB6E12Y #################################################################################################################

data$EURAB6E12Y.ICAP <- as.POSIXct(data$EURAB6E12Y.ICAP, format="%Y %m %d")
data$X.44 <- as.numeric(levels(data$X.44))[data$X.44]

EURAB6E12Y_ALL <- data[57:60]
EURAB6E12Y_MID <- EURAB6E12Y_ALL[4]


# EURAB6E18Y #################################################################################################################

data$EURAB6E18Y.ICAP <- as.POSIXct(data$EURAB6E18Y.ICAP, format="%Y %m %d")
data$X.62 <- as.numeric(levels(data$X.62))[data$X.62]

EURAB6E18Y_ALL <- data[81:84]
EURAB6E18Y_MID <- EURAB6E18Y_ALL[4]


# MULTIPLE MID RATES #########################################################################################################

MULTIPLE_MID_XTS <- na.omit(xts(cbind(EURAB6E1Y_MID, EURAB6E3Y_MID, EURAB6E5Y_MID, EURAB6E8Y_MID, EURAB6E12Y_MID, EURAB6E18Y_MID), order.by=data$EURAB6E1Y.ICAP))



Y <- paste(as.character(as.numeric(format(min(time(MULTIPLE_MID_XTS)), "%Y")) + 1), "01 01")
lag2 <- length(MULTIPLE_MID_XTS[,1])-which.max(data$EURAB6E1Y.ICAP < as.POSIXct(Y, format="%Y %m %d")) 
ind <- seq(lag2, length(MULTIPLE_MID_XTS)+270, by=261)  #for plotting grid
tsRainbow <- rainbow(6) #setting colors :)

plot.zoo(MULTIPLE_MID_XTS, main="swap rates", plot.type="single", col=tsRainbow)
legend(x = "topleft", legend = c("EURAB6E1Y", "EURAB6E3Y", "EURAB6E5Y", "EURAB6E8Y", "EURAB6E12Y", "EURAB6E18Y"), lty = 1, col = tsRainbow)
abline(h = -1:5, v=time(MULTIPLE_MID_XTS)[ind], col = "lightgray", lty = 3)



# OTHER PACkAGES #########################################################################################################

# asTheEconomist - nice :)
asTheEconomist(
  xyplot(
    EURAB6E1Y_MID_XTS,
    scales = list( y = list( rot = 0 ) ),
    main = "EURAB6E1Y_MID (lattice::xyplot.xts)"  
  )
)

# googleVis - vrne javascript
# g1 <- gvisLineChart(
#  data = EURAB6E1Y_ALL,
#  xvar = "EURAB6E1Y.ICAP",
#  yvar = "X.2",
#  options = list(
#    title = "EURAB6E1Y_MID (googleVis::gvisLineChart)",
#   height = 400,
#   width = 600
#  )
#
#print(g1,"chart")


# TIME SERIES ANALYSIS #########################################################################################################

# MA ##########################################################################################################################
#smoothing with simple MA
par(mfrow = c(2,1))
plot.ts(ts(EURAB6E5Y_MID))
plot.ts(SMA(na.omit(ts(EURAB6E5Y_MID, n=5))), ylab = "simple MA")

#simple exponential smoothing (without trend and without seasonal component) 
smoothing_noTrend <- HoltWinters(na.omit(ts(EURAB6E5Y_MID)), beta=FALSE, gamma=FALSE)
smoothing_withTrend <- HoltWinters(na.omit(ts(EURAB6E5Y_MID)), beta=TRUE, gamma=FALSE)
#EURAB6E5Y_MID[3378,]
plot(smoothing_noTrend)
plot(smoothing_withTrend)
SSE_noTrend <- smoothing_noTrend$SSE
SSE_withTrend <- smoothing_withTrend$SSE

#forecasting with HoltWinters
forecast_noTrend <- forecast.HoltWinters(smoothing_noTrend, h=30)
par(mfrow = c(2,1))
plot(forecast_noTrend, xlim=c(3100, 3500), main="Forecsats from Holtwinters (without trend)")

forecast_withTrend <- forecast.HoltWinters(smoothing_withTrend, h=30)
plot(forecast_withTrend, xlim=c(3100, 3500), main="Forecsats from Holtwinters (with trend)")

#autocorrelation
par(mfrow = c(1,1))
acf(forecast_noTrend$residuals, lag.max=10)
Box.test(forecast_noTrend$residuals, lag=10, type="Ljung-Box") #indicating there's no autocorrelation
acf(forecast_withTrend$residuals, lag.max=10)
Box.test(forecast_withTrend$residuals, lag=10, type="Ljung-Box")
par(mfrow = c(2,1))
plot.ts(forecast_noTrend$residuals)
plot.ts(forecast_withTrend$residuals)

# plot forecast errors to see if it is approx normally distributed (load plotForecastErrors.r)
plotForecastErrors(forecast_noTrend$residuals)
plotForecastErrors(forecast_withTrend$residuals)


# ARIMA (p, d, q) ##################################################################################################################

# if the series is not stationary we have to difference it until we get stationary
ts <- na.omit(ts(EURAB6E5Y_MID))
par(mfrow = c(2,1))
ts_diff <- diff(ts, differences=1)
plot.ts(ts_diff)
ts_diff2 <- diff(ts, differences=2)
plot.ts(ts_diff2)

# test for stationarity. NH = stationarity
urkpssTest(ts)
urkpssTest(ts_diff)
urkpssTest(ts_diff2) #stationary. d=2

# selecting p and q
acf(ts_diff2, lag.max=20) # q=1 suggesting ARMA(0,1)
acf(ts_diff2, plot=FALSE) #actual values
pacf(ts_diff2, lag.max=20) #p=17 suggesting ARMA(17.0)
pacf(ts_diff2, plot=FALSE)

# Choose ARIMA(0,2,1)
# Function that suggests arima model:
auto.arima(ts_diff2)
auto.arima(ts) # tells we should use ARIMA(0,1,1)


ARIMA021 <- arima(ts, order=c(0,2,1)) #slightly better
ARIMA011 <- arima(ts, order=c(0,1,1))

forecast_arima021 <- forecast.Arima(ARIMA021, h=30)
plot(forecast_arima021)

# Checking if residuals are uncorrelated
acf(forecast_arima021$residuals, lag.max=20)
plot.ts(forecast_arima021$residuals)
plotForecastErrors(forecast_arima021$residuals)
urkpssTest(forecast_arima021$residuals)

# using sarima() that does the correct diagnostics
sarima(ts, 0, 2, 1)
sarima(ts, 0, 1, 1)

#Q-Q plot
qqnorm(ts_diff2)             # normal Q-Q plot  
qqline(ts_diff2)

# ARCH, GARCH ##################################################################################################################
#conditional heteroscedastic models

# check for autocorrelation (already checked in acf grapf above - now Ljung-Box test)
acf(forecast_arima021$residuals, lag.max=3)
Box.test(forecast_arima021$residuals, lag=3, type="Ljung-Box") # no serial autocorrelation
AutocorTest(forecast_arima021$residuals, lag=ceiling(log(length(forecast_arima021))), type="Ljung-Box") #same test performed in FinTS package

# check for ARCH effect (original series=residuals)
ArchTest(forecast_arima021$residuals, lags=3) #indicates ARCH effect

# Build ARCH(0) model (p=0 from AR(0))
ARCHseries <- forecast_arima021$residuals
pacf(ARCHseries^2, ylim=c(-1,1)) #no significant lag
arch0 <- garch(ARCHseries, order=c(0,0))






