data = read.table("EURdata_noDates.txt", sep = "\t", header = TRUE)
sample <- data[1:5,1:4]
n <- length(data[,4])
dates <- data[1]
data$EURAB6E1Y.ICAP <- as.POSIXct(data$EURAB6E1Y.ICAP, format="%Y %m %d")
data$X.2 <- as.numeric(levels(data$X.2))[data$X.2]
#rownames(data) <- dates




EURAB6E1Y_ALL <- data[1:4]
class(EURAB6E1Y_ALL[,1])

EURAB6E1Y_TABLE <- EURAB6E1Y_ALL[2:4]
EURAB6E1Y_MTS <- ts(EURAB6E1Y_TABLE, start = c(2008,2,11), frequency=360, names = c("ask", "bid", "mid"))
#EURAB6E1Y_MTS <- ts(EURAB6E1Y_ALL, names = c("date","ask", "bid", "mid"))
plot(EURAB6E1Y_MTS)

#EURAB6E1Y_MID <- EURAB6E1Y_ALL[4]
EURAB6E1Y_MID <- EURAB6E1Y_ALL[1:2]
#EURAB6E1Y_XTS <- as.xts(ts(EURAB6E1Y_MID, start= c(2008, 40), frequency=360))
EURAB6E1Y_XTS <- xts(EURAB6E1Y_MID))
#EURAB6E1Yts <- ts(EURAB6E1Yvec, frequency=360, start = c(2008,40))
#print(EURAB6E1Yts)
plot(as.zoo(EURAB6E1Y_XTS), main="EURAB6E1Y MID")

#df2 <- xts(EURAB6E1Y_ALL[2:4], order.by=EURAB6E1Y_ALL$EURAB6E1Y.ICAP)


EURAB6E1Yts[20]
ts(1:10, frequency = 4, start = c(1959, 2))
plot(EURAB6E1Y[2:1491, "X.2"], ylim = c(0,10), xlim = c(0,1500), pch=".", col="blue")


EURAB6E3Y <- data[,21:24]
# first observation EURAB6E3Y[3379,]
EURAB6E3Yts_table <- ts(EURAB6E3Y[,2:4], start = c(1998, 330), frequency=360, names = c("ask", "bid", "mid"))
plot(EURAB6E3Yts_table)





sp500 <- na.omit( 
  getSymbols(
    "SP500",
    src = "FRED",
    from = "1949-12-31",
    auto.assign = FALSE
  )
)
sp500.monthly <- sp500[endpoints(sp500, on ="months")]
plot(sp500)

timeSeries::plot(
  timeSeries(sp500.monthly),
  main = "S&P 500 (timeseries::plot)"
)

m2 <- cbind(1, 1:4)
colnames(m2, do.NULL = FALSE)
colnames(m2) <- c("x","Y")
rownames(m2) <- c("a", "b", "c", "d")
rownames(m2) <- rownames(m2, do.NULL = FALSE, prefix = "Obs.")
m2
