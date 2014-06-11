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
plot(EURAB6E3Y_XTS)
tsRainbow <- rainbow(3)
plot.zoo(EURAB6E3Y_XTS, main="EURAB6E3Y ASK, BID, MID RATES", plot.type="single", col=tsRainbow)
abline(h = -1:5, v = 2000-01-01, col = "lightgray", lty = 3)


# MULTIPLE MID RATES #########################################################################################################

MULTIPLE_MID_XTS <- na.omit(xts(cbind(EURAB6E1Y_MID, EURAB6E3Y_MID), order.by=data$EURAB6E1Y.ICAP))
plot.zoo(MULTIPLE_MID_XTS, main="EURAB6E1Y_MID and EURAB6E3Y_MID", plot.type="single", col=tsRainbow)
legend(x = "topleft", legend = c("EURAB6E1Y", "EURAB6E3Y"), lty = 1, col = tsRainbow)
abline(h = -1:5, v = 2000-01-01, col = "lightgray", lty = 3)




