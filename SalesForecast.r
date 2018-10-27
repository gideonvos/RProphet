library(prophet)
library(dplyr)
library(ggplot2)
library(RODBC)
library(Cairo)
library(png)

encodeResult <-function()
{
  i = Cairo:::.image(dev.cur())
  r = Cairo:::.ptr.to.raw(i$ref, 0, i$width * i$height * 4)
  dim(r) = c(4, i$width, i$height) 
  r[c(1,3),,] = r[c(3,1),,]
  p = writePNG(r, raw()) 
  base64enc::base64encode(p)
}

encoding = TRUE # encode or display?

dbconnection <- odbcDriverConnect("Driver={ODBC Driver 17 for SQL Server};Server=localhost; Database=ML;Uid=SA; Pwd=GID792658jpa;")
stats <- sqlQuery(dbconnection,paste("SELECT * FROM SalesOrders;"))
odbcClose(dbconnection)

#stats <- read.csv('~/Datasets/SalesOrders.csv', header=TRUE, sep=',',colClasses = c('numeric','character','numeric','character','numeric','numeric','numeric','Date'))
colnames(stats) <- c('id','costprice','item','amount','description','ordered','qty','price','orderdate')
stats$orderdate <- as.Date(stats$orderdate) # sql query changes type so coerce back to date
stats <- stats[order(stats$orderdate),]
stats <- stats[stats$item=='T0003' & stats$orderdate<='2016-08-01',]
qtybreakdown <- stats[,c('ordered','orderdate')]

stats <- stats[,c('amount','orderdate')]
stats <- stats[stats$amount>0.00,]
stats <- aggregate(stats$amount, by=list(stats$orderdate), sum)
names(stats) <- c('ds','y')
format(round(as.numeric(sum(stats$y * 320)), 0), nsmall=0, big.mark=',')
summary(stats)

# how much per month in 2016?
mstats <- stats[stats$ds>='2016-01-01' & stats$ds<='2016-02-01',]
format(round(as.numeric(sum(mstats$y * 320)), 0), nsmall=0, big.mark=',')

#revenue? x320 per unit
mstats <- stats[stats$ds>='2015-01-01' & stats$ds<='2016-01-01',]
format(round(as.numeric(sum(mstats$y * 320)), 0), nsmall=0, big.mark=',')

if (encoding == TRUE) Cairo(file='/dev/null')

hist(qtybreakdown$ordered,main="Historical Sales", 
     xlab="Units", 
     border="steelblue4", 
     col="steelblue4",
     las=1, breaks=15)
if (encoding == TRUE) encodedHist <- encodeResult()

p <- ggplot(data = stats, aes(ds,y)) + 
  geom_line(color = "steelblue4", size = 1)
p
min <- as.Date("2015-01-01")
max <- as.Date("2016-12-01")
p + scale_x_date(limits = c(min, max))
p + stat_smooth(
  color = "#FC4E07", fill = "#FC4E07",
  method = "loess"
)
if (encoding == TRUE) encodedSales <- encodeResult()

m <- prophet(stats, yearly.seasonality=TRUE, weekly.seasonality = TRUE, daily.seasonality = TRUE)
future <- make_future_dataframe(m, periods = 6, freq="m")
forecast <- predict(m, future)
predictunits <- sum(tail(forecast$yhat), 6)
forecastrevenue <- format(round(as.numeric(sum(tail(forecast$yhat), 6)), 0), nsmall=2, big.mark=",")

predictunits
forecastrevenue

plot(m, forecast) +
  xlab("Date") +
  ylab("Data") +
  theme_grey() + 
  theme_grey() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ggtitle("Sales Forecast")
if (encoding == TRUE) encodedForecast <- encodeResult()

encodedForecast

prophet_plot_components(m, forecast)
if (encoding == TRUE) encodedSeasonal <- encodeResult()

