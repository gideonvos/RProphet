library(prophet)
library(dplyr)
library(ggplot2)
library(RODBC)
library(tseries)

dbconnection <- odbcDriverConnect("Driver={ODBC Driver 17 for SQL Server};Server=localhost; Database=ML;Uid=SA; Pwd=GID792658jpa;")
stats <- sqlQuery(dbconnection,paste("SELECT * FROM SalesOrders;"))
odbcClose(dbconnection)

#stats <- read.csv('~/Datasets/SalesOrders.csv', header=TRUE, sep=',',colClasses = c('numeric','character','numeric','character','numeric','numeric','numeric','Date'))
colnames(stats) <- c('id','costprice','item','lineamount','description','qtyordered','salesprice','salesqty','orderdate')
stats$orderdate <- as.Date(stats$orderdate) # sql query changes type so coerce back to date
stats <- stats[order(stats$orderdate),]
stats <- stats[stats$item=='T0001' & stats$orderdate>='2015-01-01' & stats$orderdate<='2017-01-01',]

# quick summary. 2016 is incomplete, only first 8 months available.
summary(stats)

# for proper comparison compare first 8 months of 2015 with the 8 months of 2016
stats2015 <- stats[stats$item=='T0001' & stats$orderdate>='2015-01-01' & stats$orderdate<='2015-09-01',]
stats2016 <- stats[stats$item=='T0001' & stats$orderdate>='2016-01-01' & stats$orderdate<='2016-09-01',]

# total revenue
totalrevenue2015 <- sum(stats2015$salesprice)
totalrevenue2016 <- sum(stats2016$salesprice)

# total gross profit
totalgrossprofit2015 <- sum(stats2015$salesprice) - sum(stats2015$costprice)
totalgrossprofit2016 <- sum(stats2016$salesprice) - sum(stats2016$costprice)

# profit margin
profitmargin2015 <- (totalgrossprofit2015 / totalrevenue2015) * 100
profitmargin2016 <- (totalgrossprofit2016 / totalrevenue2016) * 100

# total units shipped
unitsordered2015 <- sum(stats2015$qtyordered)
unitsshipped2015 <- sum(stats2015$salesqty)
unitsordered2016 <- sum(stats2016$qtyordered)
unitsshipped2016 <- sum(stats2016$salesqty)

# order cancellations
cancelledorders2015 <- sum(stats2015$qtyordered) - sum(stats2015$salesqty)
cancelledorders2016 <- sum(stats2016$qtyordered) - sum(stats2016$salesqty)

# work on both 2015 & 2016
# use sales quantity for predictions, units rather than $ amounts as pricing could change over time
stats <- stats[,c('salesqty','orderdate')]

# distribution of sales bands shows most unit orders are in batches of less than 20 
hist(stats$salesqty,main="Sales Bands", 
     xlab="Units", 
     border="steelblue4", 
     col="steelblue4",
     las=1, breaks=5)

# look deeper - several groups, 1-15 unit orders most popular
# with large unit order around 50 units per order as a different
# customer segment
# thus 2 types of customers, small unit order and a distinct
# large batch order at 35 to 70
hist(stats$salesqty,main="Sales Bands - 20 Bins", 
     xlab="Units", 
     border="steelblue4", 
     col="steelblue4",
     las=1, breaks=25)

# aggregate daily sales
stats <- aggregate(stats$salesqty, by=list(stats$orderdate), sum)

# and fix the column names
names(stats) <- c('orderdate','salesqty')

# let's plot 2015-2016
plot(stats)
abline(reg=lm(stats$salesqty~stats$orderdate))

# perform a Dickey-Fuller test to see if we have stationary data
adf.test(diff(log(stats$salesqty)), alternative="stationary", k=0)
# alternative hypothesis: stationary (p-value 0.02)

# plot the trend properly
p <- ggplot(data = stats, aes(orderdate,salesqty)) + 
  geom_line(color = "steelblue4", size = 1)
p
min <- as.Date("2015-01-01")
max <- as.Date("2016-12-01")
p + scale_x_date(limits = c(min, max))
p + stat_smooth(
  color = "#FC4E07", fill = "#FC4E07",
  method = "loess"
)

# use fbprophet to predict next 4 months
# fbprophet wants ds,y column names
names(stats) <- c('ds','y')

# include monthly seasonality
# exclude daily since sales are reported
# irregularly
m <- prophet(stats, yearly.seasonality=TRUE)
future <- make_future_dataframe(m, periods = 4, freq="m")
forecast <- predict(m, future)
# keep the predicted sales volumes, stored in yhat
# compared to 4557 units for first 8 months, 
# we predict 3346 units to be shipped in last 4 months
# this is skewed low due to (false) seasonality detected by fbprophet, so expect
# substantially more units moved
predictunits <- sum(tail(forecast$yhat), 4)

# plot the prediction
plot(m, forecast) +
  xlab("Date") +
  ylab("Data") +
  theme_grey() + 
  theme_grey() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ggtitle("Sales Forecast")

# plot seasonality
prophet_plot_components(m, forecast)

