library(prophet)
library(dplyr)
library(ggplot2)
library(png)
library(plumber)
library(urltools)

encodeGraphic <- function(g) {
  png(tf1 <- tempfile(fileext = ".png"))
  print(g)
  dev.off()
  encoded <- RCurl::base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt")
  return(encoded)
}

#* Do a forecast
#* @param data a CSV containing ordered, orderdate
#* @get /forecast
x<-function(data="")
{
  json = "{'forecast':'"
  tmp<-URLdecode(data)
  stats <- read.csv(text=tmp, header=TRUE, sep=',',colClasses = c('numeric','Date'))
  names(stats) <- c("y","ds")
  stats$ds <- as.Date(stats$ds) # coerce to ensure date type

  m <- prophet(stats, yearly.seasonality=TRUE, weekly.seasonality = TRUE, daily.seasonality = TRUE)
  future <- make_future_dataframe(m, periods = 6, freq="m")
  forecast <- predict(m, future)

  g<-plot(m, forecast) +
    xlab("Date") +
    ylab("Data") +
    theme_grey() + 
    theme_grey() + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line = element_line(colour = "black")) +
    ggtitle("Sales Forecast");

  encodedForecast<-encodeGraphic(g)
  json <- paste(json, encodedForecast,sep="")
  
  g<-prophet_plot_components(m, forecast)
  json <- paste(json, "','trend':'", sep="")
  encodedTrend <- encodeGraphic(g[1])
  json<-paste(json, encodedTrend,sep="")
  json<-paste(json,"','weekly':'", sep="")
  encodedWeekly <- encodeGraphic(g[2])
  json<-paste(json, encodedWeekly,sep="")
  json<-paste(json,"','yearly':'", sep="")
  encodedYearly <- encodeGraphic(g[3])
  json<-paste(json, encodedYearly,sep="")
  json<-paste(json,"','daily':'", sep="")
  encodedDaily <- encodeGraphic(g[1])
  json<-paste(json, encodedDaily,sep="")
  json<-paste(json, "'}", sep="")
  return(json)
}

x(data="ordered%2Corderdate%0D%0A380%2C2015-01-15+00%3A00%3A00%0D%0A380%2C2015-02-15+00%3A00%3A00%0D%0A369%2C2015-04-16+00%3A00%3A00%0D%0A369%2C2015-05-16+00%3A00%3A00%0D%0A369%2C2015-06-15+00%3A00%3A00%0D%0A354%2C2015-07-15+00%3A00%3A00%0D%0A354%2C2015-08-15+00%3A00%3A00%0D%0A356%2C2015-09-15+00%3A00%3A00%0D%0A371%2C2015-10-15+00%3A00%3A00%0D%0A380%2C2015-11-15+00%3A00%3A00%0D%0A391%2C2015-12-15+00%3A00%3A00%0D%0A389%2C2016-01-15+00%3A00%3A00%0D%0A391%2C2016-02-15+00%3A00%3A00%0D%0A391%2C2016-03-15+00%3A00%3A00%0D%0A390%2C2016-04-15+00%3A00%3A00%0D%0A389%2C2016-05-15+00%3A00%3A00%0D%0A391%2C2016-06-15+00%3A00%3A00%0D%0A390%2C2016-07-15+00%3A00%3A00")


