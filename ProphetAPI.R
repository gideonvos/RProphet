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
function(data="")
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
  encodedDaily <- encodeGraphic(g[4])
  json<-paste(json, encodedDaily,sep="")
  json<-paste(json, "'}", sep="")
  return(json)
}
