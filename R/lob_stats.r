graphValueAndPrice <- function(filename) {
  data <- read.csv(paste("../results/",filename,".csv", sep=""))
  maxY <- range(data$lastTradedPrice)
  pdf(paste("../graphs/", filename, "-valueAndPrice",".pdf", sep=""))
  plot(data$time, data$lastTradedPrice, type="n", xlab="Time", ylab="Price", ylim=maxY)
  lines(data$time, data$lastTradedPrice)
  lines(data$time, data$currentValue, lty=2)
  legend("topright", legend=c("Last trade price", "Current underlying value"), lty=c(1,2), inset=0.05, bg="white")
  dev.off()
}
