graphValueAndPrice <- function(filename) {
  data <- read.csv(paste("../results/",filename,".csv", sep=""))
  maxY <- range(0,max(data$lastTradedPrice, data$currentValue))
  pdf(paste("../graphs/", filename, "-valueAndPrice",".pdf", sep=""))
  plot(data$time, data$lastTradedPrice, type="n", xlab="Time", ylab="Price", ylim=maxY)
  lines(data$time, data$lastTradedPrice)
  lines(data$time, data$currentValue, lty=2)
  legend("topright", legend=c("Last trade price", "Current underlying value"), lty=c(1,2), inset=0.05, bg="white")
  dev.off()
}

graphLiquidity <- function(filename) {
  data <- read.csv(paste("../results/",filename,".csv", sep=""))
  maxY <- range(0,max(data$buySideLiquidity, data$sellSideLiquidity))
  pdf(paste("../graphs/", filename, "-liquidity",".pdf", sep=""))
  plot(data$time, data$buySideLiquidity, type="n", xlab="Time", ylab="Orders", ylim=maxY)
  lines(data$time, data$buySideLiquidity)
  lines(data$time, data$sellSideLiquidity, lty=2)
  legend("topright", legend=c("Buy side liquidity", "Sell side liquidity"), lty=c(1,2), inset=0.05, bg="white")
  dev.off()
}

graphBookPrices <- function(filename) {
  data <- read.csv(paste("../results/",filename,".csv", sep=""))
  maxY <- range(0,max(data$bestOffer, data$currentValue, data$bestBid))
  pdf(paste("../graphs/", filename, "-bookPrices",".pdf", sep=""))
  plot(data$time, data$currentValue, type="n", xlab="Time", ylab="Orders", ylim=maxY)
  lines(data$time, data$currentValue)
  lines(data$time, data$bestOffer, lty=2)
  lines(data$time, data$bestBid, lty=3)
  legend("topright", legend=c("Current underlying value", "Best bid price", "Best offer price"), lty=c(1,2,3), inset=0.05, bg="white")
  dev.off()
  }
