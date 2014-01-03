# Classic k-nearest neighbor algorithms
data <- read.csv("data/eurusd60.csv",header=F,stringsAsFactors=F)
colnames(data) <- c("date","time","open","high","low","close","volume")
plot(data$close,type="l")

kpredictn <- function(data,k=100,h=10,n.ahead=3,min.cor=0.5,
                      output=c("predicts","estimate","error")) {
  n <- length(data)
  series <- data[(n-h+1):n]
  range <- 1:(n-h-n.ahead+1)
  cors <- sapply(range,function(i) {
    pattern <- data[i:(i+h-1)]
    return(cor(pattern,series))
  })
  abs.cors <- abs(cors)
  orders <- order(abs.cors,decreasing=T)
  indices <- range[orders<=k & abs.cors>=min.cor]
  
  predicts <- t(sapply(indices,function(i) {
    pattern <- data[i:(i+h-1)]
    predictor <- data[(i+h):(i+h+n.ahead-1)]
    m <- lm(series~pattern)
    coeff <- coef(m)
    predictor <- coeff[[1]]+coeff[[2]]*predictor
    return(predictor)
  }))
  
  corsi <- cors[indices]
  abs.corsi <- abs(corsi)
  estimate <- sapply(1:n.ahead,function(t) {
    w <- exp(abs.corsi)
    sumw <- sum(w)
    preds <- w*predicts[,t]/sumw
    pred <- sum(preds)
    sd <- sd(preds)
    rsd <- sd/abs(pred)
    return(c(pred=pred,sd=sd,rsd=rsd))
  })
  error <- sum(estimate["sd",])
  result <- list(predicts=predicts,estimate=estimate,error=error)
  return(result[output])
}

kpredict <- function(data,hs=seq(10,15),k=100,n.ahead=3,min.cor=0.5) {
  n <- length(data)
  g <- length(hs)
  groups <- 1:g
  plist <- lapply(hs,function(h) {
    predict <- kpredictn(data,h=h,k=k,n.ahead=n.ahead,min.cor=min.cor)
    result <- c(list(h=h),predict)
    return(result)
  })
  errors <- sapply(groups,function(i)plist[[i]]$error)
  orders <- order(errors)
  result <- plist[[groups[orders==1]]]
  return(list(errors=errors,orders=orders,pred=result))
}

# demo code
# change to test function to evaluate the performance of the predictive algorithm
result <- kpredict(data[1:2000,"close"],hs=seq(100,120),k=60,n.ahead=15,min.cor=0.8)
result$pred$estimate["pred",]-data[2001:2015,"close"]
mean(abs(result$pred$estimate["pred",]-data[2001:2015,"close"]))
cor(result$pred$estimate["pred",],data[2001:2015,"close"])
