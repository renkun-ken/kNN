# Classic k-nearest neighbor algorithms
loadData <- function() {
  data <- read.csv("data/eurusd60.csv",header=F,stringsAsFactors=F)
  colnames(data) <- c("date","time","open","high","low","close","volume")
  plot(data$close,type="l")
}


kpredictn <- function(data,k,h,n.ahead,min.cor=0,
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

kpredict <- function(data,hs,k,n.ahead=1,min.cor=0) {
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

# TODO: parallelize the method by passing functions as arguments
kvalidate <- function(data,start,hs,k,n.ahead,min.cor=0,print.out=T) {
  n <- length(data)
  range <- start:(n-n.ahead)
  valid <- sapply(range,function(i) {
    vdata <- data[1:i]
    result <- kpredict(vdata,hs=hs,k=k,n.ahead=n.ahead,min.cor=min.cor)
    pred <- result$pred$estimate["pred",]
    actual <- data[(i+1):(i+n.ahead)]
    residuals <- pred-actual
    mde <- mean(residuals)
    made <- mean(abs(residuals))
    cor <- cor(pred,actual)
    if(print.out) {
      print(i)
    }
    return(c(mde=mde,made=made,cor=cor))
  })
  return(data.frame(t(valid)))
}

# Issues
# The predictor seems biased
test <- function() {
  result <- kvalidate(data[1:2000,"close"],1200,hs=seq(30,40),k=300,n.ahead=1,min.cor=0)
  par(mfrow=c(2,1))
  plot(density(result$mde),main="MDE")
  plot(density(result$made),main="MADE")
}

