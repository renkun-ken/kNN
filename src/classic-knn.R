# Classic k-nearest neighbor algorithms
data <- read.csv("data/eurusd60.csv",header=F,stringsAsFactors=F)
colnames(data) <- c("date","time","open","high","low","close","volume")
plot(data$close,type="l")

kpredictn <- function(data,k=10,h=10,n.ahead=5,min.cor=0.5) {
  n <- length(data)
  series <- data[(n-h+1):n]
  range <- 1:(n-h-n.ahead+1)
  cors <- sapply(range,function(i) {
    pattern <- data[i:(i+h-1)]
    return(cor(pattern,series))
  })
  abc.cors <- abs(cors)
  orders <- order(abc.cors,decreasing=T)
  indices <- range[orders<=k & abc.cors>=min.cor]
  predicts <- lapply(indices,function(i) {
    pattern <- data[i:(i+h-1)]
    predictor <- data[(i+h):(i+h+n.ahead-1)]
    m <- lm(series~pattern)
    coeff <- coef(m)
    # run a linear model to find slope and offset
    predictor <- coeff[[1]]+coeff[[2]]*predictor
    return(predictor)
  })
  return(predicts)
}

kpredict <- function(data,lens=seq(10,15),k=10,n.ahead=1) {
  n <- length(data)
  predict.df.list <- lapply(lens,function(len) {
    series <- data[(n-len+1):n]
    corr.df.list <- lapply(1:(n-len),function(i) {
      data.frame(length=len,
                 index=i,
                 cor=cor(data[i:(i+len-1)],series),
                 predict=data[i+len])
    })
    corr.df <- do.call("rbind",corr.df.list)
    corr <- corr.df[with(corr.df,order(cor,decreasing=T)),][1:k,]
    # baseline adjustment: scaling and shifting
    # adjust for pattern and predictor as well
    predictor <- exp(corr$cor)*corr$predict/sum(exp(corr$cor))
    predict.sd <- sd(exp(corr$cor)*corr$predict/sum(exp(corr$cor)))
    data.frame(length=len,predict.sd=predict.sd)
  })
  predict.df<- do.call("rbind",predict.df.list)
  result <- predict.df[with(predict.df,order(predict.sd)),]
  result
}