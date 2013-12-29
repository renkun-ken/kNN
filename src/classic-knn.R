# Classic k-nearest neighbor algorithms
data <- read.csv("data/eurusd1.csv",header=F,stringsAsFactors=F)
colnames(data) <- c("date","time","open","high","low","close","volume")
plot(data$close,type="l")

kpredict <- function(data,lens=seq(10,15),k=10) {
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
    # baseline adjustment
    predictor <- exp(corr$cor)*corr$predict/sum(exp(corr$cor))
    predict.sd <- sd(exp(corr$cor)*corr$predict/sum(exp(corr$cor)))
    data.frame(length=len,predict.sd=predict.sd)
  })
  predict.df<- do.call("rbind",predict.df.list)
  result <- predict.df[with(predict.df,order(predict.sd)),]
  result
}