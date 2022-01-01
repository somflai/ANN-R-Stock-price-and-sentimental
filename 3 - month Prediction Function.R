NxtQuatPred <- function(data.frame){
  library(MASS)
  library(neuralnet)
  ReqError <- 0.1
  Int.No.Row <- nrow(data.frame)
  data.frame$ndc <- data.frame$Close
  data.frame$Predicted.For <- data.frame$Date
  for (i in 1:(Int.No.Row-60)){
    data.frame$ndc[i] <- data.frame$Close[i+60]
    data.frame$Predicted.For[i] <- data.frame$Date[i+60]
  }
  Data <- data.frame
  Data<- na.omit(Data)
  Data <- Data[Data$Volume!=0,]
  Data <- Data[Data$Close!="null",]
  Data <- Data[Data$Open!="null",]
  Data <- Data[Data$High!="null",]
  Data <- Data[Data$Low!="null",]
  Data <- Data[Data$ndc!="null",]
  Data <- Data[Data$`Adj Close`!="null",]
  Data <- Data[Data$Volume!="null",]
  Data$ndc <- as.numeric(Data$ndc)
  Data$Open <- as.numeric(Data$Open)
  Data$High <- as.numeric(Data$High)
  Data$Low <- as.numeric(Data$Low)
  Data$Close <- as.numeric(Data$Close)
  Data$`Adj Close` <- as.numeric(Data$`Adj Close`)
  Data$Volume <- as.numeric(Data$Volume)
  Data <- Data[Data$Close!=Data$ndc,]
  No.Row <- nrow(Data)
  train <- data.frame(ndc=Data$ndc[1:(No.Row-50)], Open=Data$Open[1:(No.Row-50)], High=Data$High[1:(No.Row-50)], Low=Data$Low[1:(No.Row-50)], Close=Data$Close[1:(No.Row-50)], AdjClose=Data$`Adj Close`[1:(No.Row-50)])
  test <- data.frame(ndc=Data$ndc[(No.Row-49):No.Row], Open=Data$Open[(No.Row-49):No.Row], High=Data$High[(No.Row-49):No.Row], Low=Data$Low[(No.Row-49):No.Row], Close=Data$Close[(No.Row-49):No.Row], AdjClose=Data$`Adj Close`[(No.Row-49):No.Row])
  trainD <- train
  testD <- test
  maxc <- max(Data$Close)
  maxo <- max(Data$Open)
  maxh <- max(Data$High)
  maxl <- max(Data$Low)
  maxndc <- max(Data$ndc)
  maxac <- max(Data$`Adj Close`)
  #maxv <- max(Data$Volume)
  minc <- min(Data$Close)
  mino <- min(Data$Open)
  minh <- min(Data$High)
  minl <- min(Data$Low)
  minndc <- min(Data$ndc)
  minac <- min(Data$`Adj Close`)
  #minv <- min(Data$Volume)
  trainD$Close <- ((trainD$Close-minc)/(maxc-minc))
  trainD$Open <- ((trainD$Open-mino)/(maxo-mino))
  trainD$High <- ((trainD$High-minh)/(maxh-minh))
  trainD$Low <- ((trainD$Low-minl)/(maxl-minl))
  trainD$ndc <- ((trainD$ndc-minndc)/(maxndc-minndc))
  trainD$AdjClose <- ((trainD$AdjClose-minac)/(maxac-minac))
  #trainD$Volume <- ((trainD$Volume-minv)/(maxv-minv))
  testD$Close <- ((testD$Close-minc)/(maxc-minc))
  testD$Open <- ((testD$Open-mino)/(maxo-mino))
  testD$High <- ((testD$High-minh)/(maxh-minh))
  testD$Low <- ((testD$Low-minl)/(maxl-minl))
  testD$ndc <- ((testD$ndc-minndc)/(maxndc-minndc))
  testD$AdjClose <- ((testD$AdjClose-minac)/(maxac-minac))
  #testD$Volume <- ((testD$Volume-minv)/(maxv-minv))
  testD <- na.omit(testD)
  trainD <- na.omit(trainD)
  
  for (k in 1:5) {
    neuralModel<-neuralnet(ndc~Open+High+Low+Close+AdjClose,hidden=c(k),linear.output=T,data=trainD, stepmax = 1000000)
    #plot(neuralModel)
    predictions<-compute(neuralModel,testD[,-1])
    predV <- (predictions$net.result*(maxndc-minndc)+minndc)
    error <- (sum(abs((test$ndc-predV))/(test$ndc))/nrow(test))
    if (error < ReqError){
      PredTable <- data.frame(Predicted.On=Data$Date[(No.Row-49):No.Row], Predicted.For=Data$Predicted.For[(No.Row-49):No.Row] , NxtQuat.ActualValue=test$ndc, PredictedValue=predV, Error=((abs((test$ndc-predV))/(test$ndc))*100))
      ReqError <- error
    }
  }
  for(i in 1:5){
    for(j in 1:5){
      neuralModel<-neuralnet(ndc~Open+High+Low+Close+AdjClose,hidden=c(j,i),linear.output=T,data=trainD, stepmax = 1000000)
      #plot(neuralModel)
      predictions<-compute(neuralModel,testD[,-1])
      predV <- (predictions$net.result*(maxndc-minndc)+minndc)
      error <- (sum(abs((test$ndc-predV))/(test$ndc))/nrow(test))
      if (error < ReqError){
        PredTable <- data.frame(Predicted.On=Data$Date[(No.Row-49):No.Row], Predicted.For=Data$Predicted.For[(No.Row-49):No.Row] , NxtQuat.ActualValue=test$ndc, PredictedValue=predV, Error=((abs((test$ndc-predV))/(test$ndc))*100))
        ReqError <- error
      }
    }
    #if (error < ReqError){
     # PredTable <- data.frame(Predicted.On=Data$Date[(No.Row-49):No.Row], Predicted.For=Data$Predicted.For[(No.Row-49):No.Row] , NxtQuat.ActualValue=test$ndc, PredictedValue=predV, Error=((abs((test$ndc-predV))/(test$ndc))*100))      
      #ReqError <- error
    #}
  }
  return(PredTable)
}

