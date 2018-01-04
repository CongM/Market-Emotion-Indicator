####################################################


if( 'rpart' %in% as.data.frame(installed.packages())$Package == F )
  install.packages('rpart')
if( 'ipred' %in% as.data.frame(installed.packages())$Package == F )
  install.packages('ipred')
if( 'randomForest' %in% as.data.frame(installed.packages())$Package == F )
  install.packages('randomForest')
if( 'gbm' %in% as.data.frame(installed.packages())$Package == F )
  install.packages('gbm')
if( 'xgboost' %in% as.data.frame(installed.packages())$Package == F )
  install.packages('xgboost')


library(rpart)
library(ipred)
library(randomForest)
library(gbm)
library(xgboost)

load('index_importance.rda')


dat1 <- read.csv( '区间最高收益率.csv' , fileEncoding = 'GBK' )
dat2 <- read.csv( '区间最大亏损率.csv' , fileEncoding = 'GBK' )
dat3 <- read.csv( '区间收益率.csv' , fileEncoding = 'GBK' )
for( k in 1:ncol(dat1) )
{
  dat1[,k] <- as.numeric(dat1[,k])
  dat2[,k] <- as.numeric(dat2[,k])
  dat3[,k] <- as.numeric(dat3[,k])
}



train_predict_rr <- function( dat , n = 0.8 , index = '沪深300' , span = '1个月' , type = '区间最高收益率' )
{
  
  ss <- c(rep(0.2,6),rep(0.4,6),rep(0.5,12),rep(0.65,6),rep(0.2,6))
  
  #沪深300 1个月
  if( index == '沪深300' & span == '1个月' )
  {
    i <- 7
    
    if( type == '区间最高收益率' )
    {
      ind <- index_imp4[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
     
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
     
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
     
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
    
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
     
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
  
    }
    
    if( type == '区间最大亏损率' )
    {
      ind <- index_imp5[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间收益率' )
    {
      ind <- index_imp6[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    
    
  }

  
  #万得全A 1个月
  if( index == '万得全A' & span == '1个月' )
  {
    i <- 31
    
    if( type == '区间最高收益率' )
    {
      ind <- index_imp4[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间最大亏损率' )
    {
      ind <- index_imp5[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间收益率' )
    {
      ind <- index_imp6[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    
    
  }
  

  #沪深300 3个月
  if( index == '沪深300' & span == '3个月' )
  {
    i <- 8
    
    if( type == '区间最高收益率' )
    {
      ind <- index_imp4[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间最大亏损率' )
    {
      ind <- index_imp5[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间收益率' )
    {
      ind <- index_imp6[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    
    
  }
  
  
  #万得全A 3个月
  if( index == '万得全A' & span == '3个月' )
  {
    i <- 32
    
    if( type == '区间最高收益率' )
    {
      ind <- index_imp4[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间最大亏损率' )
    {
      ind <- index_imp5[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间收益率' )
    {
      ind <- index_imp6[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    
    
  }
  
  
  #沪深300 6个月
  if( index == '沪深300' & span == '6个月' )
  {
    i <- 9
    
    if( type == '区间最高收益率' )
    {
      ind <- index_imp4[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间最大亏损率' )
    {
      ind <- index_imp5[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间收益率' )
    {
      ind <- index_imp6[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    
    
  }
  
  
  #万得全A 6个月
  if( index == '万得全A' & span == '6个月' )
  {
    i <- 33
    
    if( type == '区间最高收益率' )
    {
      ind <- index_imp4[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间最大亏损率' )
    {
      ind <- index_imp5[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间收益率' )
    {
      ind <- index_imp6[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    
    
  }
  
  
  #沪深300 12个月
  if( index == '沪深300' & span == '12个月' )
  {
    i <- 10
    
    if( type == '区间最高收益率' )
    {
      ind <- index_imp4[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间最大亏损率' )
    {
      ind <- index_imp5[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间收益率' )
    {
      ind <- index_imp6[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    
    
  }
  
  
  #万得全A 12个月
  if( index == '万得全A' & span == '12个月' )
  {
    i <- 34
    
    if( type == '区间最高收益率' )
    {
      ind <- index_imp4[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间最大亏损率' )
    {
      ind <- index_imp5[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间收益率' )
    {
      ind <- index_imp6[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    
    
  }
  
  
  #沪深300 24个月
  if( index == '沪深300' & span == '24个月' )
  {
    i <- 11
    
    if( type == '区间最高收益率' )
    {
      ind <- index_imp4[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间最大亏损率' )
    {
      ind <- index_imp5[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间收益率' )
    {
      ind <- index_imp6[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    
    
  }
  
  
  #万得全A 24个月
  if( index == '万得全A' & span == '24个月' )
  {
    i <- 35
    
    if( type == '区间最高收益率' )
    {
      ind <- index_imp4[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间最大亏损率' )
    {
      ind <- index_imp5[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间收益率' )
    {
      ind <- index_imp6[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    
    
  }
  
  
  #沪深300 36个月
  if( index == '沪深300' & span == '36个月' )
  {
    i <- 12
    
    if( type == '区间最高收益率' )
    {
      ind <- index_imp4[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间最大亏损率' )
    {
      ind <- index_imp5[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间收益率' )
    {
      ind <- index_imp6[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    
    
  }
  
  
  #万得全A 36个月
  if( index == '万得全A' & span == '36个月' )
  {
    i <- 36
    
    if( type == '区间最高收益率' )
    {
      ind <- index_imp4[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间最大亏损率' )
    {
      ind <- index_imp5[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    if( type == '区间收益率' )
    {
      ind <- index_imp6[[i]]
      index_ind <- row.names(ind)[which(ind$general_rank>=30)]
      temp_dat <- dat[,c(match(index_ind,names(dat)),1)]
      
      a <- vector()
      for(j in 1:(ncol(temp_dat)-1))
      {
        if(mean(is.na(temp_dat[,j]))>=ss[i])
          a[j] <- j
      }
      
      a <- na.omit(a)
      if(length(a)!=0)
        temp_dat <- temp_dat[,-a]
      
      temp <- na.omit(temp_dat)
      
      k <- kmeans(temp[,ncol(temp)],5)
      
      temp$goal_ind1 <- k$cluster
      
      temp$goal_ind <- 0
      
      for(j in 1:5)
      {
        temp$goal_ind[which(temp$goal_ind1==j)] <- k$centers[j]
      }
      
      temp[,ncol(temp)-1] <- NULL
      
      temp[,ncol(temp)-1] <- NULL
      
      class <- unique(temp$goal_ind)
      
      test <- (round(nrow(temp)*n)+1):nrow(temp)
      
      dat_train <- temp[-test,]
      dat_test <- temp[test,]
      
      pred <- data.frame( pred.rpart = 1:nrow(dat_test) , pred.bag = 1 , pred.rf = 1 , pred.gbm = 1 , pred.xgbst = 1 )
      
      
      #rpart
      
      fit.rpart <- rpart( goal_ind~. , data = dat_train )
      pred$pred.rpart <- predict( fit.rpart , newdata = dat_test )
      
      #bagging
      
      fit.bag <- bagging( goal_ind~. , data = dat_train , nbagg = 100 , control = rpart.control(minsplit=2,cp=0,xval=0) )
      pred$pred.bag <- predict( fit.bag , newdata = dat_test )
      
      #rf
      
      fit.rf <- randomForest( goal_ind~. , data = dat_train )
      pred$pred.rf <- predict( fit.rf , newdata = dat_test )
      
      #gbm
      
      fit.gbm <- gbm( goal_ind~. , data = dat_train , interaction.depth = 2 )
      pred$pred.gbm <- predict( fit.gbm , newdata = dat_test , n.trees = 100 )
      
      #xgboost
      
      label1 <- as.matrix(dat_train$goal_ind)
      data1 <- as.matrix(dat_train[,-ncol(dat_train)])
      label2 <- as.matrix(dat_test$goal_ind)
      data2 <- as.matrix(dat_test[,-ncol(dat_test)])
      xgmat <- xgb.DMatrix( data1 , label = label1 , missing = -10000 )
      param <- list( "objective" = "reg:linear" , "silent" = 1 )
      fit.xgbst <- xgb.train( param , xgmat , nrounds = 10000 )
      pred$pred.xgbst <- predict( fit.xgbst , newdata = data2 )
      
    }
    
    
    
  }

  
    
  print(paste('预测基于',index,'的',span,type,'的结果','  ','训练集比例：',n,sep=''))
  
  return(pred)
  
  
}





