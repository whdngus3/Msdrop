#First Code(Fixed case)


library(readxl)
library(caret)
library(ROCR)
library(kernlab)
library(dummies)
library(e1071)
library(caret)

df <- read.csv("raw_power.csv")
df<- df[,c(2:length(df))]
im_name <- 0
str(df)
names(df)
names(df)

flx <- c(1,2,3,5,6,8,10,11,12)
fix <- c(13,14,16)
df<-df[,c(flx,fix)]
for (i in 1:ldf){
  df[,i] <- (df[,i]-min(df[,i]))/(max(df[,i])-min(df[,i]))
}



df$종류 <- as.factor(df$종류)
df$범주화된_장르 <- as.factor(df$범주화된_장르)
names(df)
#df <- df[,-c(8,9,12)]

ddf <- dummy.data.frame(df)

ddf$흥행여부 <- as.factor(ddf$흥행여부)
str(ddf)

ldf <- length(ddf)



k_fold_result <- createFolds(ddf$흥행여부, k=5, list=TRUE, returnTrain = FALSE)
ac <-0
for (i in 1:5){
  
  
  tm <- k_fold_result[i]
  train_num<-0
  for (j in tm){
    train_num <- c(train_num,j)
  }
  train_num <- train_num[2:length(train_num)]
  
  x_train <- ddf[-train_num,c(1:ldf)]
  x_train[] <- lapply(x_train,as.numeric)
  x_train$흥행여부 <- as.factor(x_train$흥행여부)
  
  x_test <- ddf[train_num,c(1:ldf)]
  x_test[] <- lapply(x_test,as.numeric)
  x_test$흥행여부 <- as.factor(x_test$흥행여부)
  
  v <- svm(흥행여부~., data=x_train,kernel = 'radial',type = "C-classification")
  
  summary(v)
  table(predict(v,x_test[,c(1:ldf-1)]),x_test[,ldf])
  
  str(x_test)
  
  svm.predictions <- predict(v,x_test[,c(1:ldf-1)])
  f_predicted <- as.factor(svm.predictions)
  f_test <- as.factor(x_test[,ldf])
  
  cm <- confusionMatrix(f_predicted, f_test)
  cm
  ac <- c(ac,cm$overall[1])
  
  
  
  #특징중요도
  formula.init <- "흥행여부 ~ ."
  formula.init <- as.formula(formula.init)
  control <- trainControl(method="repeatedcv", number=10, repeats=2)
  model <- train(formula.init, data=x_train, method="svmRadial",
                 trControl=control)
  importance <- varImp(model, scale=FALSE)
  importance
  im_name <- c(im_name,rownames(importance$importance[1:10,][1]))
  
}


mean(ac[2:6])



#######################################
#Second Code (All case)

library(readxl)
library(caret)
library(ROCR)
library(kernlab)
library(dummies)
library(e1071)
library(caret)
library(gtools)

setwd('D:\\대청캠-프로젝트')

df <- read.csv("raw_power.csv")
df<- df[,c(2:length(df))]
ac_idx <- list(0)
idx<-1
str(df)
names(df)
l <- 0
for (num in 1:12){
  x <- combinations(12,num,1:12)
  for(rnum in 1:length(x[,1])){
    flx <- x[rnum,c(1:num)]
    
    fix <- c(13,14,16)
    print(l)
    l <- l+1
    df <- read.csv("raw_power.csv")
    df<- df[,c(2:length(df))]
    df<-df[,c(flx,fix)]
    #df<-df[,-c(7)]
    ldf <- length(df)
    for (i in 1:ldf){
      df[,i] <- (df[,i]-min(df[,i]))/(max(df[,i])-min(df[,i]))
    }
    
    if (length(df$종류)>0){
      df$종류 <- as.factor(df$종류)
    }
    if (length(df$범주화된_장르)>0){
      df$범주화된_장르 <- as.factor(df$범주화된_장르)
    }
    
    names(df)
    #df <- df[,-c(8,9,12)]
    
    ddf <- dummy.data.frame(df)
    
    ddf$흥행여부 <- as.factor(ddf$흥행여부)
    #str(ddf)
    
    ldf <- length(ddf)
    
    
    
    bc <- 0
    for(k in 1:5){
      
      
      k_fold_result <- createFolds(ddf$흥행여부, k=5, list=TRUE, returnTrain =     FALSE)
      ac <-0
      for (i in 1:5){
        
        
        tm <- k_fold_result[i]
        train_num<-0
        for (j in tm){
          train_num <- c(train_num,j)
        }
        train_num <- train_num[2:length(train_num)]
        
        x_train <- ddf[-train_num,c(1:ldf)]
        x_train[] <- lapply(x_train,as.numeric)
        x_train$흥행여부 <- as.factor(x_train$흥행여부)
        
        x_test <- ddf[train_num,c(1:ldf)]
        x_test[] <- lapply(x_test,as.numeric)
        x_test$흥행여부 <- as.factor(x_test$흥행여부)
        
        
        v <- svm(흥행여부~., data=x_train)
        
        summary(v)
        table(predict(v,x_test[,c(1:ldf-1)]),x_test[,ldf])
        
        
        svm.predictions <- predict(v,x_test[,c(1:ldf-1)])
        f_predicted <- as.factor(svm.predictions)
        f_test <- as.factor(x_test[,ldf])
        
        cm <- confusionMatrix(f_predicted, f_test)
        cm
        ac <- c(ac,cm$overall[1])
        
        
        
      }
      
      
      ac <-ac[2:6]
      bc <- c(bc,mean(ac))
    }
    
    mbc <- mean(bc[2:6])
    if (mbc > 0.67){
      ac_idx[[idx]] <- flx
      idx <- idx+1
      
    }
    
  }
}

bc
