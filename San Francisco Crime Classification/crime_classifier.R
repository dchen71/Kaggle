library(MASS)
library(readr)
#library (rpart)
library(caret)
library(lubridate)


treatment<-function(fname){
    df<-read.csv(paste0('Data/',fname))
    Dates1  = strptime(as.character(df$Dates),"%Y-%m-%d %H:%M:%S")
    print(str(Dates1))
    df$Year = Dates1$year
    df$Month = Dates1$mon
    df$Hour = as.numeric(format(ymd_hms(Dates1), "%H"))
    df$Loc = as.factor(paste(round(df$X,2), round(df$Y,2), sep= " "))
    df$XY <- -df$X * df$Y    
    df$XandY <- -df$X + df$Y
    df$AddOf <- sapply(df$Address, FUN=function(x) {strsplit(as.character(x), split="of ")[[1]][2]})
    df$AddType <- as.factor(ifelse(is.na(df$AddOf ),1,2))
    return(df)
}
train<-treatment('train.csv')
print(str(train))
print('Read train data complete...')
test<-treatment('test.csv')
print('Read test data complete...')

##split train data into 10 paritions due to memory space constraint
inTrain<-createDataPartition(train$Category,p=0.55,list=F)
train.sub<-train[inTrain,]

rpart.train<-function(train,test){
    submission<-data.frame(Id=test$Id)
    response<-data.frame(Cat=train$Category)
    
    crime<-as.character(unique(train$Category))
    crime<-sort(crime)
    for (i in crime){
        response[i]<- 0
        response[i][response$Cat==i,]<- 1
        fit<-glm(response[,i]~PdDistrict+X+Y+XY+XandY+AddType+DayOfWeek+Year+Hour+Month ,data=train, family = binomial)
        pred <- predict(fit,test, type = "response")
        submission[i]<-pred
        print(paste0(ncol(submission)/length(crime)*100,'% completed'))
    }
    return(submission)
}
submission<-rpart.train(train.sub,test)
rm(train, test,train.sub)
gz_out <- gzfile("submit.csv.gz", "w")
writeChar(write_csv(submission, ""), gz_out, eos=NULL)
close(gz_out)