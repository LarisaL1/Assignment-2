#libraries
library(caret)
#install.packages("ISLR") # only install if needed
library(ISLR)
#import data
data.origin <- read.csv("~/Documents/Assignment 2/UniversalBank.csv")

#data cleaning
data <- data.origin[,-c(1,5)]
data$Personal.Loan <- as.factor(data$Personal.Loan)
view(data)

#split data
set.seed(14)
Test_Index = createDataPartition(UniversalBank$`Personal Loan`,p=0.6, list=FALSE) # 60% reserved for Test
Test_Data = UniversalBank[Test_Index,]
TraVal_Data = UniversalBank[-Test_Index,] # Validation and Training data is rest

Train_Index = createDataPartition(TraVal_Data$`Personal Loan`,p=0.4, list=FALSE) # 40% of remaining data as training
Train_Data = TraVal_Data[Train_Index,]
Validation_Data = TraVal_Data[-Train_Index,] # rest as validation

summary(Train_Data)
summary(Validation_Data)

# Copy the original data
train.norm.df <-Train_Data
valid.norm.df <- Validation_Data
traval.norm.df <- TraVal_Data

norm.values <-preProcess(Train_Data[, -c(1,5)], method=c("center","scale"))
train.norm.df[,-c(1,5)] <-predict(norm.values, Train_Data[, -c(1,5)]) # Replace columns (except for 1 and 5) with normalized values
valid.norm.df[, -c(1,5)] <- predict(norm.values, Validation_Data[, -c(1,5)])
traval.norm.df[, -c(1,5)] <- predict(norm.values, traval.norm.df[, -c(1,5)])

summary(train.norm.df)
var(train.norm.df[, -c(1,5)])
summary(valid.norm.df)
var(valid.norm.df[, -c(1,5)])


#modeling k-NN
library(FNN)
nn <- knn(train = train.norm.df[, -c(1,5)], test = train.norm.df
          cl = train.norm.df[, 0.5], k=1, prob=TRUE) #we use k = 1, and Loan Acceptance is the Y
row.names(Train_Data)[attr(, Train_Data)]


#Q1: predict for one obs.

new.data <- data.frame("Age=40, Experience=10, Income=84, Family=2, CCAvg=2, Education_1=0, Education_2=1, Education_3=0, Mortgage=0, Securities Account=0, CD Account = 0, Online =1, Credit Card = 1, )
new.data.norm <- predict(norm.values, new.data)
predict(new.data.norm)


norm.values <- preProcess(TraVal_Data [, -c(1,5)], method=c(center, scale)) # Use combined set to normalize
traval.norm.df[, -c(1,5)] <- predict(norm.values, TraVal_Data[, -c(1,5)]
test.norm.df[, -c(1,5)] <- predict (norm.values, Test_Data [, -c(1,5)]
summary(traval.norm.df)
summary(train.norm.df)


Now we predict for the test set.


knn.pred.new <- knn(traval.norm.df[, 1:2], test.norm, cl = traval.norm[, 3], k = 9)
row.names(TraVal_Data),[attr(nn, "nn.index")]


#normalization


norm_model<-preProcess(Default, method = c(center, scale))
Default_normalized<-predict(norm_model,Default)
summary(Default_normalized)
sd(Default_normalized$balance)
                       
set.seed(123)
Serach_grid <- expand.grid(k=-c(1,5))
model <-train(default~balance+income, data=Default_normalized, method="knn", tuneGrid=Serach_grid`)
                       

### train                   
set.seed(123)
Serach_grid <- expand.grid(k=-c(1,5))
model<-train(Age~balance+income, data=Personal.Loan, method=knn, tuneGrid=Serach_grid,preProcess="range"
                                    
# k-NN Class Package

library(class)
library(caret)
library(ISLR)
Summary(Personal.Loan)
                       
# Let us now Normalize the data
                       
norm_model<-preProcess(data, method = c(center, scale))
Default_normalized<-predict(norm_model,Personal.Loan)
                       
We now predict Default using income and balance
                       
Default_normalized<-Default_normalized[,-c(1,5)]
                       
Train_Data <-createDataPartition(Default_normalized$data, p=0.6, list=FALSE)
Train <-Default_normalized[Train_Data,]
Test  <-Default_normalized[-Train_Data,]
                       
                       
Train_Predictors <- Train [,-c(1,5)]  
Test_Predictors <- Validation [,-c(1,5)]

## Confusion Matrix

library("gmodels)
CrossTable(x=Test_labels,y=Predicted_Test_labels, prop.chisq=FALSE)
                       
## Probabillity Output

Predicted_Test_Labels <- knn(Train_Data, valid.norm.df, cl=Train_Index, k=1, prob=TRUE)
                       
