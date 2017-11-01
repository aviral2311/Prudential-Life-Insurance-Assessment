library(ggplot2)
library(gridExtra)
library(dplyr)
library(xgboost)
getwd()
setwd("C:/Users/ssuman/Documents")
train <- read.table("train.csv", sep=",", header=TRUE)
test <- read.table("test.csv", sep=",", header=TRUE)

#removing rows which are not in the test data

train=dplyr::filter(train, Product_Info_7!=2)
train=dplyr::filter(train, Insurance_History_3!=2)
train=dplyr::filter(train, Medical_History_5!=3)
train=dplyr::filter(train, Medical_History_6!=2)
train=dplyr::filter(train, Medical_History_9!=3)
train=dplyr::filter(train, Medical_History_12!=1)
train=dplyr::filter(train, Medical_History_16!=2)
train=dplyr::filter(train, Medical_History_17!=1)
train=dplyr::filter(train, Medical_History_23!=2)
train=dplyr::filter(train, Medical_History_31!=2)
train=dplyr::filter(train, Medical_History_37!=3)
train=dplyr::filter(train, Medical_History_41!=2)


#Separate categorical, continous and discrete variables

cat.var.names <- c(paste("Product_Info_", c(1:3,5:7), sep=""), paste("Employment_Info_", c(2,3,5), sep=""),
                   paste("InsuredInfo_", 1:7, sep=""), paste("Insurance_History_", c(1:4,7:9), sep=""), 
                   "Family_Hist_1", paste("Medical_History_", c(2:14, 16:23, 25:31, 33:41), sep=""))
cont.var.names <- c("Product_Info_4", "Ins_Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", 
                    "Employment_Info_6", "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", 
                    "Family_Hist_5")
disc.var.names <- c("Medical_History_1", "Medical_History_15", "Medical_History_24", "Medical_History_32", 
                    paste("Medical_Keyword_", 1:48, sep=""))
train.cat<- train[,cat.var.names]
test.cat<-test[,cat.var.names]
train.cont <- train[, cont.var.names]
test.cont <- test[, cont.var.names]

train.disc <- train[, disc.var.names]
test.disc <- test[, disc.var.names]

train.cat <- as.data.frame(lapply(train.cat, factor))
test.cat <- as.data.frame(lapply(test.cat, factor))
train.cat

str(train.cont)
str(train.disc)
str(test.cont)
str(test.disc)
  
summary(train.cont)
summary(train.disc)

summary(test.cont)
summary(test.disc)

#Take a look at the data with categorical features:

str(train.cat)
str(test.cat)

summary(train.cat)
summary(test.cat)


#Contrary to the suggested separation of the variables, it seems reasonable to use the variables Medical_History_2  and Medical_History_10 as continuous.

#What are the dimensions of the datasets?

nrow(train.cat)

cat("Train data has", nrow(train), "rows and", ncol(train), "columns! \n")
cat("Test data has", nrow(test), "rows and", ncol(test), "columns! \n")


#In the above structure commands we saw missing data, how much is it?

sum(is.na(train)) / (nrow(train) * ncol(train))
sum(is.na(test)) / (nrow(test) * ncol(test))

apply(train, 2, function(x) { sum(is.na(x)) })
apply(test, 2, function(x) { sum(is.na(x)) })

train.na.per.response <- sapply(sort(unique(train$Response)), function(x) { apply(train[train$Response == x, ], 2, function(y) { sum(is.na(y)) }) })
train.na.per.response
round(colSums(train.na.per.response) / sum(train.na.per.response), digits=4)


#Data with response equal 8 has the most and response equal 3 the least missing data.



#Are there any duplicate rows?


cat("Train data set - Number of duplicated rows:", nrow(train) - nrow(unique(train)), "\n")
cat("Test data set - Number of duplicated rows:", nrow(test) - nrow(unique(test)), "\n")



#Are there any constant columns?


train.const <- sapply(train, function(x) { length(unique(x)) == 1 })
test.const <- sapply(test, function(x) { length(unique(x)) == 1 })
cat("Train data set - Number of constant columns:", sum(train.const), "\n")
cat("Test data set - Number of constant columns:", sum(test.const), "\n")


print(dim(train))
print(head(train, n=5))
print(dim(test))
print(head(test, n=5))


test$Response = 0

testId = test$Id
train$Id = test$Id = NULL

train[is.na(train)] <- -1
test[is.na(test)] <- -1

train$Product_Info_2_char = as.factor(substr(train$Product_Info_2, 1,1))
train$Product_Info_2_num = as.factor(substr(train$Product_Info_2, 2,2))
test$Product_Info_2_char = as.factor(substr(test$Product_Info_2, 1,1))
test$Product_Info_2_num = as.factor(substr(test$Product_Info_2, 2,2))

train$BMI_Age <- train$BMI * train$Ins_Age
test$BMI_Age <- test$BMI * test$Ins_Age

response <- train$Response
train$Response <- NULL

train$Medical_History_10 <- NULL
train$Medical_History_24 <- NULL

test$Medical_History_10 <- NULL
test$Medical_History_24 <- NULL

feature.names <- colnames(train)
feature.names
for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}
train
dtrain<-xgb.DMatrix(data=data.matrix(train[,feature.names]),label=response, missing=NA)
watchlist<-list(val=dtrain,train=dtrain)

param <- list(  objective           = "reg:linear", 
                booster             = "gbtree",
                eta                 = 0.05, # 0.06, #0.01,
                max_depth           =  6, #changed from default of 8
                subsample           = 0.8, # 0.7
                min_child_weight    = 25,
                colsample_bytree    = 0.7, # 0.7
                silent              = 0
)

set.seed(seed)
clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 500, 
                    verbose             = 1,  
                    print.every.n       = 10,
                    watchlist           = watchlist,
                    maximize            = FALSE
)
set.seed(seed)
clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 500, 
                    verbose             = 1,  
                    print.every.n       = 10,
                    watchlist           = watchlist,
                    maximize            = FALSE
)

dtest<-xgb.DMatrix(data=data.matrix(test[,feature.names]), missing = NA)
dtest
pred <- predict(clf, dtest)
pred
