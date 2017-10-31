library(ggplot2)
library(gridExtra)

getwd()
train <- read.table("train.csv", sep=",", header=TRUE)
test <- read.table("test.csv", sep=",", header=TRUE)


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


```

Contrary to the suggested separation of the variables, it seems reasonable to use the variables Medical_History_2  and Medical_History_10 as continuous.

What are the dimensions of the datasets?

```{r}
nrow(train.cat)

cat("Train data has", nrow(train), "rows and", ncol(train), "columns! \n")
cat("Test data has", nrow(test), "rows and", ncol(test), "columns! \n")


```

#In the above structure commands we saw missing data, how much is it?


```{r}

sum(is.na(train)) / (nrow(train) * ncol(train))
sum(is.na(test)) / (nrow(test) * ncol(test))

apply(train, 2, function(x) { sum(is.na(x)) })
apply(test, 2, function(x) { sum(is.na(x)) })

train.na.per.response <- sapply(sort(unique(train$Response)), function(x) { apply(train[train$Response == x, ], 2, function(y) { sum(is.na(y)) }) })
train.na.per.response
round(colSums(train.na.per.response) / sum(train.na.per.response), digits=4)


#Data with response equal 8 has the most and response equal 3 the least missing data.



#Are there any duplicate rows?

```{r}
cat("Train data set - Number of duplicated rows:", nrow(train) - nrow(unique(train)), "\n")
cat("Test data set - Number of duplicated rows:", nrow(test) - nrow(unique(test)), "\n")

```

Are there any constant columns?

```{r}
train.const <- sapply(train, function(x) { length(unique(x)) == 1 })
test.const <- sapply(test, function(x) { length(unique(x)) == 1 })
cat("Train data set - Number of constant columns:", sum(train.const), "\n")
cat("Test data set - Number of constant columns:", sum(test.const), "\n")
```
