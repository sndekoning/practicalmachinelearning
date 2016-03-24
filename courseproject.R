# Checking if data directory exist, and if not creating the directory and downloading the data.
if(!dir.exists("./data")){
  dir.create("./data")
  
  if(!file.exists("./data/training.csv")){
    train.url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
    download.file(train.url, "./data/training.csv")
    rm(train.url)
  }
  if(!file.exists("./data/testing.csv")){
    test.url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
    download.file(test.url, "./data/testing.csv")
    rm(test.url)
  }
}

library(caret)

# Reading in the data.
training <- read.csv("./data/training.csv", na.strings = c("", "NA"))
testing <- read.csv("./data/testing.csv", na.strings = "NA")

# Removing unneccesary variables such as participant name, timestamps, etc.

training <- training[, -c(1:7)]
# testing <- testing[, -c(1:7)]

#Removing variables with large (>0.9) amounts of missing values.
na <- NULL
for(i in 1:ncol(training)){
  na[i] <- sum(is.na(training[, i]))
}
high.na <- names(training[, na > (0.9*nrow(training))])
training <- training[, !names(training) %in% high.na]
# testing <- testing[, !names(testing) %in% high.na]

# Removing variables which are highly (>0.9) correlated to each other.
corr <- cor(training[, -length(training)])
cutoff <- findCorrelation(corr, cutoff = .9)
training <- training[, -cutoff]

# Creating a new partition to test the model on.
in.train <- createDataPartition(y = training$classe, p = .7, list = FALSE)

mod.train <- training[in.train ,]
mod.test <- training[ -in.train ,]

# fitting a random forest model.
rf.model <- train(classe ~ ., method = "rf", data = mod.train)

# predicting on the cross validation set.
pred.cv <- predict(rf.model, mod.test)

confusionMatrix(pred.cv, mod.test$classe)

final.pred <- predict(rf.model, testing)