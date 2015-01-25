library(rpart)
library(caret)
library(party)

setwd("~/Desktop/Fitness Device Project - Machine Learning")

# replace DIV/0 errors with NA

training <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
testing <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))

# remove columns X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, num_window

training <- training[colnames(training[-(1:7)])]

testing <- testing[colnames(testing[-(1:7)])]

# make all formats numeric except for last column (classe)

for(i in c(1:152)) {training[,i] = as.numeric(as.character(training[,i]))}
for(i in c(1:152)) {testing[,i] = as.numeric(as.character(testing[,i]))}

# let's also remove all near zero values

nzv_vars <- nzv(training, saveMetrics = TRUE)

remove_list_nzv_vars <- rownames(nzv_vars[which(nzv_vars$nzv==TRUE),])

# create training and testing sets without nzv_vars

remove_list <- names(training) %in% remove_list_nzv_vars

training <- training[!remove_list]
testing <- testing[!remove_list]

# create testing and training subsets

inTrain <- createDataPartition(y=training$classe, p=0.8, list=FALSE )
sub_training <- training[inTrain,]
sub_testing <- training[-inTrain,]

# create model

set.seed(793462)

fit <- rpart(classe ~ ., data=sub_training, method="class")

predictions <- predict(fit, sub_testing, type = "class")

# look at the confusion matrix

confusionMatrix(predictions, sub_testing$classe)

# now use on testing set

predictions_final <- predict(fit, testing, type="class")

# let's see if we can improve on accuracy by using random forest

# remove columns that are all NA

sub_training <- sub_training[,colSums(is.na(sub_training))<nrow(sub_training)]

data.controls <- cforest_unbiased(ntree=500, mtry=3, trace=T)

data.cforest <- cforest(classe ~., data = sub_training, controls=data.controls)

sub_testing.pred <- predict(data.cforest, sub_testing, OOB=TRUE)

confusionMatrix(table(sub_testing.pred, sub_testing$classe))

# make final prediction for 20 row set

predict(data.cforest, testing, OOB=TRUE)





