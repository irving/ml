# segment one: housekeeping
library(caret)
library(randomForest)
set.seed(41536)

# segment 2: data prep
print("read the data")
# NOTE the assumption that the data exists in the same folder as this code.
# training data can be downloaded from https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
# and test data from https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv
csv <- read.csv("pml-training.csv", stringsAsFactors = FALSE)
print("convert the classe column into a factor")
csv$classe <- as.factor(csv$classe)

removeThese <- c('user_name', 'cvtd_timestamp', 'X')
convert_columns <- function(cv) {
    
    print("remove useless columns")
    cv <- cv[, !(names(cv) %in% removeThese)]
    
    
    print("now convert all columns other than classe to numeric because a few were read as character")    
    for (i in 1:length(colnames(cv))){
        name <- colnames(cv)[i]
        
        if (name != "classe"){
            cv[, name] <- as.numeric(cv[, name])
            # putting in zeroes in place of the NAs so some kind of analysis is possible
            nas <- is.na(cv[, name])
            cv[, name][nas] <- 0
        }
    }
    
    return(cv)
}

csv <- convert_columns(csv)

print("data segmentation")
inTrain <- createDataPartition(y=csv$classe, p=0.70, list=FALSE)
training <- csv[inTrain,]
testing <- csv[-inTrain,]

print("cross validation")
f <- rfcv(training[, -77], training$classe)

print("fixing columns to reflect cross validation")
columns <- c(colnames(training)[f$n.var], "classe")
training <- training[, columns]
testing <- testing[, columns]

print("building the model")
rf <- randomForest(classe ~ ., data=training)
rf

print("predictions")
pt <- predict(rf, newdata=training)
cm <- confusionMatrix(pt, training$classe)
cm

print("testing")
ptt <- predict(rf, newdata=testing)
cmt <- confusionMatrix(ptt, testing$classe)
cmt

print("Doing the test stuff now")
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

csvt <- read.csv("pml-testing.csv", stringsAsFactors = FALSE)
csvt <- convert_columns(csvt)
preds <- predict(rf, newdata=csvt)
setwd("answers")
pml_write_files(preds)