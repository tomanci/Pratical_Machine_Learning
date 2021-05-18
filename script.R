#https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
#https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv
library(caret)
library(ggplot2)
library(rattle)
library(randomForest)


pml_training<- read.csv("/Users/tommasoancilli/Downloads/pml-training.csv")
pml_testing <- read.csv("/Users/tommasoancilli/Downloads/pml-testing.csv")

na_value <- colMeans(is.na(pml_training))
na_value[which(na_value>0.5)]
na_value <- as.data.frame(na_value)
na_value$name <- names(colMeans(is.na(pml_training)))
ggplot(data=na_value,aes(x=na_value,y=name))+geom_bar(stat='identity')

#Data cleaning

na_value_elimination <- sapply(pml_training,function (x) mean(is.na(x))>0.90)

pml_training <- pml_training[,na_value_elimination==F]
pml_testing <- pml_testing[,na_value_elimination==F]

pml_training <- subset(pml_training,select=-c(1:7))
pml_testing <- subset(pml_testing,select = -c(1:7)) #invece di farlo a mano sarebbe più figo automatizzarlo

index_value <- nearZeroVar(pml_training)

pml_training <- pml_training[,-index_value]
pml_testing <- pml_testing[,-index_value]

#Data partition

partition_index <- createDataPartition(pml_training$classe,p=0.7,list = F)

training <- pml_training[partition_index,]
testing <- pml_training[-partition_index,]

#model fitting

mod_rf <- train(classe~.,data=training,method="rf",ntree=100)
Predrf<- predict(mod_rf,testing)
message("Statistics of randon forest model")
confusionMatrix(Predrf,as.factor(testing$classe))
plot(mod_rf$finalModel)

mod_gbm <- train(classe~.,data=training,method="gbm",verbose=F)
Predgbm <- predict(mod_gbm,testing)
message("Statistics of gbm")
confusionMatrix(Predgbm,as.factor(testing$classe))
plot(mod_gbm$finalModel)

fit_pca <- train(classe~.,data=training,method="rpart",preProcess="pca")
PredPCA <- predict(fit_pca,testing)
message("Statistics of r part with a pca")
confusionMatrix(PredPCA,as.factor(testing$classe))
plot(fit_pca)

mod_lda <- train(classe~.,data=training,method="lda",verbose=F)
Predlda<- predict(mod_lda,testing)
message("Statistics of  lda")
confusionMatrix(Predlda,as.factor(testing$classe))

# here works the command predict(mod_,pml_testing)

predict(mod_rf,pml_testing)

