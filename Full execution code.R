########### Installing libraries ############################################################
install.packages("factoextra")
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")
library(factoextra)
library(readxl)
#library(SFSI)
#library(energy)
#library(knitr)
library(clusterSim)
library(knitr)
library(neuralnet)
library(sigmoid)
library("writexl")
library(Metrics)
library(randomForest)
library(datasets)
library(caret)
#############################################################################################

####################### Importing Full data set for Environment Prediction ##################
y <- data.frame(read.csv("D:\\Grad school\\Lab\\clustring\\data\\FULL DATA.csv"))
#############################################################################################

################### Environment prediction ##################################################
## 75% of the sample size
smp_size <- floor(0.75 * nrow(y))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(y)), size = smp_size)

train <- y[train_ind, ]
test <- y[-train_ind, ]
train1<- train
## Binarize the categorical output
train <- cbind(train, train$ENV == 'normal')
train <- cbind(train, train$ENV == 'hot')
train <- cbind(train, train$ENV == 'cold')
train <- cbind(train, train$ENV == 'exercise')
names(train)[7:10] <- c('normal', 'hot', 'cold','exercise')
#train
## Training the neural network
nn <- neuralnet(
  normal+hot+cold+exercise ~ E+N+C,
  data=train, 
  hidden = 10 ,
  act.fct = leakyrelu,
  threshold = 0.01 ,
  err.fct = "sse",
  algorithm = "rprop+" ,
  linear.output = FALSE,
  stepmax=1e7
)
## Plot the NN
#plot(nn)
## Define the Softmax function
softmax <- function(par){
  n.par <- length(par)
  par1 <- sort(par, decreasing = TRUE)
  Lk <- par1[1]
  for (k in 1:(n.par-1)) {
    Lk <- max(par1[k+1], Lk) + log1p(exp(-abs(par1[k+1] - Lk))) 
  }
  val <- exp(par - Lk)
  return(val)
}
## Predict/Test
comp <- compute(nn, test[-6])
## Get weights for test data set
pred.weights <- comp$net.result
#pred.weights

## Get weights for train dataset
predi <-nn$net.result
#predi

## Save weights for test data set based on the 4 environments
ccc<- as.data.frame(pred.weights)
#ccc
ccc[,1]<-softmax(ccc[,1])
ccc[,2]<-softmax(ccc[,2])
ccc[,3]<-softmax(ccc[,3])
ccc[,4]<-softmax(ccc[,4])
#ccc
test['Normal'] = ccc[,1]
test['Hot'] = ccc[,2]
test['Cold'] = ccc[,3]
test['Exercise'] = ccc[,4]
#test

## Save weights for train data set based on the 4 environments
ddd<- as.data.frame(predi)
#ddd
ddd[,1]<-softmax(ddd[,1])
ddd[,2]<-softmax(ddd[,2])
ddd[,3]<-softmax(ddd[,3])
ddd[,4]<-softmax(ddd[,4])
#ddd
train1['Normal'] = ddd[,1]
train1['Hot'] = ddd[,2]
train1['Cold'] = ddd[,3]
train1['Exercise'] = ddd[,4]
#train1

## Combine train and test data sets that have the four env weights
qq<-rbind(x = train1, y = test)
#qq
## Save the whole weighted data set
write_xlsx(qq,"D:\\Grad school\\Lab\\clustring\\data\\qq2.xlsx")
########################################################################################



################# Individual variation type Estimation #################################

############## For Normal #####################################
z <- data.frame(read_excel("D:/Grad school/Lab/clustring/data/Normal class.xlsx"))
#z

## 75% of the sample size
smp_size <- floor(0.75 * nrow(z))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(z)), size = smp_size)

train <- z[train_ind, ]
test <- z[-train_ind, ]

nn <- neuralnet(
  clusters ~ E+N+C,
  data=train, 
  hidden = 10 ,
  act.fct = leakyrelu,
  threshold = 0.01 ,
  err.fct = "sse",
  algorithm = "rprop+" ,
  linear.output = FALSE,
  stepmax=1e7
)
#plot(nn)
## Predict
comp <- compute(nn, test[-11])
predi <-comp$net.result
#predi

#ccc<- as.data.frame(pred.weights)
ddd<- as.data.frame(predi)
ddd<- round(ddd)
colnames(ddd)<-"clus"
newD <-data.frame(z,ddd$clus)
names(newD)[names(newD) == 'ddd.clus'] <- 'estimated_clus'
################saving new dataset with clusters########
write_xlsx(newD,"D:\\Grad school\\Lab\\clustring\\data\\Normal class est.xlsx")

############## For Hot #####################################
z <- data.frame(read_excel("D:/Grad school/Lab/clustring/data/Hot class.xlsx"))
#z

## 75% of the sample size
smp_size <- floor(0.75 * nrow(z))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(z)), size = smp_size)

train <- z[train_ind, ]
test <- z[-train_ind, ]

nn <- neuralnet(
  clusters ~ E+N+C,
  data=train, 
  hidden = 10 ,
  act.fct = leakyrelu,
  threshold = 0.01 ,
  err.fct = "sse",
  algorithm = "rprop+" ,
  linear.output = FALSE,
  stepmax=1e7
)
#plot(nn)
## Predict
comp <- compute(nn, test[-11])
predi <-comp$net.result
#predi

#ccc<- as.data.frame(pred.weights)
ddd<- as.data.frame(predi)
ddd<- round(ddd)
colnames(ddd)<-"clus"
newD <-data.frame(z,ddd$clus)
names(newD)[names(newD) == 'ddd.clus'] <- 'estimated_clus'
################saving new dataset with clusters########
write_xlsx(newD,"D:\\Grad school\\Lab\\clustring\\data\\Hot class est.xlsx")

############## For Cold #####################################
z <- data.frame(read_excel("D:/Grad school/Lab/clustring/data/Cold class.xlsx"))
#z

## 75% of the sample size
smp_size <- floor(0.75 * nrow(z))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(z)), size = smp_size)

train <- z[train_ind, ]
test <- z[-train_ind, ]

nn <- neuralnet(
  clusters ~ E+N+C,
  data=train, 
  hidden = 10 ,
  act.fct = leakyrelu,
  threshold = 0.01 ,
  err.fct = "sse",
  algorithm = "rprop+" ,
  linear.output = FALSE,
  stepmax=1e7
)
#plot(nn)
## Predict
comp <- compute(nn, test[-11])
predi <-comp$net.result
#predi

#ccc<- as.data.frame(pred.weights)
ddd<- as.data.frame(predi)
ddd<- round(ddd)
colnames(ddd)<-"clus"
newD <-data.frame(z,ddd$clus)
names(newD)[names(newD) == 'ddd.clus'] <- 'estimated_clus'
################saving new dataset with clusters########
write_xlsx(newD,"D:\\Grad school\\Lab\\clustring\\data\\Cold class est.xlsx")

############## For Exercise #####################################
z <- data.frame(read_excel("D:/Grad school/Lab/clustring/data/Exercise class.xlsx"))
#z

## 75% of the sample size
smp_size <- floor(0.75 * nrow(z))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(z)), size = smp_size)

train <- z[train_ind, ]
test <- z[-train_ind, ]

nn <- neuralnet(
  clusters ~ E+N+C,
  data=train, 
  hidden = 10 ,
  act.fct = leakyrelu,
  threshold = 0.01 ,
  err.fct = "sse",
  algorithm = "rprop+" ,
  linear.output = FALSE,
  stepmax=1e7
)
#plot(nn)
## Predict
comp <- compute(nn, test[-11])
predi <-comp$net.result
#predi

#ccc<- as.data.frame(pred.weights)
ddd<- as.data.frame(predi)
ddd<- round(ddd)
colnames(ddd)<-"clus"
newD <-data.frame(z,ddd$clus)
names(newD)[names(newD) == 'ddd.clus'] <- 'estimated_clus'
################saving new dataset with clusters########
write_xlsx(newD,"D:\\Grad school\\Lab\\clustring\\data\\Exercise class est.xlsx")
########################################################################################

#################### Body temperature estimation #######################################

#################### 1/ For Normal ######################################
### Import dataset
v <- data.frame(read_excel("D:/Grad school/Lab/clustring/data/Normal class est.xlsx"))


######### 1-1 Using NN ##########
df_matrix <- model.matrix(~ BT + E + N + C  + Normal+ Hot +Cold +Exercise + estimated_clus ,data = v)[,-1]
df_matrix
df1_matrix <- matrix()
f <- as.formula(paste(c(colnames(df_matrix))[1], 
                      paste(c(colnames(df_matrix[,c(2:ncol(df_matrix))])), collapse = "+"), 
                      sep=" ~ "))

smp_siz <- floor(0.75*nrow(df_matrix))
set.seed(465) 
train_ind <- sample(seq_len(nrow(df_matrix)),size = smp_siz)
train_df <- as.data.frame(df_matrix[train_ind, ])
test_df <- as.data.frame(df_matrix[-train_ind, ])
#head(test_df)
#relu_copy <- function(x) ifelse(x>=0, x, 0)
nn <- neuralnet::neuralnet(f ,
                           data = train_df ,
                           hidden = 10 ,
                           act.fct = leakyrelu,
                           threshold = 0.01 ,
                           err.fct = "sse",
                           algorithm = "rprop+" ,
                           linear.output = TRUE,
                           stepmax=1e7)




### Extracting predicted BT for training dataset
predi <-unlist(nn$net.result)
#predi
est_BT <- data.frame(train_df$BT,predi)
colnames(est_BT)<- c('Original BT', 'Predicted BT')
write_xlsx(est_BT,"D:\\Grad school\\Lab\\clustring\\data\\Normal BT original vs predicted Train NN.xlsx")
##RMSE for training data
cat("RMSE for Training dataset using NN- Normal environment=",rmse(train_df$BT,predi))

### Predicting BT for testing dataset
nnp=predict(nn,test_df)
### Extracting predicted BT for testing dataset
est_BT1 <- data.frame(test_df$BT,nnp)
colnames(est_BT1)<- c('Original BT', 'Predicted BT')
write_xlsx(est_BT1,"D:\\Grad school\\Lab\\clustring\\data\\Normal BT original vs predicted Test NN.xlsx")
## RMSE for test dataset
cat("RMSE for Test dataset using NN - Normal environment=",rmse(test_df$BT,nnp))


##### 1-2 Using random forest ##############
##Performing RFR
rf <- randomForest(BT~., data=train_df, proximity=TRUE) 
## Trace the Forest
print(rf)
### Extracting predicted BT for training dataset
RFpre<-rf$predicted
est_BT2 <- data.frame(train_df$BT,RFpre)
colnames(est_BT2)<- c('Original BT', 'Predicted BT')
write_xlsx(est_BT2,"D:\\Grad school\\Lab\\clustring\\data\\Normal BT original vs predicted Train RFR.xlsx")
##RMSE for training data
cat("RMSE for Training dataset using RFR- Normal environment=",rmse(train_df$BT,RFpre))

### Predicting BT for testing dataset
y_pred = predict(rf, newdata = test_df)
### Extracting predicted BT for testing dataset
est_BT3 <- data.frame(test_df$BT,y_pred)
colnames(est_BT3)<- c('Original BT', 'Predicted BT')
write_xlsx(est_BT3,"D:\\Grad school\\Lab\\clustring\\data\\Normal BT original vs predicted Test RFR.xlsx")
## RMSE for test dataset
cat("RMSE for Test dataset using RFR - Normal environment=",rmse(test_df$BT,y_pred))

#####################################################################################

#################### 2/ For Hot ######################################
### Import dataset
v <- data.frame(read_excel("D:/Grad school/Lab/clustring/data/Hot class est.xlsx"))


######### 2-1 Using NN ##########
df_matrix <- model.matrix(~ BT + E + N + C  + Normal+ Hot +Cold +Exercise + estimated_clus ,data = v)[,-1]
df_matrix
df1_matrix <- matrix()
f <- as.formula(paste(c(colnames(df_matrix))[1], 
                      paste(c(colnames(df_matrix[,c(2:ncol(df_matrix))])), collapse = "+"), 
                      sep=" ~ "))

smp_siz <- floor(0.75*nrow(df_matrix))
set.seed(465) 
train_ind <- sample(seq_len(nrow(df_matrix)),size = smp_siz)
train_df <- as.data.frame(df_matrix[train_ind, ])
test_df <- as.data.frame(df_matrix[-train_ind, ])
#head(test_df)
#relu_copy <- function(x) ifelse(x>=0, x, 0)
nn <- neuralnet::neuralnet(f ,
                           data = train_df ,
                           hidden = 10 ,
                           act.fct = leakyrelu,
                           threshold = 0.01 ,
                           err.fct = "sse",
                           algorithm = "rprop+" ,
                           linear.output = TRUE,
                           stepmax=1e7)




### Extracting predicted BT for training dataset
predi <-unlist(nn$net.result)
#predi
est_BT <- data.frame(train_df$BT,predi)
colnames(est_BT)<- c('Original BT', 'Predicted BT')
write_xlsx(est_BT,"D:\\Grad school\\Lab\\clustring\\data\\Hot BT original vs predicted Train NN.xlsx")
##RMSE for training data
cat("RMSE for Training dataset using NN- Hot environment=",rmse(train_df$BT,predi))

### Predicting BT for testing dataset
nnp=predict(nn,test_df)
### Extracting predicted BT for testing dataset
est_BT1 <- data.frame(test_df$BT,nnp)
colnames(est_BT1)<- c('Original BT', 'Predicted BT')
write_xlsx(est_BT1,"D:\\Grad school\\Lab\\clustring\\data\\Hot BT original vs predicted Test NN.xlsx")
## RMSE for test dataset
cat("RMSE for Test dataset using NN - Hot environment=",rmse(test_df$BT,nnp))


##### 2-2 Using random forest ##############
##Performing RFR
rf <- randomForest(BT~., data=train_df, proximity=TRUE) 
## Trace the Forest
print(rf)
### Extracting predicted BT for training dataset
RFpre<-rf$predicted
est_BT2 <- data.frame(train_df$BT,RFpre)
colnames(est_BT2)<- c('Original BT', 'Predicted BT')
write_xlsx(est_BT2,"D:\\Grad school\\Lab\\clustring\\data\\Hot BT original vs predicted Train RFR.xlsx")
##RMSE for training data
cat("RMSE for Training dataset using RFR- Hot environment=",rmse(train_df$BT,RFpre))

### Predicting BT for testing dataset
y_pred = predict(rf, newdata = test_df)
### Extracting predicted BT for testing dataset
est_BT3 <- data.frame(test_df$BT,y_pred)
colnames(est_BT3)<- c('Original BT', 'Predicted BT')
write_xlsx(est_BT3,"D:\\Grad school\\Lab\\clustring\\data\\Hot BT original vs predicted Test RFR.xlsx")
## RMSE for test dataset
cat("RMSE for Test dataset using RFR - Hot environment=",rmse(test_df$BT,y_pred))

#####################################################################################

#################### 3/ For Cold ######################################
### Import dataset
v <- data.frame(read_excel("D:/Grad school/Lab/clustring/data/Cold class est.xlsx"))


######### 3-1 Using NN ##########
df_matrix <- model.matrix(~ BT + E + N + C  + Normal+ Hot +Cold +Exercise + estimated_clus ,data = v)[,-1]
df_matrix
df1_matrix <- matrix()
f <- as.formula(paste(c(colnames(df_matrix))[1], 
                      paste(c(colnames(df_matrix[,c(2:ncol(df_matrix))])), collapse = "+"), 
                      sep=" ~ "))

smp_siz <- floor(0.75*nrow(df_matrix))
set.seed(465) 
train_ind <- sample(seq_len(nrow(df_matrix)),size = smp_siz)
train_df <- as.data.frame(df_matrix[train_ind, ])
test_df <- as.data.frame(df_matrix[-train_ind, ])
#head(test_df)
#relu_copy <- function(x) ifelse(x>=0, x, 0)
nn <- neuralnet::neuralnet(f ,
                           data = train_df ,
                           hidden = 10 ,
                           act.fct = leakyrelu,
                           threshold = 0.01 ,
                           err.fct = "sse",
                           algorithm = "rprop+" ,
                           linear.output = TRUE,
                           stepmax=1e7)




### Extracting predicted BT for training dataset
predi <-unlist(nn$net.result)
#predi
est_BT <- data.frame(train_df$BT,predi)
colnames(est_BT)<- c('Original BT', 'Predicted BT')
write_xlsx(est_BT,"D:\\Grad school\\Lab\\clustring\\data\\Cold BT original vs predicted Train NN.xlsx")
##RMSE for training data
cat("RMSE for Training dataset using NN- Cold environment=",rmse(train_df$BT,predi))

### Predicting BT for testing dataset
nnp=predict(nn,test_df)
### Extracting predicted BT for testing dataset
est_BT1 <- data.frame(test_df$BT,nnp)
colnames(est_BT1)<- c('Original BT', 'Predicted BT')
write_xlsx(est_BT1,"D:\\Grad school\\Lab\\clustring\\data\\Cold BT original vs predicted Test NN.xlsx")
## RMSE for test dataset
cat("RMSE for Test dataset using NN - Cold environment=",rmse(test_df$BT,nnp))


##### 3-2 Using random forest ##############
##Performing RFR
rf <- randomForest(BT~., data=train_df, proximity=TRUE) 
## Trace the Forest
print(rf)
### Extracting predicted BT for training dataset
RFpre<-rf$predicted
est_BT2 <- data.frame(train_df$BT,RFpre)
colnames(est_BT2)<- c('Original BT', 'Predicted BT')
write_xlsx(est_BT2,"D:\\Grad school\\Lab\\clustring\\data\\Cold BT original vs predicted Train RFR.xlsx")
##RMSE for training data
cat("RMSE for Training dataset using RFR- Cold environment=",rmse(train_df$BT,RFpre))

### Predicting BT for testing dataset
y_pred = predict(rf, newdata = test_df)
### Extracting predicted BT for testing dataset
est_BT3 <- data.frame(test_df$BT,y_pred)
colnames(est_BT3)<- c('Original BT', 'Predicted BT')
write_xlsx(est_BT3,"D:\\Grad school\\Lab\\clustring\\data\\Cold BT original vs predicted Test RFR.xlsx")
## RMSE for test dataset
cat("RMSE for Test dataset using RFR - Cold environment=",rmse(test_df$BT,y_pred))

#####################################################################################


#################### 4/ For Exercise ######################################
### Import dataset
v <- data.frame(read_excel("D:/Grad school/Lab/clustring/data/Exercise class est.xlsx"))


######### 4-1 Using NN ##########
df_matrix <- model.matrix(~ BT + E + N + C  + Normal+ Hot +Cold +Exercise + estimated_clus ,data = v)[,-1]
df_matrix
df1_matrix <- matrix()
f <- as.formula(paste(c(colnames(df_matrix))[1], 
                      paste(c(colnames(df_matrix[,c(2:ncol(df_matrix))])), collapse = "+"), 
                      sep=" ~ "))

smp_siz <- floor(0.75*nrow(df_matrix))
set.seed(465) 
train_ind <- sample(seq_len(nrow(df_matrix)),size = smp_siz)
train_df <- as.data.frame(df_matrix[train_ind, ])
test_df <- as.data.frame(df_matrix[-train_ind, ])
#head(test_df)
#relu_copy <- function(x) ifelse(x>=0, x, 0)
nn <- neuralnet::neuralnet(f ,
                           data = train_df ,
                           hidden = 10 ,
                           act.fct = leakyrelu,
                           threshold = 0.01 ,
                           err.fct = "sse",
                           algorithm = "rprop+" ,
                           linear.output = TRUE,
                           stepmax=1e7)




### Extracting predicted BT for training dataset
predi <-unlist(nn$net.result)
#predi
est_BT <- data.frame(train_df$BT,predi)
colnames(est_BT)<- c('Original BT', 'Predicted BT')
write_xlsx(est_BT,"D:\\Grad school\\Lab\\clustring\\data\\Exercise BT original vs predicted Train NN.xlsx")
##RMSE for training data
cat("RMSE for Training dataset using NN- Exercise environment=",rmse(train_df$BT,predi))

### Predicting BT for testing dataset
nnp=predict(nn,test_df)
### Extracting predicted BT for testing dataset
est_BT1 <- data.frame(test_df$BT,nnp)
colnames(est_BT1)<- c('Original BT', 'Predicted BT')
write_xlsx(est_BT1,"D:\\Grad school\\Lab\\clustring\\data\\Exercise BT original vs predicted Test NN.xlsx")
## RMSE for test dataset
cat("RMSE for Test dataset using NN - Exercise environment=",rmse(test_df$BT,nnp))


##### 3-2 Using random forest ##############
##Performing RFR
rf <- randomForest(BT~., data=train_df, proximity=TRUE) 
## Trace the Forest
print(rf)
### Extracting predicted BT for training dataset
RFpre<-rf$predicted
est_BT2 <- data.frame(train_df$BT,RFpre)
colnames(est_BT2)<- c('Original BT', 'Predicted BT')
write_xlsx(est_BT2,"D:\\Grad school\\Lab\\clustring\\data\\Exercise BT original vs predicted Train RFR.xlsx")
##RMSE for training data
cat("RMSE for Training dataset using RFR- Exercise environment=",rmse(train_df$BT,RFpre))

### Predicting BT for testing dataset
y_pred = predict(rf, newdata = test_df)
### Extracting predicted BT for testing dataset
est_BT3 <- data.frame(test_df$BT,y_pred)
colnames(est_BT3)<- c('Original BT', 'Predicted BT')
write_xlsx(est_BT3,"D:\\Grad school\\Lab\\clustring\\data\\Exercise BT original vs predicted Test RFR.xlsx")
## RMSE for test dataset
cat("RMSE for Test dataset using RFR - Exercise environment=",rmse(test_df$BT,y_pred))

#####################################################################################