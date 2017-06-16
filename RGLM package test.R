list.of.packages <- c("randomGLM", "tidyverse", "data.table", "plyr", "dummies", "caret")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(randomGLM)
library(tidyverse)
library(data.table)
library(dummies)
library(caret)
library(plyr)

# attach("./Code/supporting_functions.rda")
source("./Code/supporting_functions_source.R")

## may need to fix this and get both test and train to load in separately
set.seed(123)
setwd('C:/Users/Lance/Dropbox/Programming/R working directory/')
data.in <- fread('./Data/fire_contents.csv', na.strings=c(''), stringsAsFactors = T)
data.in <- as.data.frame(data.in)

## process data
var.list <- c('company', 'channel', 'yearsInsured', 'stateRisk', 'ContStockOtherSI', 'suncorp_household_score', 'target')
cat.var.list <- c('company', 'channel', 'stateRisk')

data.in$company <- mapvalues(data.in$company, 
                               from=c("3","6","12","13","17"), 
                               to=c("VERO","AAMI","Resilium","GIO","GIO"))

data.subset <- data.in[, noquote(var.list)]
                
data.subset <- data.subset %>% 
  mutate_each(funs(as.factor(.)), company, channel, stateRisk)                       

## one-hot encoding of categorical variables

company.encoding <- as.data.frame(dummy('company', data = data.subset, sep = "", drop = TRUE, fun = as.integer, verbose = FALSE))
channel.encoding <- as.data.frame(dummy('channel', data = data.subset, sep = "", drop = TRUE, fun = as.integer, verbose = FALSE))
stateRisk.encoding <- as.data.frame(dummy('stateRisk', data = data.subset, sep = "", drop = TRUE, fun = as.integer, verbose = FALSE))

data.full <- data.frame(select(data.subset,-c(company,channel,stateRisk)),company.encoding, channel.encoding, stateRisk.encoding)



## split data into train and test 
xTrain <- data.full[1:807,-4]
yTrain <- data.full[1:807, 4]
xTest <- data.full[-(1:807),-4]
yTest <- data.full[-(1:807), 4]

## fit model
RGLM <- randomGLM(xTrain,
                  yTrain,
                  classify=F, 
                  nBags =100,
                  randomSeed = 123,
                  nFeaturesInBag = 20,
                  keepModels=TRUE)

## Single glm iteration
Singl_RGLM <- randomGLM(xTrain,
                        yTrain,
                        classify=F, 
                        nBags =1,
                        replace = F,
                        nObsInBag = 807,
                        randomSeed = 123,
                        nFeaturesInBag = 28,
                        keepModels=TRUE)


## Feature selection
varImp = RGLM$timesSelectedByForwardRegression
sum(varImp>0)
varImp
table(varImp)
# select most important features
impF = colnames(xTrain)[varImp>=5]
impF


## Coefficients
coef(RGLM$models[[30]])

nBags = length(RGLM$featuresInForwardRegression)
coefMat = matrix(0, nBags, RGLM$nFeatures)
for (i in 1:nBags)
{
  coefMat[i, RGLM$featuresInForwardRegression[[i]]] = RGLM$coefOfForwardRegression[[i]]
}

coefMean = apply(coefMat, 2, mean)
names(coefMean) = colnames(xTrain)
summary(coefMean)
coefMean[impF]

## Score test data
xTest$model_score <- predict(RGLM, xTest, type="response")
xTest$singl_score <- predict(Singl_RGLM, xTest, type="response")
Test <- data.frame(cbind(xTest, yTest))

Test_2 <- data.table(Test)

## Assess model performance
CalculateGains(Test_2, "yTest", "model_score")
CalculateGains(Test, "yTest", "singl_score")


CalculateGains(model.data, "renew", "xgb.gbm1.pred", plot=T)

















### JUNK
# data_subset <- transmute(data_subset, data_subset[[x]] <- ifelse(x %in% cat_var_list, as.factor(data_subset[[x]]))
# if (x %in% cat_var_list) as.factor(datasubset[[x]]))
for (x in cat_var_list) {
  assign(paste('onehot_', gsub('"','',x), sep = '_'), x) <- dummy(x, data = data_subset, sep = "", drop = TRUE, fun = as.integer, verbose = FALSE)
}



num_format <- function(df, in_list) {
  for (i in in_list){
    df[i] <- as.factor(df[[i]])
  }
}

num_format(data_subset, cat_var_list)


data_frame["company"] <- as.factor(data_frame$"company")



if (!is.numeric(df[i])){
  df[i] <- as.numeric(as.factor(df$i))
}





DF_list = list(select(data_subset,-c(company,channel,stateRisk)), company_encoding, channel_encoding, stateRisk_encoding)
temp = Reduce(function(...) merge(..., all=T), DF_list)
temp <- select(temp, -c(Row.names, Row.names.x, Row.names.y))





sample <- sample(seq_len(nrow(data_subset)), size = floor(.75*nrow(data_subset)))

train_set <- data_subset[sample, ]
test_set <- data_subset[-sample, ]




#temp <- merge(merge(select(data_subset,c(company,channel)), company_encoding, by=0), 
#              merge(channel_encoding, stateRisk_encoding, by=0), by=0)

















