library(randomGLM)
library(tidyverse)
library(data.table)
library(dummies)
library(caret)
library(plyr)

attach("./Code/supporting_functions.rda")

## may need to fix this and get both test and train to load in separately
set.seed(123)
setwd('C:/Users/Lance/Dropbox/Programming/R working directory/')
data_table <- fread('./Data/fire_contents.csv', na.strings=c(''), stringsAsFactors = T)
data_frame <- as.data.frame(data_table)

## process data
var_list <- c('company', 'channel', 'yearsInsured', 'stateRisk', 'ContStockOtherSI', 'suncorp_household_score', 'target')
cat_var_list <- c('company', 'channel', 'stateRisk')

data_frame$company <- mapvalues(data_frame$company, 
                               from=c("3","6","12","13","17"), 
                               to=c("VERO","AAMI","Resilium","GIO","GIO"))

data_subset <- data_frame[, noquote(var_list)]
                
data_subset <- data_subset %>% 
  mutate_each(funs(as.factor(.)), company, channel, stateRisk)                       

## one-hot encoding of categorical variables

company_encoding <- as.data.frame(dummy('company', data = data_subset, sep = "", drop = TRUE, fun = as.integer, verbose = FALSE))
channel_encoding <- as.data.frame(dummy('channel', data = data_subset, sep = "", drop = TRUE, fun = as.integer, verbose = FALSE))
stateRisk_encoding <- as.data.frame(dummy('stateRisk', data = data_subset, sep = "", drop = TRUE, fun = as.integer, verbose = FALSE))

data_full <- data.frame(select(data_subset,-c(company,channel,stateRisk)),company_encoding, channel_encoding, stateRisk_encoding)






## split data into train and test 
xTrain <- data_full[1:807,-4]
yTrain <- data_full[1:807, 4]
xTest <- data_full[-(1:807),-4]
yTest <- data_full[-(1:807), 4]

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

















