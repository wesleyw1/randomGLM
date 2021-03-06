list.of.packages <- c("randomGLM", "tidyverse", "data.table", "plyr", "dummies", 
                      "caret", "rstudioapi", "installr")
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# library(installr)
# updateR()

library(rstudioapi)
library(randomGLM)
library(tidyverse)
library(data.table)
library(dummies)
library(caret)
library(plyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# attach("./Code/supporting_functions.rda")
source("./Code/supporting_functions_source.R")
# source("./Code/RGLM_package_functions.R")

## may need to fix this and get both test and train to load in separately
set.seed(123)
setwd('C:/Users/Lance/Dropbox/Programming/R working directory/')
data.in <- fread('./Data/fire_contents.csv', na.strings=c(''), stringsAsFactors = T)
emb.score <- as.data.frame(fread('./Data/fire_cso_size_predval.csv', na.strings=c(''), stringsAsFactors = T))
data.in <- as.data.frame(data.in)

## process data
var.list <- c('company', 'channel', 'yearsInsured', 'stateRisk', 'ContStockOtherSI', 'suncorp_household_score', 'target')
cat.var.list <- c('company', 'channel', 'stateRisk')

data.in$company <- mapvalues(data.in$company, 
                               from=c("3","6","12","13","17"), 
                               to=c("VERO","AAMI","Resilium","GIO","GIO"))

data.subset <- data.in[, noquote(var.list)]
                
data.subset <- data.subset %>% 
  mutate_at(c("company", "channel", "stateRisk"), funs(as.factor(.)))                       

## one-hot encoding of categorical variables

company.encoding <- as.data.frame(dummy('company', data = data.subset, sep = "", drop = TRUE, fun = as.integer, verbose = FALSE))
channel.encoding <- as.data.frame(dummy('channel', data = data.subset, sep = "", drop = TRUE, fun = as.integer, verbose = FALSE))
stateRisk.encoding <- as.data.frame(dummy('stateRisk', data = data.subset, sep = "", drop = TRUE, fun = as.integer, verbose = FALSE))

data.full <- data.frame(select(data.subset,-c(company,channel,stateRisk)),company.encoding, channel.encoding, stateRisk.encoding)

ind <- 799
train.prop <- 0.8
target.loc <- grep("target", colnames(data.full))

## split data into train, CV and test 
xTrain <- data.full[1:ind,-target.loc][1:floor(train.prop*ind),]
yTrain <- data.full[1:ind, target.loc][1:floor(train.prop*ind)]

xCV <- data.full[1:ind,-target.loc][-(1:floor(train.prop*ind)),]
yCV <- data.full[1:ind, target.loc][-(1:floor(train.prop*ind))]

xTest <- data.full[-(1:ind),-target.loc]
yTest <- data.full[-(1:ind), target.loc]

## fit model
# simple version
# RGLM <- randomGLM(xTrain,
#                   yTrain,
#                   classify=FALSE, 
#                   nBags =100,
#                   randomSeed = 123,
#                   nFeaturesInBag = 20,
#                   keepModels=TRUE)

## parameter tuning for RGLM
nbag <- seq(25, 150, 25)
model.list <- list()
CV.list <- list()
CV <- data.frame(cbind(xCV, yCV))

for (bag.cnt in nbag) {
  paste("now fitting model with number of bags =", bag.cnt, sep = " ")  
  RGLM.CV <- randomGLM(xTrain,
                    yTrain,
                    classify = F, 
                    nBags = bag.cnt,
                    randomSeed = 123,
                    nFeaturesInBag = 20,
                    nCandidateCovariates = 20,
                    mandatoryCovariates = 2,
                    keepModels=TRUE)
  

  # Store model outputs and gains
  model.list[[toString(bag.cnt)]] <- RGLM.CV  
  CV$model_score_test <- predict(model.list[[toString(bag.cnt)]], xCV)
  CV.list[toString(bag.cnt)] <- CalculateGains(CV, "yCV", "model_score_test")[3]
  
  CV <- subset(CV, select = -model_score_test)
    # Test[, !(names(Test) == "model_score_test")]
}

CV.list
# OUTPUT
# $`25`
# [1] 0.4122131
# 
# $`50`
# [1] 0.4389428
# 
# $`75`
# [1] 0.4479827
# 
# $`100`
# [1] 0.4473153
# 
# $`125`
# [1] 0.4195315
# 
# $`150`
# [1] 0.417666
best.bag.cnt <- 75 # manually adjust this after CV


## Feature selection
varImp = model.list[[toString(best.bag.cnt)]]$timesSelectedByForwardRegression
sum(varImp>0)
varImp
table(varImp)


empty <- varImp[order(sapply(varImp,'[[',1))]


# select most important features
impF = colnames(xTrain)[varImp>=5]
impF

## Coefficients
coef(model.list[[toString(best.bag.cnt)]]$models[[30]])

nBags = length(model.list[[toString(best.bag.cnt)]]$featuresInForwardRegression)
coefMat = matrix(0, nBags, model.list[[toString(best.bag.cnt)]]$nFeatures)
for (i in 1:nBags)
{
  coefMat[i, RGLM$featuresInForwardRegression[[i]]] = RGLM$coefOfForwardRegression[[i]]
}

coefMean = apply(coefMat, 2, mean)
names(coefMean) = colnames(xTrain)
summary(coefMean)
coefMean[impF]

## Single tree randomGLM with no replacement
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("./Code/RGLM_package_functions.R")
Singl_RGLM <- randomGLM_mod(xTrain,
                            yTrain,
                            classify=F,
                            nBags =1,
                            replace = FALSE,
                            nObsInBag = ind,
                            randomSeed = 123,
                            nFeaturesInBag = 28,
                            nCandidateCovariates = 28,
                            verbose = 1,
                            nThreads = 1,
                            keepModels=TRUE)

## Score test data
xTest$model.score <- predict(model.list[["75"]], xTest, type="response")

#manual score
xTest <- mutate(xTest, singl.score = 28214.801 + 0.102*ContStockOtherSI + 26185.867*channelBISA + -74055.019*stateRiskACT)
# xTest$singl_score <- predict.randomGLM_mod(Singl_RGLM, xTest, type="response")
Test <- data.frame(cbind(xTest, yTest, emb.score))

# Test_2 <- data.table(Test)

## Assess model performance

CalculateGains(Test, "yTest", "model.score") # randomGLM with CVed trees
# Model gains   Max gains   Model/max 
# 0.1705993   0.3702753   0.4607363 
CalculateGains(Test, "yTest", "singl.score") # randomGLM with one tree
# Model gains   Max gains   Model/max 
# 0.1553151   0.3702753   0.4194584 
CalculateGains(Test, "yTest", "EmblemPred")


setwd('C:/Users/Lance/Dropbox/Programming/R working directory/')















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

















