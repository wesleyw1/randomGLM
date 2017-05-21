library(randomGLM)
library(tidyverse)
library(data.table)

data_model <- fread('Data/fire_contents_model.csv', na.strings=c(''))
data_test <- fread('Data/fire_contents_test.csv', na.strings=c(''))

num_format <- function(df) {
  for (i in names(df)){
    if (!is.numeric(df[i])){
      df[i] <- as.numeric(as.factor(df$i))
    }
  }
}
num_format(data_test)

































