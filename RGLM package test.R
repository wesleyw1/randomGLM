library(randomGLM)
library(tidyverse)
library(data.table)

setwd('C:/Users/Lance/Dropbox/Programming/R working directory/')
data_table <- fread('./Data/fire_contents.csv', na.strings=c(''), stringsAsFactors = T)
data_frame <- as.data.frame(data_table)

factor_list <- c("company", "channel", "yearsInsured", "stateRisk", "contStockOtherSI", "")


num_format <- function(df) {
  for (i in names(df)){
    if (!is.numeric(df[i])){
      df[i] <- as.numeric(as.factor(df$i))
    }
  }
}

num_format(data_frame)


data_frame["company"] <- as.factor(data_frame$"company")






























