pacman::p_load(
  tidyverse,
  ggcorrplot
)

train <- read.csv("Data/train.csv")
test <- read.csv("Data/test.csv")

train <- train %>% mutate_if(is.character, ~na_if(., ""))

na_counta <- sapply(train, function(y) sum(length(which(is.na(y)))))
na_counta <- data.frame(na_counta)

names_num <- names(which(sapply(train, is.numeric)))

train_numeric <- train[, names_num]
cor <- round(cor(train_numeric), 1)

ggcorrplot(cor)

str(train)
    
train <- train %>% 
  mutate(date_of_last_rech_6 = as.POSIXct(mdy(date_of_last_rech_6)),
         date_of_last_rech_7 = as.POSIXct(mdy(date_of_last_rech_7)),
         date_of_last_rech_8 = as.POSIXct(mdy(date_of_last_rech_8)),
         date_of_last_rech_data_6 = as.POSIXct(mdy(date_of_last_rech_data_6)),
         date_of_last_rech_data_7 = as.POSIXct(mdy(date_of_last_rech_data_7)),
         date_of_last_rech_data_8 = as.POSIXct(mdy(date_of_last_rech_data_8)),
         last_date_of_month_6 = as.POSIXct(mdy(last_date_of_month_6)),
         last_date_of_month_7 = as.POSIXct(mdy(last_date_of_month_7)),
         last_date_of_month_8 = as.POSIXct(mdy(last_date_of_month_8))
         )


summary(train)

