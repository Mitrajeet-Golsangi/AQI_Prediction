setwd("D:/VIT Projects/Second Year/1st Semester/DS CP/")
shell("cls")

library(caTools)
library(randomForest)

df <- read.csv("./Air Pollution/city_day.csv");
df <- df[, -c(1, 2)]

print(head(df))

df_cleaned <- na.omit(df)
row.names(df_cleaned) <- NULL
print(head(df_cleaned))
print(nrow(df_cleaned))

set.seed(123)

splitRatio = 0.8 # take in increaments of 0.2
split <- sample.split(df_cleaned, SplitRatio = splitRatio)

df_train <- subset(df_cleaned, split == TRUE) # Complete dataframe is taken
df_test <- subset(df_cleaned, split == FALSE)

rf <- randomForest(
    AQI~.,
    data = df_train,
    mtry = 3.4641016151377,
    ntree = 500,
    na.action = na.omit
)

pred <- predict(rf, df_test)

df_test <- cbind(df_test, pred)

cfm <- table(df_test$AQI, df_test$pred)

acc <- sum(diag(cfm) / sum(cfm))

cat("Accuraccy is : ", acc)
