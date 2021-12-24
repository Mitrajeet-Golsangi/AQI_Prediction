setwd("D:/VIT Projects/Second Year/1st Semester/DS CP/")
shell("cls")

library(caTools)
library(randomForest)
library(mice)

set.seed(123)

split_ratio <- 0.8 # take in increaments of 0.2
split <- sample.split(df_impute, SplitRatio = splitRatio)

df_train <- subset(df_impute, split == TRUE) # Complete dataframe is taken
df_test <- subset(df_impute, split == FALSE)

lm1 <- lm(AQI ~ ., data = df_train)
print(summary(lm1))

pred1 <- predict.lm(lm1, df_test, type = "response")
print(head(pred1))

avg_y <- mean(df_impute$AQI)
cat("avg_AQI = ", avg_y, "\n")

ssr <- 0
for (i in pred1) {
    ssr <- ssr + (i - avg_y)^2
}
cat("SSR = ", ssr, "\n")

sst <- 0
for (i in df_impute$AQI) {
    sst <- sst + (i - avg_y)^2
}
cat("SST = ", sst, "\n")

cat("R^2 = ", ssr / sst, "\n")

performance1 <- data.frame(
    RMSE = RMSE(pred1, df_test$AQI),
    R2 = R2(pred1, df_test$AQI)
)
print(performance1)

avg_y <- mean(df_impute$AQI)
cat("avg_AQI = ", avg_y, "\n")

ssr <- 0
for (i in pred2) {
    ssr <- ssr + (i - avg_y)^2
}
cat("SSR = ", ssr, "\n")

sst <- 0
for (i in df_impute$AQI) {
    sst <- sst + (i - avg_y)^2
}
cat("SST = ", sst, "\n")

cat("R^2 = ", ssr / sst, "\n")

performance <- data.frame(
    RMSE = RMSE(pred2, df_test$AQI),
    R2 = R2(pred2, df_test$AQI)
)
print(performance)