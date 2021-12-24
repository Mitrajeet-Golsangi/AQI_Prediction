setwd("D:/VIT Projects/Second Year/1st Semester/DS CP/")
shell("cls")

library(mice)
library(caTools)

# df <- read.csv("./Air Pollution/city_day.csv")
# df_AQI <- df$AQI
# df_AQI[is.na(df_AQI)] <- 0

# df <- df[, -c(1, 2, 15, 16)]
# View(df)

# methods <- c(
#     "pmm",
#     "pmm",
#     "pmm",
#     "pmm",
#     "pmm",
#     "pmm",
#     "pmm",
#     "pmm",
#     "pmm",
#     "pmm",
#     "pmm",
#     "pmm"
# )

# df_impute <- mice(data = df, m = 5, method = methods, maxit = 20)

# df_final <- complete(df_impute, 5)
# head(df_impute)

# df_final <- cbind(df_final, df_AQI)

set.seed(123)

splitRatio = 0.8 # take in increaments of 0.2
split <- sample.split(df_final, SplitRatio = splitRatio)

df_train <- subset(df_final, split == TRUE) # Complete dataframe is taken
df_test <- subset(df_final, split == FALSE)

lm1 <- lm(df_AQI ~ ., data = df_train)
print(summary(lm1))

pred1 <- predict.lm(lm1, df_test, type = "response")
print(head(pred1))

avg_y <- mean(df_final$df_AQI)
cat("avg_AQI = ", avg_y, "\n")

ssr <- 0
for (i in pred1) {
    ssr <- ssr + (i - avg_y)^2
}
cat("SSR = ", ssr, "\n")

sst <- 0
for (i in df_final$df_AQI) {
    sst <- sst + (i - avg_y)^2
}
cat("SST = ", sst, "\n")

cat("R^2 = ", ssr / sst, "\n")

performance1 <- data.frame(
    RMSE = RMSE(pred1, df_test$df_AQI),
    R2 = R2(pred1, df_test$df_AQI)
)
print(performance1)

avg_y <- mean(df_final$df_AQI)
cat("avg_AQI = ", avg_y, "\n")

ssr <- 0
for (i in pred2) {
    ssr <- ssr + (i - avg_y)^2
}
cat("SSR = ", ssr, "\n")

sst <- 0
for (i in df_final$df_AQI) {
    sst <- sst + (i - avg_y)^2
}
cat("SST = ", sst, "\n")

cat("R^2 = ", ssr / sst, "\n")

performance <- data.frame(
    RMSE = RMSE(pred2, df_test$df_AQI),
    R2 = R2(pred2, df_test$df_AQI)
)
print(performance)