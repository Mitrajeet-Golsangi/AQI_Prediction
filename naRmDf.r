setwd("D:/VIT Projects/Second Year/1st Semester/DS CP/")
shell("cls")

# ------------------------------- Reading the Dataframe ------------------------------- #
df <- read.csv('./Air Pollution/city_day.csv');

head(df)

# Making NA AQI Values zero

df$AQI[is.na(df$AQI)] <- 0
df <- df[, -c(1, 2, 16)]

print(head(df))

df_cleaned <- na.omit(df)
row.names(df_cleaned) = NULL
print(head(df_cleaned))
print(nrow(df_cleaned))

set.seed(123)

splitRatio = 0.8 # take in increaments of 0.2
split <- sample.split(df_cleaned, SplitRatio = splitRatio)

df_train <- subset(df_cleaned, split == TRUE) # Complete dataframe is taken
df_test <- subset(df_cleaned, split == FALSE)

lm1 <- lm(AQI~., data = df_train)
print(summary(lm1))

cat("Correlation between different vairables\n")
cat("NO, SO2\t\t:",     cor(df$NO, df$SO2),       "\n")
cat("NO, NH3\t\t:",     cor(df$NO, df$NH3),       "\n")
cat("NO, Benzene\t:",   cor(df$NO, df$Benzene),   "\n")
cat("SO2, NH3\t:",      cor(df$SO2, df$NH3),      "\n")
cat("SO2, Benzene\t:",  cor(df$SO2, df$Benzene),  "\n")
cat("NH3, Benzene\t:",  cor(df$NH3, df$Benzene),  "\n")

lm2 <- switch(
  toString(splitRatio),
  "0.8" = lm(AQI~PM2.5 + PM10 + NO2 + NOx + CO + O3 + Toluene + Xylene, df_train),
  "0.6" = lm(AQI~PM2.5 + PM10 + NO2 + NOx + NH3 + CO + O3 + Toluene + Xylene, df_train),
  "0.4" = lm(AQI~PM2.5 + PM10 + NO2 + NOx + NH3 + CO + O3 + Toluene + Xylene, df_train),
  "0.2" = lm(AQI~PM2.5 + PM10 + NO2 + NOx + NH3 + CO + O3, df_train),
)

print(summary(lm2))

pred1 <- predict.lm(lm1, df_test, type="response")
pred2 <- predict.lm(lm2, df_test, type="response")
print(head(pred1))
print(head(pred2))

avg_y <- mean(df_cleaned$AQI)
cat("avg_AQI = ", avg_y, "\n")

ssr <- 0
for (i in pred1){
  ssr = ssr + (i - avg_y)^2
}
cat("SSR = ", ssr, "\n")

sst <- 0
for (i in df_cleaned$AQI){
  sst = sst + (i - avg_y)^2
}
cat("SST = ", sst, "\n")

cat("R^2 = ", ssr/sst, "\n")

performance1 <- data.frame(
  RMSE = RMSE(pred1, df_test$AQI),
  R2 = R2(pred1, df_test$AQI)
)
print(performance1)

avg_y <- mean(df_cleaned$AQI)
cat("avg_AQI = ", avg_y, "\n")

ssr <- 0
for (i in pred2){
  ssr = ssr + (i - avg_y)^2
}
cat("SSR = ", ssr, "\n")

sst <- 0
for (i in df_cleaned$AQI){
  sst = sst + (i - avg_y)^2
}
cat("SST = ", sst, "\n")

cat("R^2 = ", ssr/sst, "\n")

performance <- data.frame(
  RMSE = RMSE(pred2, df_test$AQI),
  R2 = R2(pred2, df_test$AQI)
)
print(performance)