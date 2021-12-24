setwd("D:/VIT Projects/Second Year/1st Semester/DS CP/")
shell("cls")

# ---------------------------- Requried Libraries ---------------------------- #

library(caTools)

# ------------------------------- Reading data ------------------------------- #

df <- read.csv("./Air Pollution/city_day.csv");

# -------------------- Making Na value in AQI column zero -------------------- #

df$AQI[is.na(df$AQI)] <- 0
df <- df[, -c(1, 2, 16)]

# ---------------------------- Remove rows with NA --------------------------- #

df_cleaned <- na.omit(df)
row.names(df_cleaned) <- NULL

# ---------------------- Split into Training and testing --------------------- #

set.seed(123)
split_ratio <- 0.8
split <- sample.split(df_cleaned, SplitRatio = split_ratio)

df_train <- subset(df_cleaned, split == TRUE)
df_test <- subset(df_cleaned, split == FALSE)

# ---------------------------- Creating MLR Model ---------------------------- #

lm1 <- lm(AQI~., data = df_train)
print(summary(lm1))

# ----------------------------- Predicting values ---------------------------- #

pred1 <- predict.lm(lm1, df_test, type = "response")

# ----------------------- Checking performance of model ---------------------- #

performance1 <- data.frame(
  RMSE = RMSE(pred1, df_test$AQI),
  R2 = R2(pred1, df_test$AQI)
)
print(performance1)

avg_y <- mean(df_cleaned$AQI)
cat("avg_AQI = ", avg_y, "\n")
