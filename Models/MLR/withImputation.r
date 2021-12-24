setwd("D:/VIT Projects/Second Year/1st Semester/DS CP/")
shell("cls")

# ---------------------------- Requried Libraries ---------------------------- #

library(mice)
library(caTools)

# ------------------------------- Reading data ------------------------------- #

df <- read.csv("./Air Pollution/city_day.csv")

# ------------------- Making the NA values 0 in AQI column ------------------- #

df_aqi <- df$AQI
df_aqi[is.na(df_aqi)] <- 0

# -------------------- Drop unused columns from dataframe -------------------- #

df <- df[, -c(1, 2, 15, 16)]

# -------------------- vector holding imputation methods --------------------- #

methods <- c(
    "pmm",
    "pmm",
    "pmm",
    "pmm",
    "pmm",
    "pmm",
    "pmm",
    "pmm",
    "pmm",
    "pmm",
    "pmm",
    "pmm"
)

# ------------------------ Imputing with mice library ------------------------ #

df_impute <- mice(data = df, m = 5, method = methods, maxit = 20)

# ---------------- Getting the imputed dataframe from options ---------------- #

df_final <- complete(df_impute, 5)

# ---------------- Adding the AQI column to imputed dataframe ---------------- #

df_final <- cbind(df_final, df_AQI)

# --------------------- Split into training and testing ---------------------- #

set.seed(123)

splitRatio = 0.8 # take in increaments of 0.2
split <- sample.split(df_final, SplitRatio = splitRatio)

df_train <- subset(df_final, split == TRUE) # Complete dataframe is taken
df_test <- subset(df_final, split == FALSE)

# ---------------------------- Creating MLR Model ---------------------------- #

lm1 <- lm(df_AQI ~ ., data = df_train)
print(summary(lm1))

# ----------------------------- Predicting values ---------------------------- #

pred1 <- predict.lm(lm1, df_test, type = "response")
print(head(pred1))

# ----------------------- Checking performance of model ---------------------- #

performance1 <- data.frame(
    RMSE = RMSE(pred1, df_test$df_AQI),
    R2 = R2(pred1, df_test$df_AQI)
)
print(performance1)
