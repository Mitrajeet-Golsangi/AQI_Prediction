setwd("D:/VIT Projects/Second Year/1st Semester/DS CP/")
shell("cls")

library(mice)
library(caTools)
library(randomForest)
library(ggplot2)

# ------------------------------- Reading data ------------------------------- #

df <- read.csv("./Air Pollution/city_day.csv")

# ------------------- Making the NA values 0 in AQI column ------------------- #

df_AQI <- df$AQI
df_AQI[is.na(df_AQI)] <- 0

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

split_ratio <- 0.8 # take in increaments of 0.2
split <- sample.split(df_final, SplitRatio = split_ratio)

df_impute_train <- subset(df_final, split == TRUE)
df_impute_test <- subset(df_final, split == FALSE)

# ---------------------------- Creating RF Model ----------------------------- #

rf_na_impute <- randomForest(
    df_AQI~.,
    data = df_impute_test,
    mtry = sqrt(12),
    ntree = 500
)

# ----------------------------- Predicting values ---------------------------- #

pred1 <- predict(rf_na_impute, df_impute_test, type = "response")
df_impute_test$Prediction <- pred1


# ----------------------- Checking performance of model ---------------------- #

cat("Random Forest with Imputation : \n")

print(rf_na_impute)

# ----------------------------- Plotting the Graph --------------------------- #

# Actual vs Prediction
plt <- ggplot(df_impute_test, aes(PM2.5, df_AQI)) +
        geom_line(aes(color = "Actual")) +
        geom_line(
          aes(
            PM2.5,
            Prediction,
            color = "Prediction"
          )
        ) +
        scale_color_manual(
            values = c(
                "Actual" = "red",
                "Prediction" = "blue"
            )
        ) +
        labs(
            title = "RF WITHOUT IMPUTATION : ACTUAL VS PREDICTION",
        )
View(plt)