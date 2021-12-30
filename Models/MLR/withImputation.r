setwd("D:/VIT Projects/Second Year/1st Semester/DS CP/")
shell("cls")

# ---------------------------- Requried Libraries ---------------------------- #

library(mice)
library(caTools)
library(ggplot2)
library(ggfortify)

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

splitRatio = 0.8 # take in increaments of 0.2
split <- sample.split(df_final, SplitRatio = splitRatio)

df_train <- subset(df_final, split == TRUE) # Complete dataframe is taken
df_test <- subset(df_final, split == FALSE)

# ---------------------------- Creating MLR Model ---------------------------- #

lm1 <- lm(df_AQI ~ ., data = df_train)
print(summary(lm1))


cat("MLR with Imputation : \n")


# ----------------------------- Predicting values ---------------------------- #

pred1 <- predict.lm(lm1, df_test, type = "response")
print(head(pred1))

df_test$Prediction <- pred1

# # ----------------------- Calculating RMSE of the model --------------------- #

res <- summary(lm1)
rss <- c(crossprod(res$residuals))
mse <- rss / length(res$residuals)

rmse <- sqrt(mse)
cat("RMSE : ", rmse, "\n")

# ----------------------------- Plotting the Graph --------------------------- #

# Actual vs Prediction
plt <- ggplot(df_test, aes(PM2.5, df_AQI)) +
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
            title = "MLR WITH IMPUTATION : ACTUAL VS PREDICTION",
        )

View(plt)

# plt3 <- autoplot(lm1, which = 1:6, ncol = 2, label.size = 3,
#          colour = "lightgreen") + theme_bw()

# print(plt3)