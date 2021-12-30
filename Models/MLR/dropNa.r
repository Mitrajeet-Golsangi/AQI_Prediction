setwd("D:/VIT Projects/Second Year/1st Semester/DS CP/")
shell("cls")

# ---------------------------- Requried Libraries ---------------------------- #

library(caTools)
library(ggplot2)
library(ggfortify)

# ------------------------------- Reading data ------------------------------- #

df <- read.csv("./Air Pollution/city_day.csv")
df_plt <- read.csv("./Air Pollution/city_day.csv")
# -------------------- Making Na value in AQI column zero -------------------- #

df$AQI[is.na(df$AQI)] <- 0
df <- df[, -c(1, 2, 16)]

df_plt$City <- as.factor(df_plt$City)
df_plt <- df_plt[, -c(16)]

# ---------------------------- Remove rows with NA --------------------------- #

df_cleaned <- na.omit(df)
df_plt <- na.omit(df_plt)
row.names(df_cleaned) <- NULL
row.names(df_plt) <- NULL

# ---------------------- Split into Training and testing --------------------- #

set.seed(123)
split_ratio <- 0.8
split <- sample.split(df_cleaned, SplitRatio = split_ratio)

df_train <- subset(df_cleaned, split == TRUE)
df_test <- subset(df_cleaned, split == FALSE)

# ---------------------------- Creating MLR Model ---------------------------- #

lm1 <- lm(AQI ~ ., data = df_train)
print(summary(lm1))

cat("MLR without Imputation : \n")


# ----------------------------- Predicting values ---------------------------- #

pred1 <- predict.lm(lm1, df_test, type = "response")
df_test$Prediction <- pred1

# # ----------------------- Calculating RMSE of the model --------------------- #

res <- summary(lm1)
rss <- c(crossprod(res$residuals))
mse <- rss / length(res$residuals)

rmse <- sqrt(mse)
cat("RMSE : ", rmse, "\n")

avg_y <- mean(df_cleaned$AQI)
cat("avg_AQI = ", avg_y, "\n")

# ----------------------------- Plotting the Graph --------------------------- #

# Actual vs Prediction
plt <- ggplot(df_test, aes(PM10, AQI)) +
        geom_line(aes(color = "Actual")) +
        geom_line(
          aes(
            PM10,
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
            title = "MLR WITHOUT IMPUTATION : ACTUAL VS PREDICTION",
        )

# City vs Average AQI Plot
df_plt1 <- aggregate(AQI~City, data = df_plt, mean, na.rm = TRUE)

plt1 <- ggplot(df_plt1, aes(City, AQI)) +
        geom_bar(
            stat = "identity",
            aes(fill=AQI)
        ) +
        geom_text(
            aes(label = round(AQI)),
            vjust = -1
        )

plt2 <- ggplot(df_plt, aes(City, AQI)) +
        geom_violin(
            scale = "area",
            aes(fill=City)
        )

df_plt2 <- df_test[, c("PM2.5", "AQI", "Prediction")]

plt3 <- autoplot(lm1, which = 1:6, ncol = 2, label.size = 3,
         colour = "steelblue") + theme_bw()

print(plt3)
View(plt)
View(plt1)
View(plt2)