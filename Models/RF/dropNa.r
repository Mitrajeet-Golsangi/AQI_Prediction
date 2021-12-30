setwd("D:/VIT Projects/Second Year/1st Semester/DS CP/")
shell("cls")

# ---------------------------- Requried Libraries ---------------------------- #

library(caTools)
library(randomForest)
library(mice)
library(ggplot2)
# library(party)

# ------------------------------- Reading data ------------------------------- #

df <- read.csv("./Air Pollution/city_day.csv");
df <- df[, -c(1, 2)]
df$AQI_Bucket <- as.factor(df$AQI_Bucket)

# ---------------------------- Remove rows with NA --------------------------- #

df_cleaned <- na.omit(df)
row.names(df_cleaned) <- NULL

# --------------------- Split into training and testing ---------------------- #

set.seed(123)

split_ratio <- 0.8 # take in increaments of 0.2
split <- sample.split(df_cleaned, SplitRatio = split_ratio)

df_train <- subset(df_cleaned, split == TRUE) # Complete dataframe is taken
df_test <- subset(df_cleaned, split == FALSE)

# ---------------------------- Creating RF Model ----------------------------- #

rf_na_omit <- randomForest(
    AQI~.,
    data = df_train,
    mtry = sqrt(12),
    ntree = 500,
    na.action = na.omit
)

# ----------------------------- Predicting values ---------------------------- #

pred1 <- predict(rf_na_omit, df_test, type = "response")
df_test$Prediction <- pred1

# ----------------------- Checking performance of model ---------------------- #


cat("Random Forest without Imputation : \n")


print(rf_na_omit)

# ----------------------------- Plotting the Graph --------------------------- #

# Actual vs Prediction
plt <- ggplot(df_test, aes(PM2.5, AQI)) +
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

# tr <- ctree(AQI~., data = df_train)
# print(plot(tr))