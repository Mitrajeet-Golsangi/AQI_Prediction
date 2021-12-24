setwd("D:/VIT Projects/Second Year/1st Semester/DS CP/")
shell("cls")

# ---------------------------- Requried Libraries ---------------------------- #

library(caTools)
library(randomForest)
library(mice)

# ------------------------------- Reading data ------------------------------- #

df <- read.csv("./Air Pollution/city_day.csv");
df <- df[, -c(1, 2)]

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

# ----------------------- Checking performance of model ---------------------- #

print(rf_na_omit)
