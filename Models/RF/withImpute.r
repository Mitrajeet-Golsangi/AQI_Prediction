setwd("D:/VIT Projects/Second Year/1st Semester/DS CP/")
shell("cls")

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

split_ratio <- 0.8 # take in increaments of 0.2
split <- sample.split(df_cleaned, SplitRatio = split_ratio)

df_impute_train <- subset(df_cleaned, split == TRUE)
df_impute_test <- subset(df_cleaned, split == FALSE)

# ---------------------------- Creating RF Model ----------------------------- #

rf_na_impute <- randomForest(
    AQI~.,
    data = df_impute_test,
    mtry = sqrt(12),
    ntree = 500
)

# ----------------------- Checking performance of model ---------------------- #

print(rf_na_impute)
