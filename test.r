setwd("D:/VIT Projects/Second Year/1st Semester/DS CP/")
shell("cls")

df_impute <- read.csv("./Air Pollution/city_day.csv")


df_factor <- data.frame(
    as.factor(df_impute$PM2.5),
    as.factor(df_impute$PM10),
    as.factor(df_impute$NO),
    as.factor(df_impute$NO2),
    as.factor(df_impute$NOx),
    as.factor(df_impute$NH3),
    as.factor(df_impute$CO),
    as.factor(df_impute$SO2),
    as.factor(df_impute$O3),
    as.factor(df_impute$Benzene),
    as.factor(df_impute$Toluene),
    as.factor(df_impute$Xylene)
)