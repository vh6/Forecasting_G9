# Load packages
library(tidyverse)
library(lubridate)
library(fpp3)
library(stats)
library(dplyr)
library(ggplot2)
library(fable)
library(tsibble)
library(fabletools)


# Load data
df <- read.csv("Dataset_tourism.csv", stringsAsFactors = FALSE)
# Translate Monat column to English
german_months <- c("Januar", "Februar", "M\xe4rz", "April", "Mai", "Juni", 
                   "Juli", "August", "September", "Oktober", "November", "Dezember")
english_months <- month.name

# Replace German month names with English month names
df$Monat <- factor(df$Monat, levels = german_months, labels = english_months)

# Check if any month translations have failed leading to NAs
if(any(is.na(df$Monat))) {
  warning("NA values found in month translation. Check 'Monat' levels and labels.")
}
df$date <- ymd(paste(df$Jahr, df$Monat, "01", sep = "-"))
if(any(is.na(df$date))) {
  warning("NA values found in 'date' creation. Check 'Jahr', 'Monat' concatenation.")
}

# Count NAs
sum(is.na(df$Jahr)) # 0 NAs
sum(is.na(df$Monat)) # 0 NAs
sum(is.na(df$date)) # 0 NAs
sum(is.na(df$value)) # 51395 NAs
sum(is.na(df$Kanton)) # 0 NAs
sum(is.na(df$Herkunftsland)) # 0 NAs

### 1. Predict total visitors between Oct 2023 and Dec 2024

# Subset dataframe to remove nationalities and cantons, and keep totals.
df <- df[df$Herkunftsland == "Herkunftsland - Total" & df$Kanton == "Vaud", ]

# Transform df to tsibble
df <- tsibble(df, index = date)

# Plot visitors
df |> autoplot(value) +
  ggtitle("Monthly visitors to Vaud") +
  ylab("Visitors") +
  xlab("Months")

# We see a upwards trend with seasonality, and a little bit of noise. We also see a strong dip in the amount of visitors during COVID, which will bias our model.
# We will add a dummy variable for all observations during covid, so we may remove them when creating a model in order to not bias our model.
# We consider COVID to be a black swan, and a unique event, and we will assume that it will not happen again during the period that we are predicting (Oct 23 - Dec 24).
# Lockdown started in March 2020, and all measures except masks were lifted indefinitely in February 2022.

# Add dummy variable covid, set to 1 between March 2020 and Feb 2022, 0 everywhere else.
df$covid <- ifelse(df$date >= ymd("2020-03-01") & df$date <= ymd("2022-02-01"), 1, 0)

# Create no covid tsibble
df_nocovid <- df
df_nocovid$value[df_nocovid$covid == 1] <- NA

df_nocovid |> autoplot(value) +
  ggtitle("Monthly visitors to Vaud") +
  ylab("Visitors") +
  xlab("Months")

# Since the COVID period is exactly 2 years, we can try to "stick" the before and after values together, so we have a time series without gaps, and this won't affect seasonality.

# Drop covid rows
df_nocovid <- df_nocovid[df_nocovid$covid != 1, ]

# Add 2 years from data before covid
df_nocovid$date[df_nocovid$date < ymd("2022-03-01")] <- df_nocovid$date[df_nocovid$date < ymd("2022-03-01")] + years(2)


df_nocovid <- tsibble(df_nocovid, index = date)

df_nocovid |> autoplot(value) +
  ggtitle("Monthly visitors to Vaud") +
  ylab("Visitors") +
  xlab("Months")

# Now we can run models on the data without gaps and reset the dates afterwards.

# Automatic ARIMA model
df_nocovid$date <- yearmonth(df_nocovid$date)
df_nocovid <- tsibble(df_nocovid, index = date)

fit <- df_nocovid |> model(ARIMA(value))
report(fit)

fc <- fit |> forecast(h = "15 months")
fc |> autoplot(df_nocovid) +
  ggtitle("Forecast of monthly visitors to Vaud") +
  ylab("Visitors") +
  xlab("Months")

# Plot model on original data
df$date <- yearmonth(df$date)

fc |> autoplot(df) +
  ggtitle("Forecast of monthly visitors to Vaud") +
  ylab("Visitors") +
  xlab("Months")

### 2. Repeat process for Filipino visitors in Ticino. (Reload data before point 1, then run lines below).

# Subset dataframe
df <- df[df$Herkunftsland == "Philippinen" & df$Kanton == "Ticino", ]

# Transform df to tsibble
df <- tsibble(df, index = date)

# Plot visitors
df |> autoplot(value) +
  ggtitle("Monthly Filipino visitors to Ticino") +
  ylab("Visitors") +
  xlab("Months")

# Add dummy variable covid, set to 1 between March 2020 and Feb 2022, 0 everywhere else.
df$covid <- ifelse(df$date >= ymd("2020-03-01") & df$date <= ymd("2022-02-01"), 1, 0)

# Create no covid tsibble
df_nocovid <- df
df_nocovid$value[df_nocovid$covid == 1] <- NA

df_nocovid |> autoplot(value) +
  ggtitle("Monthly Filipino visitors to Ticino") +
  ylab("Visitors") +
  xlab("Months")

# Since the COVID period is exactly 2 years, we can try to "stick" the before and after values together, so we have a time series without gaps, and this won't affect seasonality.

# Drop covid rows
df_nocovid <- df_nocovid[df_nocovid$covid != 1, ]

# Add 2 years from data before covid
df_nocovid$date[df_nocovid$date < ymd("2022-03-01")] <- df_nocovid$date[df_nocovid$date < ymd("2022-03-01")] + years(2)


df_nocovid <- tsibble(df_nocovid, index = date)

df_nocovid |> autoplot(value) +
  ggtitle("Monthly Filipino visitors to Ticino") +
  ylab("Visitors") +
  xlab("Months")

# Now we can run models on the data without gaps and reset the dates afterwards.

# Automatic ARIMA model
df_nocovid$date <- yearmonth(df_nocovid$date)
df_nocovid <- tsibble(df_nocovid, index = date)

fit <- df_nocovid |> model(ARIMA(value))
report(fit)

fc <- fit |> forecast(h = "15 months")
fc |> autoplot(df_nocovid) +
  ggtitle("Forecast of monthly Filipino visitors to Ticino") +
  ylab("Visitors") +
  xlab("Months")

# Plot model on original data
df$date <- yearmonth(df$date)

fc |> autoplot(df) +
  ggtitle("Forecast of monthly Filipino visitors to Ticino") +
  ylab("Visitors") +
  xlab("Months")

# Since the sample of visitors is much smaller for this subset of the data, we notice much more noise in the chart. 
# The automatic ARIMA model has taken this into account, and given us very large confidence intervals. It is unlikely that we will find a better model. Ideas?



#We are going to modify these 2 models adding also exogenous variables like , GDP per kanton , Temperature of the cities like Geneva and Lugano ,and also we will try to see if the exchange rate of swiss will make a difference in our forecasting , we will see if will get better or not .

#Firstly we will start will visitors of Vaud 
Payerne_weather <- read.csv("payerne.weather.csv")
GDP.Vaud <- read.csv("Gdp.Vaud.csv")
#Regulating the Date variable to be possible to merge the 3 dataset
#Including years with prediction in GDP.Vaud because the dataset had infomation from 2008-2021
Payerne_weather$Date <- as.Date(paste(Payerne_weather$Year, Payerne_weather$Month, "1", sep = "-"), "%Y-%m-%d")
GDP.Vaud$Date <- as.Date(paste(GDP.Vaud$Date, "01", "01", sep = "-"), format = "%Y-%m-%d")
known_years <- GDP.Vaud$Date
known_gdp <- GDP.Vaud$GDP.V
missing_years <- as.Date(c("2005-01-01", "2006-01-01", "2007-01-01", "2022-01-01", "2023-01-01"))
full_years <- seq(min(c(known_years, missing_years)), max(c(known_years, missing_years)), by = "year")
predicted_gdp <- spline(x = known_years, y = known_gdp, xout = full_years)
GDP.Vaud <- data.frame(
  Date = as.Date(paste(full_years, "-01-01", sep = "")), 
  GDP.V = predicted_gdp$y)
print(GDP.Vaud)

#Merging the datasets
str(df$Date)
str(Payerne_weather$Date)
str(GDP.Vaud$Date)
df_weather_merged <- df |> 
  left_join(Payerne_weather |>  select(Date, Temperature, Precipitation), by = "Date")
final_merged_data <- left_join(df_weather_merged, GDP.Vaud, by = "Date")
str(final_merged_data)
Canton.Vaud <- final_merged_data |> 
  group_by(Jahr) |> 
  mutate(
    GDP.Jan = GDP.V[Monat == "January"]
  ) |> 
  mutate(
    GDP.V = if_else(Monat != "January", NA_real_, GDP.V),
    GDP.V = if_else(is.na(GDP.V), GDP.Jan, GDP.V)
  ) |> 
  select(-GDP.Jan)
# Creating a scatterplot matrix to see the relationship btw exogenous variables and the visitors 
scatterplot_matrix <- ggplot(Canton.Vaud, aes(x = Temperature, y = Precipitation)) +
  geom_point(aes(color = GDP.V, size = value)) +  
  geom_smooth(method = "lm", se = FALSE) +  
  labs(x = "Temperature", y = "Precipitation", color = "GDP.V", size = "Visitors") +  
  ggtitle("Relationship between Temperature, Precipitation, GDP, and Visitors to Vaud") + 
  theme_minimal() 
  print(scatterplot_matrix)
#Fitting an automatic ARIMA model and plotting the forecast
  Canton.Vaud$Date <- yearmonth(Canton.Vaud$Date)
  Canton.Vaud_ts <- as_tsibble(Canton.Vaud, index = Date, key = Kanton) 
  fit <- Canton.Vaud_ts |> 
    model(ARIMA(value ~ GDP.V + Temperature + Precipitation))
  report(fit)
  # Forecast for 15 months ahead using the model
  future_exogenous <- new_data(Canton.Vaud_ts, n = 15) %>%
    mutate(
      GDP.V = mean(Canton.Vaud_ts$GDP.V, na.rm = TRUE),  # Using mean for future values as example
      Temperature = mean(Canton.Vaud_ts$Temperature, na.rm = TRUE),
      Precipitation = mean(Canton.Vaud_ts$Precipitation, na.rm = TRUE)
    )
  
  fc <- fit |> forecast(new_data = future_exogenous)
  autoplot(fc, Canton.Vaud_ts) +
    ggtitle("Forecast of Monthly Values for Canton Vaud with ARIMA Model") +
    ylab("Forecasted Value") +
    xlab("Date")
#Comparing these two models, the first ARIMA model (without exogenous variables and with drift) appears to be more efficient based on its superior statistical measures (AIC, BIC, and log likelihood). It seems to provide a better balance between fitting the historical data and avoiding overfitting, as indicated by its lower complexity (fewer parameters) and more efficient use of the data (lower AIC and BIC).

#We repeat the same process for Ticino (Philippine visitors) see how the model will be including expgenous variables 
  Lugano_weather <- read.csv("Lugano.weather.csv")
  GDP.Ticino <- read.csv("Gdp.Ticino.csv") 
  #Regulating the Date variable to be possible to merge the 3 dataset
  #Including years with prediction in GDP.Ticino because the dataset had information from 2008-2021
  Lugano_weather$Date <- as.Date(paste(Lugano_weather$Year, Lugano_weather$Month, "1", sep = "-"), "%Y-%m-%d")
  df <- df |> 
    mutate(Date = as.Date(date)) |> 
    select(-date)  
  GDP.Ticino$Date <- as.Date(paste(GDP.Ticino$Date, "01", "01", sep = "-"), format = "%Y-%m-%d")
  known_years <- GDP.Ticino$Date
  known_gdp <- GDP.Ticino$GDP.T
  missing_years <- as.Date(c("2005-01-01", "2006-01-01", "2007-01-01", "2022-01-01", "2023-01-01"))
  full_years <- seq(min(c(known_years, missing_years)), max(c(known_years, missing_years)), by = "year")
  predicted_gdp <- spline(x = known_years, y = known_gdp, xout = full_years)
  GDP.Ticino <- data.frame(
    Date = as.Date(paste(full_years, "-01-01", sep = "")),  
    GDP.T = predicted_gdp$y)
  print(GDP.Ticino)
  #Merging the datasets
  str(df$Date)
  str(Lugano_weather$Date)
  str(GDP.Ticino$Date)
  df_weather_merged <- df |> 
    left_join(Lugano_weather |>  select(Date, Temperature, Precipitation), by = "Date")
  final_merged_data <- left_join(df_weather_merged, GDP.Ticino, by = "Date")
  str(final_merged_data)
  
  Canton.Ticino <- final_merged_data |> 
    group_by(Jahr) |> 
    mutate(
      GDP.Jan = GDP.T[Monat == "January"]
    ) |> 
    mutate(
      GDP.T = if_else(Monat != "January", NA_real_, GDP.T),
    GDP.T = if_else(is.na(GDP.T), GDP.Jan, GDP.T)
    ) |> 
    select(-GDP.Jan)
  # Creating a scatterplot matrix to see the relationship btw variables 
  scatterplot_matrix <- ggplot(Canton.Ticino, aes(x = Temperature, y = Precipitation)) +
    geom_point(aes(color = GDP.T, size = value)) +  
    geom_smooth(method = "lm", se = FALSE) +  
    labs(x = "Temperature", y = "Precipitation", color = "GDP.T", size = "Philippinen") +  
    ggtitle("Relationship between Temperature, Precipitation, GDP, and Philipino visitors to Ticino") + 
    theme_minimal() 
  print(scatterplot_matrix)
  #Fitting an automatic ARIMA model and plotting the forecast
  Canton.Ticino$Date <- yearmonth(Canton.Ticino$Date)
  Canton.Ticino_ts <- as_tsibble(Canton.Ticino, index = Date, key = Kanton) 
  fit <- Canton.Ticino_ts |> 
    model(ARIMA(value ~ GDP.T + Temperature + Precipitation))
  report(fit)
  # Forecast for 15 months ahead using the model
  future_exogenous <- new_data(Canton.Ticino_ts, n = 15) %>%
    mutate(
      GDP.T = mean(Canton.Ticino_ts$GDP.T, na.rm = TRUE), 
      Temperature = mean(Canton.Ticino_ts$Temperature, na.rm = TRUE),
      Precipitation = mean(Canton.Ticino_ts$Precipitation, na.rm = TRUE)
    )
  
  fc <- fit |> forecast(new_data = future_exogenous)
  autoplot(fc, Canton.Ticino_ts) +
    ggtitle("Forecast of Monthly Values for Canton Ticino for Philipino visitors") +
    ylab("Forecasted Value") +
    xlab("Date")
#The exogenous variables have coefficients indicating a positive relationship with the dependent variable, particularly significant for Temperature. However, the AIC and BIC values are significantly higher than the ARIMA model, indicating a more complex model without necessarily providing a proportionate improvement in fit as per the increase in complexity.