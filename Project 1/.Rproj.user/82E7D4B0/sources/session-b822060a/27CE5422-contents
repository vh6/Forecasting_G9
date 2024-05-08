# Load packages
library(tidyverse)
library(lubridate)
library(fpp3)
library(patchwork)

# Load data
df <- read.csv("Dataset_tourism.csv")

# Translate Monat column to english
german_months <- c("Januar", "Februar", "M\xe4rz", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember")
english_months <- month.name

# Replace German month names with English month names
df$Monat <- factor(df$Monat, levels = german_months, labels = english_months)

# Create date column
df$date <- ymd(paste(df$Jahr, df$Monat, "01", sep = "-"))

# Count NAs
sum(is.na(df$Jahr)) # 0 NAs
sum(is.na(df$Monat)) # 0 NAs
sum(is.na(df$date)) # 0 NAs
sum(is.na(df$value)) # 51395 NAs
sum(is.na(df$Kanton)) # 0 NAs
sum(is.na(df$Herkunftsland)) # 0 NAs

# EDA #################################################

# tsibble plot: there are no extreme values except for the drop during covid.

# STL: see stl decompositions below on df_nocovid

# ACF plots: see below on df_nocovid

#########################################################

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

# STL decomposition (EDA)
df_nocovid$date <- yearmonth(df_nocovid$date)
df_nocovid <- tsibble(df_nocovid, index = date)
df_stl <- df_nocovid %>% model(STL(value))
components(df_stl) %>% autoplot()

# ACF plot
df_nocovid <- df_nocovid %>% mutate(diff_value = difference(value))
df_nocovid %>% autoplot(diff_value)

p1 <- df_nocovid %>% ACF(value) %>% autoplot()
p2 <- df_nocovid %>% ACF(diff_value) %>% autoplot()
p1 + p2

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

# STL decomposition (EDA)
df_nocovid$date <- yearmonth(df_nocovid$date)
df_nocovid <- tsibble(df_nocovid, index = date)
df_stl <- df_nocovid %>% model(STL(value))
components(df_stl) %>% autoplot()

# ACF plot
df_nocovid <- df_nocovid %>% mutate(diff_value = difference(value))
df_nocovid %>% autoplot(diff_value)

p1 <- df_nocovid %>% ACF(value) %>% autoplot()
p2 <- df_nocovid %>% ACF(diff_value) %>% autoplot()
p1 + p2

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