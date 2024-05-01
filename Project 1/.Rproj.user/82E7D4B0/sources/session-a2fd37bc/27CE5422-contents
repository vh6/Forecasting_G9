# Load packages
library(tidyverse)
library(lubridate)
library(fpp3)

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

# Remove 2 years from data after covid
df_nocovid$date[df_nocovid$date >= ymd("2022-03-01")] <- df_nocovid$date[df_nocovid$date >= ymd("2022-03-01")] - years(2)

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

fc <- fit |> forecast(h = "15 months", level=90)
fc |> autoplot(df_nocovid) +
  ggtitle("Forecast of monthly visitors to Vaud") +
  ylab("Visitors") +
  xlab("Months")

# Add 2 years to fc
fc$date <- ymd(paste0(fc$date, "-01")) + years(2)

# Create blank rows in original df
df <- df[order(df$date), ]
blank_rows <- data.frame(matrix(NA, nrow = 15, ncol = ncol(df)))
colnames(blank_rows) <- colnames(df)
df <- as_tibble(df)
df <- rbind(df, blank_rows)

# Add dates and values from fc model to original df
df$date[(nrow(df) - 14):nrow(df)] <- fc$date
df$value[(nrow(df) - 14):nrow(df)] <- fc$.mean

# Create confidence intervals from distributions in fc
distributions <- fc$value
confidence_levels <- c(0.80, 0.95)

# Function to calculate CI bounds
calculate_ci_bounds <- function(distribution, confidence_levels) {
  lower_bounds <- sapply(confidence_levels, function(level) {
    qnorm((1 - level) / 2, mean = distribution$mean, sd = distribution$sd)
  })
  upper_bounds <- sapply(confidence_levels, function(level) {
    qnorm((1 + level) / 2, mean = distribution$mean, sd = distribution$sd)
  })
  return(list(lower_bounds = lower_bounds, upper_bounds = upper_bounds))
}

# Calculate confidence intervals
ci_bounds <- sapply(distributions, calculate_ci_bounds, confidence_levels)

# Add confidence interval column to original df
df <- df %>%
  mutate(ci = NA)

df$ci[(nrow(df) - 14):nrow(df)] <- fc$value

ci_list <- as.list(fc$value)
ci_list[1]


# Plot final df
df <- tsibble(df, index = date)
df |> autoplot(value) +
  ggtitle("Forecast of monthly visitors to Vaud") +
  ylab("Visitors") +
  xlab("Months")

