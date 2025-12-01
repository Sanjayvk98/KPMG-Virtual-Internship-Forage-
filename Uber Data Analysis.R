library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(tidyverse) # metapackage of all tidyverse packages
library(DT)
library(scales)
colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")
colors
apr <- read.csv("C:/Users/sanj/OneDrive/Documents/R Documents/uber-raw-data-apr14.csv")
may <- read.csv("C:/Users/sanj/OneDrive/Documents/R Documents/uber-raw-data-may14.csv")
june <- read.csv("C:/Users/sanj/OneDrive/Documents/R Documents/uber-raw-data-jun14.csv")
july <- read.csv("C:/Users/sanj/OneDrive/Documents/R Documents/uber-raw-data-jul14.csv")
aug <- read.csv("C:/Users/sanj/OneDrive/Documents/R Documents/uber-raw-data-aug14.csv")
sept <- read.csv("C:/Users/sanj/OneDrive/Documents/R Documents/uber-raw-data-sep14.csv")

# Combine the data together 
data <- rbind(apr, may, june, july, aug, sept)
cat("The dimensions of the data are:", dim(data))
head(data)

data<-na.omit(data)
dim(data)
ts<-data
data$Date.Time <- as.POSIXct(data$Date.Time, format="%m/%d/%Y %H:%M:%S")
data$Time <- format(as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")
data$Date<-factor(date(data$Date.Time))
data$Date.Time <- ymd_hms(data$Date.Time)
# Create individual columns for month day and year
data$day <- factor(day(data$Date.Time))
data$month <- factor(month(data$Date.Time, label=TRUE))
data$year <- factor(year(data$Date.Time))
data$dayofweek <- factor(wday(data$Date.Time, label=TRUE))

data$hour = factor(hour(hms(data$Time)))
data$week = factor(chron::is.weekend(data$Date.Time))
data$week<-factor(ifelse(data$week == "TRUE", "Weekend", "Weekday"))


data<-data%>%drop_na()
head(data)
#df$day_type <- ifelse(df$week == "TRUE", "Weekend", "Weekday")
#data$week<-factor(ifelse(data$week == "TRUE", "Weekend", "Weekday"))
head(data)

dim(data)
hourly_data <- data %>% 
  group_by(hour) %>% 
  dplyr::summarize(Total = n())

# Shos data in a searchable js table
datatable(hourly_data)
ggplot(hourly_data, aes(hour, Total)) + 
  geom_bar(stat="identity", 
           fill="steelblue", 
           color="black") + 
  ggtitle("Trips Every Hour", subtitle = "aggregated today") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)
monthly_data <- data %>% 
  group_by(month) %>% 
  dplyr::summarize(Total = n())

ggplot(monthly_data, aes(month, Total)) + 
  geom_bar(stat="identity", 
           fill="aliceblue", 
           color="black") + 
  ggtitle("Trips Every Month", subtitle = "aggregated today") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)
daily_data <- data %>% 
  group_by(day) %>% 
  dplyr::summarize(Total = n())
ggplot(daily_data, aes(day, Total)) + 
  geom_bar(stat="identity", 
           fill="plum", 
           color="lightgreen") + 
  ggtitle("Trips Every Day", subtitle = "aggregated today") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)
daily_week_data <- data %>% 
  group_by(dayofweek) %>% 
  dplyr::summarize(Total = n())
ggplot(daily_week_data, aes(dayofweek, Total)) + 
  geom_bar(stat="identity", 
           fill="salmon", 
           color="wheat") + 
  ggtitle("Trips Every Week", subtitle = "aggregated today") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)
daily_weekend_data <- data %>% 
  group_by(week) %>% 
  dplyr::summarize(Total = n())
ggplot(daily_weekend_data, aes(week, Total)) + 
  geom_bar(stat="identity", 
           fill="lightcyan", 
           color="navy") + 
  ggtitle("Trips Every Week", subtitle = "aggregated today") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)

#------------------------------------------------------------------------------------
month_hour_data <- data %>% group_by(month, hour) %>%  dplyr::summarize(Total = n())

ggplot(month_hour_data, aes(hour, Total, fill=month)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Hour and Month") + 
  scale_y_continuous(labels = comma)
month_week_data <- data %>% group_by(month, dayofweek) %>%  dplyr::summarize(Total = n())

ggplot(month_week_data, aes(month, Total, fill=dayofweek)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Week and Month") + 
  scale_y_continuous(labels = comma)

day_week_data <- data %>% group_by(day, month) %>%  dplyr::summarize(Total = n())

ggplot(day_week_data, aes(day, Total, fill=month)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Day and Month") + 
  scale_y_continuous(labels = comma)

month_weekend_data <- data %>% group_by(month, week) %>%  dplyr::summarize(Total = n())

ggplot(month_weekend_data, aes(month, Total, fill=week)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by month and Weekdays/Weekends") + 
  scale_y_continuous(labels = comma)

dayweek_week_data <- data %>% group_by(dayofweek, week) %>%  dplyr::summarize(Total = n())

ggplot(dayweek_week_data, aes(dayofweek, Total, fill=week)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Trips by Day and Weekdays") + 
  scale_y_continuous(labels = comma)

#-------------------------------------------------------------------------------
# Collect data by day of the week and month

day_month_data <- data %>% group_by(dayofweek, month) %>% dplyr::summarize(Trips = n())
day_month_data
ggplot(day_month_data, aes(dayofweek, Trips, fill = month)) + 
  geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
  ggtitle("Trips by Day and Month") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

dayweek_month_data <- data %>% group_by(week, month) %>% dplyr::summarize(Trips = n())
dayweek_month_data
ggplot(dayweek_month_data, aes(week, Trips, fill = month)) + 
  geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
  ggtitle("Trip by Weekdays and Month") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)

days_month_data <- data %>% group_by(day, month) %>% dplyr::summarize(Trips = n())
days_month_data
ggplot(days_month_data, aes(day, Trips, fill = month)) + 
  geom_bar(stat = "identity", aes(fill = month), position = "dodge") + 
  ggtitle("Trip by Weekdays and Month") + 
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values = colors)
#--------------------------------------------------------------------------------
month_data <- data %>% group_by(month) %>% dplyr::summarize(Total = n())

month_data
day_hour_data <- data %>% group_by(day, hour) %>% dplyr::summarize(Total = n())
datatable(day_hour_data)
ggplot(day_hour_data, aes(day, hour, fill = Total)) + 
  geom_tile(color = "white") + 
  ggtitle("Heat Map by Hour and Day")

ggplot(day_month_data, aes(dayofweek, month, fill = Trips)) + 
  geom_tile(color = "black") + 
  ggtitle("Heat Map by Week and Month")

ggplot(month_hour_data, aes(hour, month, fill = Total)) + 
  geom_tile(color = "darkgrey") + 
  ggtitle("Heat Map by Hour and Month")

month_day_data <- data %>% group_by(month, day) %>% dplyr::summarize(Trips = n())
month_day_data
ggplot(month_day_data, aes(day, month, fill = Trips)) + 
  geom_tile(color = "white") + 
  ggtitle("Heat Map by Month and Day")

#---------------------------------------------------------------------------------------------
min_lat <- 40 
max_lat <- 40.91
min_long <- -74.15
max_long <- -73.7004

ggplot(apr, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")

mean_d<-mean(month_data$Total)
mean_d
    # Perform one-sample t-test
t_test_result <- t.test(month_data$Total, mu = 65000)  # mu is the population mean under the null hypothesis
  
  # Display results
print(t_test_result)  
#--------------------------------------------------------------------------------------------------------------------
observed_data <- matrix(dayweek_month_data$Trips, nrow = 4,ncol=3)

# Perform chi-square test
chi_square_result <- chisq.test(observed_data)

# Display results
print(chi_square_result)
#----------------------------------------------------------------------------------------------------------------------------
summary(data)

cat("End")

head(data)
str(data)
summary(data)

base_data <- data %>% 
  group_by(Base) %>% 
  dplyr::summarize(Total = n())

ggplot(base_data, aes(Base, Total)) + 
  geom_bar(stat="identity", 
           fill="aliceblue", 
           color="black") + 
  ggtitle("Trips Every Month", subtitle = "aggregated today") + 
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) + 
  scale_y_continuous(labels=comma)

daily_trips_b <- data %>%
  group_by(Base, Date) %>%
  summarise(Daily_Trips = n(), .groups = "drop")

# Average trips per Base
base_data <- daily_trips_b %>%
  group_by(Base) %>%
  summarise(Mean = mean(Daily_Trips))

# Plot
ggplot(base_data, aes(x = Base, y = Mean)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "grey") +
  ggtitle("Average Daily Trips per Base", subtitle = "Aggregated by Day") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(labels = comma)
# Count trips per base per weekday
base_weekday <- data %>%
  group_by(Base, week) %>%
  summarise(Trips = n(), .groups = "drop")

# Plot
ggplot(base_weekday, aes(x = week, y = Trips, fill = Base)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Trips per Base by Weekday",
    x = "Weekday",
    y = "Number of Trips"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(labels = comma)
# Count trips per base per month
base_month <- data %>%
  group_by(Base, month) %>%
  summarise(Trips = n(), .groups = "drop")

ggplot(base_month, aes(x = month, y = Trips, fill = Base)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Trips per Base by Days of Month",
    x = "Month",
    y = "Number of Trips"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(labels = comma)

# Count trips per base per calendar day
base_day <- data %>%
  group_by(Base, dayofweek) %>%
  summarise(Trips = n(), .groups = "drop")

ggplot(base_day, aes(x = dayofweek, y = Trips, color = Base)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_line(size = 1) +
  labs(
    title = "Trips per Base by Day of Week",
    x = "Day of the Month",
    y = "Number of Trips"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(labels = comma)

base_days <- data %>%
  group_by(Base, day) %>%
  summarise(Trips = n(), .groups = "drop")

ggplot(base_days, aes(x = day, y = Trips, color = Base)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_line(size = 1) +
  labs(
    title = "Trips per Base by Day of Calendar",
    x = "Day of the Month",
    y = "Number of Trips"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(labels = comma)
#-------------------------------------------------------------------------------
install.packages('forecast')
library('forecast')
#apr<- apr %>% select(-Lat, -Lon)
#may<- may %>% select(-Lat, -Lon)
#june<- june %>% select(-Lat, -Lon)
#july<- july %>% select(-Lat, -Lon)
#aug<- aug %>% select(-Lat, -Lon)
#sept<- sept %>% select(-Lat, -Lon)
#plot(apr)
library(dplyr)
head(data)

data<-data %>% select(-day,-month,-year,-dayofweek,-hour,-week)
head(data)

daily_avg <- data %>%
  group_by(Date = as_date(Date.Time)) %>%
  summarise(Average_Lat = mean(Lat, na.rm = TRUE),
            Average_Lon = mean(Lon, na.rm = TRUE),
            Trips = n(),
            .groups = "drop")
head(daily_avg)
daily_avg_data<-daily_avg %>% select(-Average_Lat, -Average_Lon)
head(daily_avg_data)
plot(daily_avg_data)
ts<-ts(daily_avg_data, frequency=6)
d<-decompose(ts, "additive")
plot(d)

tsm<-ts(daily_avg_data, frequency=6)
dm<-decompose(tsm, "multiplicative")
plot(dm)

# Sort by date to ensure order
daily_avg_data <- daily_avg_data %>% arrange(Date)

# Convert to time series
uber_ts <- ts(daily_avg_data$Trips, 
              frequency = 7,  # weekly seasonality
              start = c(2014, yday(min(daily_avg_data$Date))/7))
head(uber_ts)

library(tseries)     # for ADF test
library(forecast)    # for HoltWinters, ARIMA, SARIMA
library(prophet)     

#Checking whether the data is staionary or not using dickey fuller test
adf_result <- adf.test(uber_ts)
print(adf_result)

hw_model <- HoltWinters(uber_ts)
summary(hw_model)

# Forecast next 6 months (~180 days)
hw_forecast <- forecast(hw_model, h = 180)

autoplot(hw_forecast) +
  labs(title = "Holt-Winters Forecast - Next 6 Months", 
       x = "Months", y = "Trips")

#Arima Model
arima_model <- auto.arima(uber_ts)
summary(arima_model)

arima_forecast <- forecast(arima_model, h = 180)
autoplot(arima_forecast) +
  labs(title = "ARIMA Forecast -Next 6 Months",
       x = "Date", y = "Trips")

#Sarima model
sarima_model <- auto.arima(uber_ts, seasonal = TRUE)
summary(sarima_model)

sarima_forecast <- forecast(sarima_model, h = 180)
autoplot(sarima_forecast) +
  labs(title = "SARIMA Forecast - Next 6 Months",
       x = "Date", y = "Trips")

#ARIMAX Model
set.seed(42)
external_reg <- rnorm(length(uber_ts))  # Example: random variable

arimax_model <- auto.arima(uber_ts, xreg = external_reg)
summary(arimax_model)

future_reg <- rnorm(180)  # Future regressor values for forecast
arimax_forecast <- forecast(arimax_model, xreg = future_reg, h = 180)

autoplot(arimax_forecast) +
  labs(title = "ARIMAX Forecast - Next 6 Months",
       x = "Date", y = "Trips")

#Prophet model
# Prepare data for Prophet
prophet_data <- daily_avg_data %>%
  rename(ds = Date, y = Trips)

prophet_model <- prophet(prophet_data)

future <- make_future_dataframe(prophet_model, periods = 180)
prophet_forecast <- predict(prophet_model, future)

# Plot forecast
plot(prophet_model, prophet_forecast)
prophet_plot_components(prophet_model, prophet_forecast)

#Compare model's accuracy
train <- head(uber_ts, -30)
test  <- tail(uber_ts, 30)

models <- list(
  "HoltWinters" = forecast(HoltWinters(train), h = 30),
  "ARIMA" = forecast(auto.arima(train), h = 30),
  "SARIMA" = forecast(auto.arima(train, seasonal = TRUE), h = 30)
)

lapply(models, accuracy, x = test)

#Visualize forecast performance
fit <- auto.arima(train)
pred <- forecast(fit, h = length(test))
accuracy(pred, test)
autoplot(pred) +
  autolayer(test, series="Actual") +
  labs(title="Actual vs Forecast", x="Date", y="Trips")

#compare model's
models <- list(
  "HoltWinters" = forecast(HoltWinters(train), h = length(test)),
  "ARIMA" = forecast(auto.arima(train), h = length(test)),
  "SARIMA" = forecast(auto.arima(train, seasonal = TRUE), h = length(test))
)

lapply(models, accuracy, x = test)

autoplot(pred) +
  autolayer(test, series="Actual") +
  labs(title="Actual vs Forecast", x="Date", y="Trips")
tsCV(uber_ts, forecastfunction = auto.arima, h = 7)

#Final model
final_model <- auto.arima(uber_ts)
final_forecast <- forecast(final_model, h = 180)

autoplot(final_forecast) +
  labs(title="6-Month Uber Trips Forecast", x="Date", y="Trips")

checkresiduals(final_model)
write.csv(as.data.frame(final_forecast), "Uber_Forecast.csv", row.names = FALSE)
