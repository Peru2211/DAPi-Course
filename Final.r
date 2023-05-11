
setwd("~/Downloads")
agriculture=read.csv("India Agriculture Crop Production.csv")
sum(is.na(agriculture))
table(agriculture$Season)
names(agriculture)
agriculture[agriculture==""]=NA
agriculture <- na.omit(agriculture) # remove rows with missing values
trainData <- na.omit(trainData) # remove rows with missing values
agriculture[agriculture=="0"]=NA
table(agriculture$Crop)
sum(is.na(agriculture$Production.Units))
agriculture <- agriculture[agriculture$Season != "Whole Year", ]
agriculture$Season <- factor(agriculture$Season, levels = c("Kharif", "Rabi", "Summer", "Autumn", "Winter"))
agriculture$Crop <- factor(agriculture$Crop, levels = c("Arecanut", "Arhar/Tur", "Bajra", "Banana", "Barley", "Black pepper", "Cardamom", "Cashewnut", "Castor seed", "Coriander", "Cotton(lint)", "Cowpea(Lobia)", "Dry chillies", "Dry Ginger", "Garlic", "Ginger", "Gram", "Groundnut", "Guar seed", "Horse-gram", "Jowar", "Jute", "Khesari", "Linseed", "Maize", "Masoor", "Mesta", "Moong(Green Gram)", "Moth", "Niger seed", "Oilseeds total", "Onion", "Other Cereals", "Other Kharif pulses", "other oilseeds", "Other Rabi pulses", "Other Summer Pulses", "Peas & beans (Pulses)", "Potato", "Ragi", "Rapeseed &Mustard", "Rice", "Safflower", "Sannhamp", "Sesamum", "Small millets", "Soyabean", "Sugarcane", "Sunflower", "Sweet potato", "Tapioca", "Tobacco", "Turmeric", "Urad", "Wheat"))

agriculture <- subset(agriculture, !grepl("Nuts", agriculture$Production.Units))

# Check if 'Crop' column exists in 'agriculture' dataset
if ("Crop" %in% colnames(agriculture)) {
  print("Crop column exists in the dataset")
} else {
  print("Crop column does not exist in the dataset")
}

# Check the spelling of the 'Crop' column
if ("Crop" %in% colnames(agriculture)) {
  print("The column name is spelled correctly")
} else {
  print("The column name is misspelled")
}

library(dplyr)
library(caret)

# Split data by season
Kharif_data <- filter(agriculture, Season == "Kharif")
summer_data <- filter(agriculture, Season == "Summer")
Rabi_data <- filter(agriculture, Season == "Rabi")
autumn_data <- filter(agriculture, Season == "Autumn")
winter_data <- filter(agriculture, Season == "Winter")

# Define a function to convert units to tonnes
convert_to_tonnes <- function(value, unit) {
  if (unit == "Tonnes") {
    return(value)
  } else if (unit == "Bales") {
    return(value * 0.218)

  } else {
    return(NA)
  }
}



# Apply the function to the Production column
agriculture$Production <- mapply(convert_to_tonnes, agriculture$Production, agriculture$Production.Units)

# Change the units to 'tonnes'
agriculture$Production.Units <- "tonnes"

# Create a vector of column names in the desired order
new_col_order <- c("State_Initials", names(agriculture)[1:3], "Season", "Crop","Year", "Area", "Area.Units", "Production", "Production.Units", "Yield")


agriculture$Year <- as.numeric(substr(agriculture$Year, 1, 4))

# Reorder the columns using the vector of column names
agriculture <- agriculture[, new_col_order]

# Remove the original "State" column
agriculture$State <- NULL
agriculture$Crop.1=NULL
table(agriculture$Season)
library(dplyr)


# Create a new dataframe with only the top 10 crops
top_10_crops <- aggregate(Production ~ Crop, data = agriculture, sum)
top_10_crops <- top_10_crops[order(-top_10_crops$Production),]
top_10_crops <- top_10_crops[1:10,]
top_10_crops_df <- agriculture %>% filter(Crop %in% top_10_crops$Crop)

# Group the data by crop and state, then calculate the total production for each combination
crop_state_production <- top_10_crops_df %>%
  group_by(Crop, State_Initials) %>%
  summarise(total_production = sum(Production)) %>%
  arrange(desc(total_production))

# View the top 5 states for each crop
top_5_states <- crop_state_production %>%
  group_by(Crop) %>%
  top_n(5, total_production)
top_5_states

table(top_5_states$State_Initials)
sugarcane_df <- agriculture %>%
  filter(Crop == "Sugarcane") %>%
  group_by(State_Initials) %>%
  summarize(Total_Production = sum(Production)) %>%
  arrange(desc(Total_Production))

head(sugarcane_df, n = 10)

library(dplyr)
library(tidyr)

# Select only the numeric variables
numeric_vars <- c("Area", "Production", "Yield")
agriculture_numeric <- select(agriculture, all_of(numeric_vars))

# Calculate the correlation matrix
cor_matrix <- cor(agriculture_numeric)

# Display the correlation matrix
print(cor_matrix)


library(ggplot2)
library(dplyr)

# Create a new dataset with total production by State and District
state_district_prod <- agriculture %>%
  group_by(State, District) %>%
  summarise(Total_Production = sum(Production)) %>%
  ungroup()

# Create a bar plot of Total_Production by State
ggplot(state_district_prod, aes(x = State, y = Total_Production, fill = District)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Production by State and District",
       x = "State",
       y = "Total Production")
agriculture$Production <- as.numeric(agriculture$Production)



crop_production <- agriculture %>%
  group_by(Crop, Year) %>%
  summarise(Total_Production = sum(Production, na.rm = TRUE))

ggplot(crop_production, aes(x = Year, y = Total_Production, color = Crop)) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Crop production over time", x = "Year", y = "Total Production")
# Create a histogram of Yield
ggplot(agriculture, aes(x = Yield)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Distribution of Crop Yield",
       x = "Yield",
       y = "Count")

library(stringr)
crop_space_count <- str_count(agriculture$Crop, " ")

table(crop_space_count)

library(dplyr)
library(sf)
library(maptools)
library(sf)

# set your working directory to the downloads folder
setwd("~/Downloads")

# read the shapefile
india_map <- st_read(dsn = "IND_adm/IND_adm1.shp")

india_map <- st_read("IND_adm/IND_adm1.shp")


# Aggregate crop production at state level
state_crop_prod <- agriculture %>%
  group_by(State, Crop) %>%
  summarise(Total_Production = sum(Production, na.rm = TRUE)) %>%
  ungroup()
options(scipen = 999)
state_crop_prod$Total_Production <- round(state_crop_prod$Total_Production / 10^5, 3)

# Get the top crop produced in each state
top_crop <- state_crop_prod %>%
  group_by(State) %>%
  slice_max(Total_Production)

# Merge the top crop with the original dataset to get the region of each state
state_region_crop <- agriculture %>%
  select(State, District, Crop, Production) %>%
  left_join(top_crop, by = c("State", "Crop"))

# Get the total production of each crop at the state level
state_crop_prod <- state_region_crop %>%
  group_by(State, Crop) %>%
  summarise(Total_Production = sum(Production, na.rm = TRUE)) %>%
  ungroup()

# Get the top crop for each state
top_crop_state <- state_crop_prod %>%
  group_by(State) %>%
  slice_max(Total_Production)
top_crop_state$Total_Production=as.numeric(top_crop_state$Total_Production)



# Merge the top crop with the India map data to get the region of each state
state_map <- india_map %>%
  left_join(top_crop, by = c("NAME_1" = "State"))
# Plot the map
ggplot() +
  geom_sf(data = state_map, aes(fill = Total_Production), color = "grey") +
  scale_fill_gradient(low = "grey", high = "blue") +
  theme_void() +
  labs(title = "Crop production in each state")

# Create a dataset with the total production of each crop at the state level
state_crop_prod <- state_crop_prod %>%
  left_join(top_crop_state, by = c("State", "Crop"))


# join crop production data to the map
india_map <- left_join(india_map, state_crop_prod, by = c("NAME_1" = "State"))

# plot the map
plot(india_map["Total_Production"])


duplicated(state_crop_prod$State)
state_crop_prod[duplicated(state_crop_prod$State), ]

table(agriculture$Production.Units)


library(ggplot2)

# Aggregate the data by state to get the total crop production
state_crop_prod <- aggregate(agriculture$Production, by=list(State=agriculture$State), sum)

# Create a mapping of state names to their initials
state_initials <- c(
  "Andaman and Nicobar Islands" = "AN",
  "Chandigarh"= "CH",
  "Dadra and Nagar Haveli" = "D&N",
  "Daman and Diu"= "D&D",
  "Delhi"="DH",
  "Ladakh"="LA",
  "Puducherry"="PC",
  "Andhra Pradesh" = "AP",
  "Arunachal Pradesh" = "AR",
  "Assam" = "AS",
  "Bihar" = "BR",
  "Chhattisgarh" = "CG",
  "Goa" = "GA",
  "Gujarat" = "GJ",
  "Haryana" = "HR",
  "Himachal Pradesh" = "HP",
  "Jammu and Kashmir" = "JK",
  "Jharkhand" = "JH",
  "Karnataka" = "KA",
  "Kerala" = "KL",
  "Madhya Pradesh" = "MP",
  "Maharashtra" = "MH",
  "Manipur" = "MN",
  "Meghalaya" = "ML",
  "Mizoram" = "MZ",
  "Nagaland" = "NL",
  "Odisha" = "OR",
  "Punjab" = "PB",
  "Rajasthan" = "RJ",
  "Sikkim" = "SK",
  "Tamil Nadu" = "TN",
  "Telangana" = "TG",
  "Tripura" = "TR",
  "Uttar Pradesh" = "UP",
  "Uttarakhand" = "UK",
  "West Bengal" = "WB"
)

# Add state initials to the data frame
state_crop_prod$State_Initials <- state_initials[state_crop_prod$State]
agriculture$State_Initials= state_initials[agriculture$State]

# Create the plot
ggplot(state_crop_prod, aes(x = State_Initials, y = x)) +
  geom_bar(stat = "identity") +
  ggtitle("Total Crop Production by State") +
  xlab("State") +
  ylab("Total Production (in kg)")

library(ggplot2)

# Calculate total production by season
season_production <- aggregate(agriculture$Production, 
                               by=list(Season=agriculture$Production), 
                               FUN=sum)

# Plot the bar plot
ggplot(data=season_production, aes(x=Season, y=x)) + 
  geom_bar(stat="identity", fill="steelblue") +
  labs(title="Total Crop Production by Season", 
       x="Season", y="Production")

library('dplyr')
library('caret')
library('gbm')
install.packages("doSNOW")
library('doSNOW')

library(caret)
library(randomForest)




##### Model Tuning #####
names(agriculture)
agriculture.df <- select(agriculture,
                         Crop,
                         Year,
                         Area,
                         Production,
                         Yield)
outcomeName='Crop'
predictorNames='Season'

# Split the data into training and testing sets
set.seed(12345)
trainIndex <- createDataPartition(agriculture$Crop, p = 0.8, list = FALSE)
trainData <- agriculture[trainIndex,]
testData <- agriculture[-trainIndex,]
unique(trainData$Crop)
unique(testData$Crop)

##### MODEL 1 : RF Model##### 
# Control parameters
fitControl.1 <- trainControl(method = "none")
# Model
rf.1<-train(trainData[,predictorNames],trainData[,outcomeName],
            method='rf',
            trControl=fitControl.1)
# Variables of importance
rfImp.1<-varImp(rf.1)
rfImp.1
plot(rfImp.1)
# Performance measures
rf.1.predict<-predict(rf.1,testData[,predictorNames],type="raw")
confusionMatrix(rf.1.predict,testData[,outcomeName], positive = "Banana")




# Define the search grid for hyperparameter tuning
tuneGrid <- expand.grid(mtry = c(2, 3, 4))

# Train the random forest model with cross-validation and hyperparameter tuning
model <- train(Yield ~ State + District + Crop + Season + Year + Area, data = trainData,
               method = "rf",
               trControl = trainControl(method = "cv", number = 5),
               tuneGrid = tuneGrid,
               ntree = 500)

# Make predictions on the test data
predictions <- predict(model, testData)

# Evaluate the model
accuracy <- mean(predictions == testData$Yield)


table(agriculture$Year)


# Load libraries
library(tidyverse)
library(lubridate)
library(forecast)



# Filter data for top 10 crops
top10_crops <- agriculture %>%
  group_by(Crop) %>%
  summarise(total_production = sum(Production)) %>%
  arrange(desc(total_production)) %>%
  head(10) %>%
  pull(Crop)

agriculture_top10 <- agriculture %>%
  filter(Crop %in% top10_crops)

# Convert Year column to a date
agriculture_top10$Year <- as.Date(paste0(agriculture_top10$Year, "-01-01"))

# Create time series object
ts_agriculture_top10 <- ts(agriculture_top10$Production,
                           start = c(year(min(agriculture_top10$Year)), 1),
                           frequency = 12)

# Plot the time series
autoplot(ts_agriculture_top10)

# Check for stationarity
ggtsdisplay(ts_agriculture_top10)

# Check for seasonality
ggseasonplot(ts_agriculture_top10)

# Differencing to achieve stationarity
diff_ts_agriculture_top10 <- diff(ts_agriculture_top10, differences = 1)

# Check for stationarity again
ggtsdisplay(diff_ts_agriculture_top10)

# Check for seasonality again
ggseasonplot(diff_ts_agriculture_top10)

# Fit a forecasting model (ARIMA)
model <- auto.arima(diff_ts_agriculture_top10)

# Forecast for the next 3 years
forecast <- forecast(model, h = 36)

# Plot the forecast
autoplot(forecast)




library(dplyr)
#Plot for sugarcane#######
# Subset the data for sugarcane production only
sugarcane_data <- filter(agriculture,Crop =='Sugarcane')

# Create a time series object for sugarcane production
sugarcane_ts <- ts(sugarcane_data$Production, start = c(min(sugarcane_data$Year), 1), frequency = 1)


# Create a forecast for the next 10 years using the auto.arima function
library(forecast)
sugarcane_model <- auto.arima(sugarcane_ts)
sugarcane_fcst <- forecast(sugarcane_model, h = 10)

# Create a data frame of the forecasted values
sugarcane_fcst_df <- data.frame(Year = seq(2022, 2031), Production = sugarcane_fcst$mean)

library(ggplot2)

ggplot(sugarcane_fcst_df, aes(x = Year, y = Production)) + 
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  labs(title = "Forecast Projection for Sugarcane Production (2021-2031)", x = "Year", y = "Production") +
  scale_y_continuous(limits = c(0, max(sugarcane_fcst_df$Production) * 1.2), 
                     labels = function(x) format(x, scientific = FALSE))
library(forecast)

# Load the required packages
library(forecast)
library(tidyverse)



# Create a time series object for sugarcane production
sugarcane_ts <- ts(sugarcane_data$Production, start = c(min(sugarcane_data$Year), 1), frequency = 1)

library(forecast)
install.packages("MASS")
library(stats)
library(MASS)
library(Metrics)

library(zoo)





library(forecast)
library(Metrics)



# Set the frequency of the time series
sugarcane_ts <- ts(sugarcane_data$Production, start=c(1997,1), frequency=1)

# Split the data into training and testing sets
sugarcane_train <- window(sugarcane_ts, end=c(2020,12))

# Train the ARIMA model using the training set
model <- auto.arima(sugarcane_train)

# Use rolling origin forecast to evaluate the accuracy of the model
frc <- list()
for(i in 1:length(sugarcane_train)){
  if(i == 1){
    frc[[i]] <- forecast(model, h = 1)
  }else{
    model_temp <- Arima(window(sugarcane_train, end=c(1997+i-2,12)), model = model)
    frc[[i]] <- forecast(model_temp, h = 1)
  }
}

# Compute the MAPE for the rolling origin forecast
frc <- unlist(frc)
actual <- sugarcane_train
cv_mape <- mean(abs(as.numeric(frc) - as.numeric(actual))/as.numeric(actual)) * 100
cat("MAPE:", cv_mape, "%\n")



#forecast for which state sugarcane is produced the most########

library(forecast)

# Subset data for sugarcane only
sugarcane_data <- subset(agriculture, Crop == "Sugarcane")

# Convert year column to date format
sugarcane_data$Year <- as.Date(paste(sugarcane_data$Year, "-01-01", sep=""))

# Split data into training and testing sets
sugarcane_train <- subset(sugarcane_data, Year < as.Date("2016-01-01"))
sugarcane_test <- subset(sugarcane_data, Year >= as.Date("2016-01-01"))

# Create a time series object for sugarcane production
sugarcane_ts <- ts(sugarcane_train$Production, start = c(1972, 1), end = c(2015, 1), frequency = 1)

# Fit an ARIMA model to the sugarcane time series
sugarcane_model <- auto.arima(sugarcane_ts)

# Forecast sugarcane production for the next 10 years
sugarcane_forecast <- forecast(sugarcane_model, h = 10)

# Plot the forecast for sugarcane production
plot(sugarcane_forecast, xlab = "Year", ylab = "Production", main = "Forecast for Sugarcane Production")

# Identify the state with the highest average production for sugarcane
sugarcane_state_avg <- aggregate(sugarcane_train$Production, by = list(State = sugarcane_train$State_Initials), FUN = mean)
sugarcane_highest_state <- sugarcane_state_avg[which.max(sugarcane_state_avg$x), "State"]

# Print the state with the highest average production for sugarcane
cat(paste("The state with the highest average production for sugarcane is", sugarcane_highest_state, "\n"))

# Forecast sugarcane production for each state individually
sugarcane_forecasts <- vector("list", length(unique(sugarcane_train$State_Initials)))
names(sugarcane_forecasts) <- unique(sugarcane_train$State_Initials)

for (state in unique(sugarcane_train$State_Initials)) {
  state_data <- subset(sugarcane_train, State_Initials == state)
  state_ts <- ts(state_data$Production, start = c(1972, 1), end = c(2015, 1), frequency = 1)
  state_model <- auto.arima(state_ts)
  state_forecast <- forecast(state_model, h = 10)
  sugarcane_forecasts[[state]] <- state_forecast
}

# Identify the state with the highest forecasted production for sugarcane in the next 10 years
sugarcane_state_forecasts <- vector("list", length(unique(sugarcane_train$State_Initials)))
names(sugarcane_state_forecasts) <- unique(sugarcane_train$State_Initials)

for (state in unique(sugarcane_train$State_Initials)) {
  state_forecast <- sugarcane_forecasts[[state]]
  state_forecast_df <- as.data.frame(state_forecast)
  state_forecast_df$State <- state
  sugarcane_state_forecasts[[state]] <- state_forecast_df
}

# Create a data frame with the forecasts for each state
sugarcane_forecast_df <- do.call(rbind, sugarcane_state_forecasts)

# Aggregate the forecasts by state and sum the forecasted production for each state
sugarcane_forecast_sum <- aggregate(sugarcane_forecast_df$Point.Forecast, by = list(State = sugarcane_forecast_df$State), FUN = sum)



# Calculate the average production for each crop and year
crop_year_production <- aggregate(Production ~ Crop + Year, data = agriculture, FUN = mean)

# Calculate the production trend for each crop over the years
library(dplyr)
crop_production_trend <- crop_year_production %>% 
  group_by(Crop) %>% 
  mutate(trend = predict(lm(Production ~ Year), data.frame(Year = Year))) %>% 
  ungroup() %>% 
  mutate(trend_diff = c(0, diff(trend))) %>% 
  select(Crop, Year, Production, trend, trend_diff)

# Find the crops whose production is decreasing year by year
decreasing_crops <- crop_production_trend %>% 
  group_by(Crop) %>% 
  filter(all(trend_diff < 0)) %>% 
  distinct(Crop)

# Load required libraries
library(forecast)

# Load the required libraries
library(forecast)
library(tidyverse)
library(caret)

library(forecast)
library(doParallel)

# Set up a cluster
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Subset the data for turmeric crop
turmeric <- filter(agriculture, Crop == "Turmeric")
turmeric$Production <- turmeric$Production * 10

turmeric$Year <- as.Date(paste(turmeric$Year, "-01-01", sep=""))

# Split data into training and testing sets
turmeric_train <- subset(turmeric, Year < as.Date("2016-01-01"))
turmeric_test <- subset(turmeric, Year >= as.Date("2016-01-01"))

# Remove predictors with near-zero variance
nzv <- nearZeroVar(turmeric_train)
turmeric_train <- turmeric_train[, -nzv]
turmeric_test <- turmeric_test[, -nzv]

# Create a formula for the model
formula <- as.formula(paste("Production ~", paste(colnames(turmeric_train)[-1], collapse = " + ")))

# Define the tuning grid
tune_grid <- expand.grid(alpha = seq(0, 1, 0.1), lambda = seq(0, 1, 0.1))

# Define the cross-validation method
ctrl <- trainControl(method = "cv", number = 10)
library(glmnet)
# Tune the model
model_tuned <- train(formula, data = turmeric_train, method = "glmnet", tuneGrid = tune_grid, trControl = ctrl)

library(glmnet)

# Check for NAs after converting the data to a matrix
anyNA(as.matrix(turmeric_train[, -which(names(turmeric_train) == "Production")]))

# Build the final model using the selected features and the tuned hyperparameters
model_final <- glmnet(
  x = as.matrix(turmeric_train[, -which(names(turmeric_train) == "Production")]),
  y = turmeric_train$Production,
  lambda = model_tuned$bestTune$lambda,
  alpha = model_tuned$bestTune$alpha
)

frc <- foreach(i = 1:5, .combine = rbind) %dopar% {
  
  # Check the dimensions of the input matrix
  pred_mat <- as.matrix(t(turmeric_test[i, -1]))
  print(dim(pred_mat))
  
  # Check the dimensions of the model matrix
  model_mat <- model_final$x
  print(dim(model_mat))
  library(forecast)
  # Make the forecast
  forecast(model_final, newx = pred_mat)
  
}

# Stop the cluster
stopCluster(cl)

# Calculate the accuracy for each forecast
accuracy_list <- lapply(frc, accuracy, x = turmeric_test$Production)

# Combine the accuracy measures into a single data frame
accuracy_df <- do.call(rbind, accuracy_list)

# Print the forecast and accuracy measures
print(frc)
print(accuracy_df)

