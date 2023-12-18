library(corrplot)
library(plotly)
library(dplyr)
library(ggplot2)
library(caret)
library(caTools) 

# Read the CSV file
data <- read.csv("../RTA Dataset.csv")

# Display the structure of the dataset
str(data)

#Display summary of dataset
summary(data)

# Create a pie chart
fig <- plot_ly(data, labels = ~Sex_of_driver, values = ~Number_of_casualties, type = 'pie',
               marker = list(colors = rainbow(length(unique(data$Sex_of_driver)))))

# Customize the layout
fig <- fig %>% layout(title = "Pie Chart",
                      showlegend = TRUE,
                      template = "ggplot2",
                      hole = 0.5)

# Display the plot
fig

# Create a violin plot using Plotly
fig <- plot_ly(data, x = ~Road_surface_type, y = ~Number_of_casualties, type = "violin", 
               color = ~Accident_severity, colors = rainbow(length(unique(data$Accident_severity))),
               box = list(visible = TRUE, width = 0.2),
               points = "all") %>%
  layout(title = "Violin Plot",
         xaxis = list(title = "Road Surface Type"),
         yaxis = list(title = "Number of Casualties"),
         template = "ggplot2")

# Display the plot
fig

fig <- plot_ly(data, labels = ~Accident_severity, values = ~Number_of_casualties, type = 'pie',
               marker = list(colors = rainbow(length(unique(data$Accident_severity)))),
               hole = 0.5)

# Customize the layout
fig <- fig %>% layout(title = "Pie Chart",
                      showlegend = TRUE,
                      template = "ggplot2")

# Display the plot
fig

# Create a pie chart
fig <- plot_ly(data, labels = ~Cause_of_accident, values = ~Number_of_casualties, type = 'pie',
               marker = list(colors = rainbow(length(unique(data$Cause_of_accident)))),
               hole = 0.35)

# Customize the layout
fig <- fig %>% layout(title = "Pie Chart",
                      showlegend = TRUE,
                      template = "ggplot2")

# Display the plot
fig


# Create a histogram
ggplot(data, aes(x = Day_of_week, y = Number_of_casualties, fill = Day_of_week)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Histogram",
       x = "Day of Week",
       y = "Number of Casualties") +
  theme_minimal()

ggplot(data, aes(x = Educational_level, y = Number_of_casualties, fill = Educational_level)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Histogram",
       x = "Educational Level",
       y = "Number of Casualties") +
  theme_minimal()

ggplot(data, aes(x = Vehicle_driver_relation, y = Number_of_casualties, fill = Vehicle_driver_relation)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Histogram",
       x = "Vehicle Driver Relation",
       y = "Number of Casualties") +
  theme_minimal()

# Drop specified columns
parsed_data <- data %>%
  select(-Time, -Driving_experience, -Type_of_vehicle, -Educational_level,
         -Vehicle_driver_relation, -Lanes_or_Medians, -Owner_of_vehicle, -Area_accident_occured,
         -Road_allignment, -Types_of_Junction, -Light_conditions, -Weather_conditions,
         -Vehicle_movement, -Fitness_of_casuality, -Age_band_of_driver, -Sex_of_driver,
         -Pedestrian_movement, -Cause_of_accident, -Work_of_casuality,
         -Road_surface_conditions, -Service_year_of_vehicle, -Defect_of_vehicle)

# Remove rows with missing values
parsed_data <- na.omit(parsed_data)

# Print unique values and counts for object columns
for (i in names(parsed_data)) {
  if (class(parsed_data[[i]]) == "character") {
    cat(i, "\n")
    cat(unique(parsed_data[[i]]), "\n")
    cat(nlevels(as.factor(parsed_data[[i]])), "\n\n")
  }
}


# Convert character columns to factors
parsed_data[] <- lapply(parsed_data, function(x) if(is.character(x)) as.factor(x) else x)

# Convert factors to numeric
parsed_data[] <- lapply(parsed_data, function(x) if(is.factor(x)) as.numeric(x) else x)
# Encode categorical variables
for (col in names(parsed_data)) {
  if (class(parsed_data[[col]]) == "character") {
    parsed_data[[col]] <- as.numeric(labelEncoder(parsed_data[[col]]))
  }
}


# Split data into train and test sets
set.seed(123)  # for reproducibility
split <- sample.split(parsed_data$Accident_severity, SplitRatio = 0.7)
train_data <- subset(parsed_data, split == TRUE)
test_data <- subset(parsed_data, split == FALSE)

# Extract predictor variables and target variable
x_train <- train_data[, !(names(train_data) %in% c("Accident_severity"))]
y_train <- train_data$Accident_severity
x_test <- test_data[, !(names(test_data) %in% c("Accident_severity"))]
y_test <- test_data$Accident_severity

# Scale the predictor variables
scaler <- preProcess(x_train, method = c("center", "scale"))
x_train_scaled <- predict(scaler, x_train)
x_test_scaled <- predict(scaler, x_test)

# Print the first few rows of the scaled data frames
head(x_train_scaled)
head(x_test_scaled)

LRmodel <- lm(Accident_severity ~ ., data = train_data)

# Make predictions on the test set
y_pred <- predict(LRmodel, newdata = test_data)

# Evaluate the model using R-squared
r_squared <- R2(y_pred, test_data$Accident_severity)

# Print the R-squared score
cat("R-squared Score:", r_squared, "\n")
X <- select(parsed_data, -Accident_severity)
y <- parsed_data$Accident_severity
y_final <- predict(LRmodel, newdata = X)
# Create a data frame for comparison
comparison_df <- data.frame(Actual = y, Predicted = y_final)

# Scatter plot: Actual vs Predicted
fig_scatter <- plot_ly(data = comparison_df, x = ~Actual, y = ~Predicted, color = ~Actual,
                       type = 'scatter', mode = 'markers', marker = list(color = ~Actual),
                       showlegend = FALSE) %>%
  layout(template = 'plotly_dark', title = 'Comparison of Actual vs. Predicted',
         xaxis = list(title = 'Actual severenity'), yaxis = list(title = 'Predicted severanity'))


fig_hist <- plot_ly(data = comparison_df, x = ~Actual, color = I("red"),
                    nbins = 30, type = 'histogram', barmode = 'overlay',
                    histnorm = "probability density") %>%
  add_trace(x = ~Predicted, type = 'histogram', nbins = 30, barmode = 'overlay',
            histnorm = "probability density") %>%
  layout(template = 'plotly_dark', title = 'Distribution of Actual and Predicted severenity',
         xaxis = list(title = 'Severenity'), yaxis = list(title = 'Density'),
         color_discrete_sequence = c('#185ADB', '#FC5C9C'))
fig_hist

fig_residual <- ggplot(data = comparison_df, aes(x = Predicted, y = residuals)) +
  geom_point(color = 'orangered') +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'orange') +
  theme_minimal() +
  labs(title = 'Residual Plot', x = 'Predicted Values', y = 'Residuals')

# Display the residual plot
print(fig_residual)
# Predicted vs True Line Plot
fig_line <- plot_ly() %>%
  add_trace(x = ~y, y = ~y, type = 'scatter', mode = 'lines', line = list(color = '#98DFD6'), name = 'Ideal Line') %>%
  add_trace(x = ~y, y = ~y_final, type = 'scatter', mode = 'markers', marker = list(color = 'orangered'), name = 'Predicted Values') %>%
  add_trace(x = ~y, y = ~predict(lm(y_final ~ y)), type = 'scatter', mode = 'lines',
            line = list(color = '#FFDD83'), name = 'Regression Line') %>%
  layout(template = 'plotly_dark', title = 'Predicted vs. True Line Plot',
         xaxis = list(title = 'True Values'), yaxis = list(title = 'Predicted Values'))
