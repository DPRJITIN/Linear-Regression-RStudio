#Step 1: Import Necessary Libraries
library(datasets)
library(caTools)
library(ggplot2)

#Step 2: Load and Explore the Dataset
data("iris")
str(iris)
summary(iris)

#Step 3: Split the Data
set.seed(123)
split <- sample.split(iris$Sepal.Length, SplitRatio = 0.8)
training_set <- subset(iris, split == TRUE)
test_set <- subset(iris, split == FALSE)

#Step 4: Train the Linear Regression Model
model <- lm(Sepal.Length ~ ., data = training_set)
summary(model)

#Step 5: Make Predictions and Evaluate the Model
predictions <- predict(model, newdata = test_set)
rmse <- sqrt(mean((predictions - test_set$Sepal.Length) ^ 2))
print(rmse)

#Step 6: Visualise the Results
results <- data.frame(Actual = test_set$Sepal.Length, Predicted = predictions)
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +  #Scatter plot
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +  #Diagonal line
  geom_segment(aes(x = Actual, y = Predicted, xend = Actual, yend = Actual), color = "blue", linetype = "dotted") +  #Residual lines
  ggtitle("Actual vs Predicted Sepal Length") +
  xlab("Actual Sepal Length") +
  ylab("Predicted Sepal Length")