library(rpart)
library(rpart.plot)
library(caret)
library(tidyverse)
library(ggplot2)
library(plotly)
library(nnet)
library(ggpubr)
 
 


df <- read.csv("D:/train_values.csv")

df$damage_grade <- factor(df$damage_grade)
set.seed(123) # set seed for reproducibility
train_idx <- createDataPartition(df$damage_grade, p = 0.7, list = FALSE)
train_data <- df[train_idx, ]
test_data <- df[-train_idx, ]

formula <- as.formula("damage_grade ~ age + count_floors_pre_eq")

model <- rpart(formula, data = train_data)

rpart.plot(model, type = 3, extra = 1)

predictions <- predict(model, newdata = test_data, type = "class")






ggplot(data = train_data, aes(x = damage_grade)) + 
        geom_bar() +
        ggtitle("Damage Grade Distribution")


ggplot(data = train_data, aes(x = count_floors_pre_eq)) +
        geom_histogram(aes(fill = factor(damage_grade)), position = "dodge") +
        labs(x = "# of Floors ", y = "Frequency", title = "# of Floors  vs damage ") +
        scale_fill_discrete(name = "damage_grade", labels = c("1", "2", "3")) +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(legend.position = "bottom") +
        coord_flip()



ggplot(data = train_data, aes(x = age)) +
        geom_histogram(aes(fill = factor(train_data$damage_grade)),
                       position = "dodge") +
        xlab("Building's Age") +
        ylab("Frequency") +
        ggtitle("Age vs damage grade") +
        scale_fill_discrete(name = "Damage Grade",
                            labels = c("1", "2", "3")) +
        theme(axis.text.x = element_text(angle = 90))





model1 <- multinom(formula, data = train_data)
summary(model1)
predictions <- predict(model1, newdata = test_data)

table(factor(predictions, levels=min(test):max(test)), 
      factor(test_data, levels=min(test):max(test)))

confusion_matrix <- table(Predicted = predictions, Actual = test_data$damage_grade)

ggplot() +
        geom_tile(data = as.data.frame(confusion_matrix), aes(x = Actual, y = Predicted, fill = Freq)) +
        geom_text(data = as.data.frame(confusion_matrix), aes(x = Actual, y = Predicted, label = Freq)) +
        scale_fill_gradient(low = "white", high = "steelblue", name = "Frequency") +
        labs(title = "Confusion Matrix", x = "Actual Damage Grade", y = "Predicted Damage Grade")

conf_mat <- confusionMatrix(
        factor(predictions, levels = 1:3),
        factor(test_data$damage_grade, levels = 1:3)
)

conf_mat


