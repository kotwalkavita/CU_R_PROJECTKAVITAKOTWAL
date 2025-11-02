library(readr)
library(caTools)
library(caret)
library(e1071)

data <- read.csv("diabetes.csv")
head(data) 

summary(data)

colSums(is.na(data))

X <- data[, 1:8]
y <- data[, 9]

scaled_X <- as.data.frame(scale(X))

scaled_data <- cbind(scaled_X, y)

X <- scaled_data[, 1:8]
y <- scaled_data[, 9]

set.seed(123)
sample <- sample.split(y, SplitRatio = 0.7)
X_train <- X[sample == TRUE, ]
y_train <- y[sample == TRUE]
X_test <- X[sample == FALSE, ]
y_test <- y[sample == FALSE] 

library(ggplot2)
library(reshape2)

correlation_matrix <- cor(data)

correlation_melted <- melt(correlation_matrix)

ggplot(correlation_melted, aes(Var1, Var2, fill=value)) +
  geom_tile(color="white") +
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0, 
                       limit=c(-1,1), space="Lab", name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Correlation Heatmap", x="Features", y="Features")

outcome_counts <- table(data$Outcome)
outcome_df <- data.frame(Outcome = names(outcome_counts), 
                         Count = as.numeric(outcome_counts))

ggplot(outcome_df, aes(x=Outcome, y=Count)) +
  geom_bar(stat="identity", fill="pink") +
  labs(title="Distribution of Diabetes Outcomes", x="Outcome", y="Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size=12),
        plot.title = element_text(size=16, face="bold"))