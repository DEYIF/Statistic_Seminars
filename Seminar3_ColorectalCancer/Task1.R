library(corrplot) # for plotting correlation matrix
library(ggplot2)
library(leaps)  # for best subset selection
library(gridExtra)  # for combining plots
library(glmnet)  # for ridge regression
library(pROC)  # for ROC curve

# import data
data <- read.csv('data_task1.csv')
data <- data[,-1]
feature <- data[,-1]
scaledFeature <- data.frame(scale(feature)) # without label
scaledData <- data.frame(cbind(DV=data$DV, scaledFeature))  # with label

# correlation matrix, representing linear relationship
corrMat <- cor(scaledData)
corrplot(corrMat,
         method = "circle",     
         type = "upper",         
         col = COL2("RdBu", 10),
         tl.col = "black",
         tl.srt = 45,
         order = "original")
# 3 pairs of features have high correlation, which may cause multicollinearity.
# TC-LDL, HDL-APOA1, WBC-NEU
# plot each pair of high-related features
p1 <- ggplot(data, aes(x = TC, y = LDL)) + 
  geom_point() + 
  geom_smooth(method = "lm")+
  theme_minimal()
p2 <- ggplot(data, aes(x = HDL, y = APOA1)) + 
  geom_point() + 
  geom_smooth(method = "lm")+
  theme_minimal()
p3 <- ggplot(data, aes(x = WBC, y = NEU)) + 
  geom_point() + 
  geom_smooth(method = "lm")+
  theme_minimal()
grid.arrange(p1, p2, p3, ncol = 2)

# Divide into training and testing sets
set.seed(2024)
# split the data into training and testing sets
trainIndex <- sample(1:nrow(data), 0.7*nrow(data))
trainFea <- scaledFeature[trainIndex,]
trainLabel <- scaledData[trainIndex,1]
testFea <- scaledFeature[-trainIndex,]
testLabel <- scaledData[-trainIndex,1]

x = as.matrix(trainFea) # transfer the data frame to matrix
y = as.matrix(trainLabel)
newx <- as.matrix(testFea)

# Logistic Regression
logic_reg = glm(y ~ ., data = trainFea, family = "binomial")

#Elastic Net
elastic_net_model <- cv.glmnet(x, y, alpha = 0.5, family = "binomial")  # 对于分类问题，选择 family = "binomial"
plot(elastic_net_model)


best_lambda <- elastic_net_model$lambda.min  # 选择使得均方误差最小的lambda值
print(best_lambda)

best_model_coeff <- coef(elastic_net_model, s = "lambda.min")
print(best_model_coeff)
# Check the selected features
selected_features <- rownames(best_model_coeff)[which(best_model_coeff != 0)]
selected_features <- selected_features[-1]  # delete the intercept
print(selected_features)


# predict
pre_logic_prob <- predict(logic_reg, newdata = testFea, type = "response")
pre_elas_prob <- predict(elastic_net_model, s = "lambda.min", newx = newx, type = "response")
# 将预测结果转换为01分类类型
pre_logic_class <- ifelse(pre_logic_prob > 0.5, 1, 0)
pre_elas_class <- ifelse(pre_elas_prob > 0.3, 1, 0)

# 计算评估指标：准确率、精确率、召回率、F1值、AUC、ROC曲线
# Accuracy
accuracy_logic <- sum(pre_logic_class == testLabel) / length(testLabel)
accuracy_elas <- sum(pre_elas_class == testLabel) / length(testLabel)
# Precision
precision_logic <- sum(pre_logic_class == 1 & testLabel == 1) / sum(pre_logic_class == 1)
precision_elas <- sum(pre_elas_class == 1 & testLabel == 1) / sum(pre_elas_class == 1)
# Recall
recall_logic <- sum(pre_logic_class == 1 & testLabel == 1) / sum(testLabel == 1)
recall_elas <- sum(pre_elas_class == 1 & testLabel == 1) / sum(testLabel == 1)
# F1
f1_logic <- 2 * precision_logic * recall_logic / (precision_logic + recall_logic)
f1_elas <- 2 * precision_elas * recall_elas / (precision_elas + recall_elas)
# AUC
roc_logic <- roc(testLabel, pre_logic_class)
roc_elas <- roc(testLabel, pre_elas_class)
# ROC曲线
plot(roc_logic, col = "red", lwd = 2, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")
lines(roc_elas, col = "blue", lwd = 2)
legend("bottomright", legend = c("Logistic Regression", "Elastic Net"), col = c("red", "blue"), lty = 1, lwd = 2)

