library(caret)
library(irr)
library(pROC)
library(ggplot2)

data <- read.csv("data_task4.csv")
# Using ROC to determine the threshold、
roc_obj <- roc(data$labels_obs,data$prob_pred)
AUC <- roc_obj$auc
print(AUC)
roc_data <- data.frame(
  Spec = roc_obj$specificities,      # Specificity
  Sens = roc_obj$sensitivities,      # Sensitivities
  Thresholds = roc_obj$thresholds
)
# draw ROC
ggplot(roc_data, aes(x = Spec, y = Sens)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "red") +  # 参考线
  labs(
    title = "ROC Curve",
    x = "Specificity",
    y = "Sensitivity"
  ) +
  theme_minimal() +
  scale_x_reverse()  # Reverse X-axis

optimal_idx <- which.max(roc_obj$sensitivities + roc_obj$specificities)  # maximize sens + spec
optimal_threshold <- roc_obj$thresholds[optimal_idx]
print(paste("Optimal Threshold:", optimal_threshold))

optimal_point <- roc_data[optimal_idx, ]
ggplot(roc_data, aes(x = Spec, y = Sens)) +
  geom_point(aes(x=Spec,y=Sens),data=optimal_point,color="red",size=6,shape=18)+
  geom_line(color = "blue", size = 1) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "red") +  # 参考线
  labs(
    title = "ROC Curve",
    x = "Specificity",
    y = "Sensitivity"
  ) +
  theme_minimal() +
  scale_x_reverse()  # Reverse X-axis


#give a threshold
binary_pred = numeric(nrow(data))
for (i in data$X){
  if (data$prob_pred[i] >= optimal_threshold){
    binary_pred[i] = 1
  }
  else{
    binary_pred[i] = 0
  }
}
CM <- confusionMatrix(factor(binary_pred),factor(data$labels_obs))
print(CM$overall)
print(CM$byClass)

# determine target
# filter
filtered_90 <- roc_data[roc_data$Spec > 0.9, ]
# find the max Sensitivity in this situation
threshold_90 <- filtered_90$Threshold[which.max(filtered_90$Sens)]
print(paste("Threshold 0.9 Specificity:", threshold_90))
idx_90 <- which(roc_data$Threshold == threshold_90)

# determine target
# filter
filtered_95 <- roc_data[roc_data$Spec > 0.95, ]
# find the max Sensitivity in this situation
threshold_95 <- filtered_95$Threshold[which.max(filtered_95$Sens)]
print(paste("Threshold 0.95 Specificity:", threshold_95))
idx_95 <- which(roc_data$Threshold == threshold_95)

optimal_point_90 <- roc_data[idx_90, ]
optimal_point_95 <- roc_data[idx_95, ]

ggplot(roc_data, aes(x = Spec, y = Sens)) +
  geom_point(aes(x=Spec,y=Sens),data=optimal_point,color="red",size=6,shape=18)+
  geom_point(aes(x=Spec,y=Sens),data=optimal_point_90,color="green",size=6,shape=15)+
  geom_point(aes(x=Spec,y=Sens),data=optimal_point_95,color="orange",size=6,shape=17)+
  geom_line(color = "blue", size = 1) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "red") +  # 参考线
  labs(
    title = "ROC Curve",
    x = "Specificity",
    y = "Sensitivity"
  ) +
  theme_minimal() +
  scale_x_reverse()  # Reverse X-axis

#give a threshold
binary_pred = numeric(nrow(data))
for (i in data$X){
  if (data$prob_pred[i] >= threshold_90){
    binary_pred[i] = 1
  }
  else{
    binary_pred[i] = 0
  }
}
CM <- confusionMatrix(factor(binary_pred),factor(data$labels_obs))
print(CM$overall)
print(CM$byClass)

#give a threshold
binary_pred = numeric(nrow(data))
for (i in data$X){
  if (data$prob_pred[i] >= threshold_95){
    binary_pred[i] = 1
  }
  else{
    binary_pred[i] = 0
  }
}
CM <- confusionMatrix(factor(binary_pred),factor(data$labels_obs))
print(CM$overall)
print(CM$byClass)
