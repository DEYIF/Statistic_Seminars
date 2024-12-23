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
  scale_x_reverse() +  # Reverse X-axis
  coord_equal()        # set x equal to y


optimal_idx <- which.max(roc_obj$sensitivities + roc_obj$specificities)  # maximize sens + spec
optimal_threshold <- roc_obj$thresholds[optimal_idx]
print(paste("Optimal Threshold:", optimal_threshold))

optimal_weighted_idx <- which.max(2*(roc_obj$sensitivities) + roc_obj$specificities)  # maximize sens + spec
optimal_weighted_threshold <- roc_obj$thresholds[optimal_weighted_idx]
print(paste("Optimal Threshold:", optimal_weighted_threshold))

# determine target
# filter
filtered_90 <- roc_data[roc_data$Sens > 0.9, ]
# find the max Sensitivity in this situation
threshold_90 <- filtered_90$Threshold[which.max(filtered_90$Spec)]
print(paste("Threshold 0.9 Sensitivity:", threshold_90))
idx_90 <- which(roc_data$Threshold == threshold_90)

# determine target
# filter
filtered_95 <- roc_data[roc_data$Sens > 0.95, ]
# find the max Sensitivity in this situation
threshold_95 <- filtered_95$Threshold[which.max(filtered_95$Spec)]
print(paste("Threshold 0.95 Specificity:", threshold_95))
idx_95 <- which(roc_data$Threshold == threshold_95)

optimal_point <- roc_data[optimal_idx, ]
optimal_weighted_point <- roc_data[optimal_weighted_idx, ]
optimal_point_90 <- roc_data[idx_90, ]
optimal_point_95 <- roc_data[idx_95, ]


ggplot(roc_data, aes(x = Spec, y = Sens)) +
  # 添加点，使用分类变量映射到颜色和形状
  geom_point(data = optimal_point, aes(x = Spec, y = Sens, color = "Optimal", shape = "Optimal"), size = 6) +
  geom_point(data = optimal_weighted_point, aes(x = Spec, y = Sens, color = "Optimal Weighted", shape = "Optimal Weighted"), size = 6) +
  geom_point(data = optimal_point_90, aes(x = Spec, y = Sens, color = "Sensitivity 90", shape = "Sensitivity 90"), size = 6) +
  geom_point(data = optimal_point_95, aes(x = Spec, y = Sens, color = "Sensitivity 95", shape = "Sensitivity 95"), size = 6) +
  # ROC 曲线
  geom_line(color = "blue", size = 1) +
  # 参考线
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "red") +
  # 标签和主题
  labs(
    title = "ROC Curve",
    x = "Specificity",
    y = "Sensitivity",
    color = "Legend",  # 图例标题（颜色）
    shape = "Legend"   # 图例标题（形状）
  ) +
  theme_minimal() +
  scale_x_reverse() +  # Reverse X-axis
  coord_equal() +      # 设置比例相等
  # 自定义图例内容
  scale_color_manual(values = c(
    "Optimal" = "red",
    "Optimal Weighted" = "purple",
    "Sensitivity 90" = "green",
    "Sensitivity 95" = "orange"
  )) +
  scale_shape_manual(values = c(
    "Optimal" = 18,
    "Optimal Weighted" = 16,
    "Sensitivity 90" = 15,
    "Sensitivity 95" = 17
  ))


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


#give a threshold
binary_pred = numeric(nrow(data))
for (i in data$X){
  if (data$prob_pred[i] >= optimal_weighted_threshold){
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
