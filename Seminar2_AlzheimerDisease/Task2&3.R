# Task2
library(ggplot2)
library(dplyr)  #for using group_by()
library(lme4) #to use mixed-effect model
data_2 <- read.csv("data_task2.csv")
# scatter plot
ggplot(data_2, aes(x = age, y = DV_amyloid)) +
  geom_point(pch = 19, col = rgb(0.15,0.6,0.96,0.6)) +
  labs(title = "DV Amyloid vs Age", x = "Age", y = "DV Amyloid") +
  theme_minimal()

## construct linear model
model_linear <- lm(DV_amyloid ~ age, data = data_2)
summary(model_linear)
coefficients(model_linear)
minAge= min(data_2$age)
maxAge= max(data_2$age)
new_data <- data.frame(age = seq(minAge, maxAge, by = 1)) # Create regression data
new_data$predicted_linear_Amyloid <- predict(model_linear, newdata = new_data)

# plot regression
ggplot() +
  geom_point(data = data_2, aes(x= age, y=DV_amyloid), color = rgb(0.15, 0.6, 0.96, 0.6), size = 2) +  # 绘制散点
  geom_line(data = new_data,aes(x = age, y = predicted_linear_Amyloid), color = "red", size = 1.2) +  # 添加线性回归直线
  labs(title = "Linear Fit vs. Age", 
       x = "Age", 
       y = "DV Amyloid") +
  theme_minimal()

# residual vs fitted
merged_data <- merge(data_2, new_data, by = "age")  # find the corresponding predicted value
merged_data$residual_linear <- merged_data$DV_amyloid - merged_data$predicted_linear_Amyloid

ggplot(merged_data, aes(x = predicted_linear_Amyloid, y = residual_linear)) +
  geom_point(color = rgb(0.15, 0.6, 0.96, 0.6), size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Linear model Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Q-Q Plot
ggplot(merged_data, aes(sample = residual_linear)) +
  stat_qq(alpha = 0.5, pch = 19, col = rgb(0.15,0.6,0.96,0.6), size = 2) +
  stat_qq_line(color = "red", size = 1) +
  labs(title = "linear model Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()

# Shapiro-Wilk test
shpiro_result <- shapiro.test(merged_data$residual_linear)
print(shpiro_result)


## construct logarithmic model
model_log <- lm(DV_amyloid ~ log(age), data = data_2)
summary(model_log)

# plot regression
new_data$predicted_log_Amyloid <- predict(model_log, newdata = new_data)
temp <- merge(data_2, new_data, by = "age")  # find the corresponding predicted value
merged_data$predicted_log_Amyloid <- temp$predicted_log_Amyloid
merged_data$residual_log <- merged_data$DV_amyloid - merged_data$predicted_log_Amyloid

ggplot() +
  geom_point(data = data_2, aes(x= age, y=DV_amyloid), color = rgb(0.15, 0.6, 0.96, 0.6), size = 2) +  # 绘制散点
  geom_line(data = new_data,aes(x = age, y = predicted_log_Amyloid), color = "red", size = 1.2) +  # 添加线性回归直线
  labs(title = "Logarithmic Fit vs. Age", 
       x = "Age", 
       y = "DV Amyloid") +
  theme_minimal()

# residual vs fitted
ggplot(merged_data, aes(x = predicted_linear_Amyloid, y = residual_log)) +
  geom_point(color = rgb(0.15, 0.6, 0.96, 0.6), size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "logarithmic model Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Q-Q plot
ggplot(merged_data, aes(sample = residual_log)) +
  stat_qq(alpha = 0.5, pch = 19, col = rgb(0.15,0.6,0.96,0.6), size = 2) +
  stat_qq_line(color = "red", size = 1) +
  labs(title = "logarithmic model Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()

# Shapiro-Wilk test
shpiro_result <- shapiro.test(merged_data$residual_log)
print(shpiro_result)



## construct polynomial model(2)
model_poly2 <- lm(DV_amyloid ~ poly(age,2), data = data_2)
summary(model_poly2)

# plot regression
new_data$predicted_poly2_Amyloid <- predict(model_poly2, newdata = new_data)
temp <- merge(data_2, new_data, by = "age")  # find the corresponding predicted value
merged_data$predicted_poly2_Amyloid <- temp$predicted_poly2_Amyloid
merged_data$residual_poly2 <- merged_data$DV_amyloid - merged_data$predicted_poly2_Amyloid

ggplot() +
  geom_point(data = data_2, aes(x= age, y=DV_amyloid), color = rgb(0.15, 0.6, 0.96, 0.6), size = 2) +  # 绘制散点
  geom_line(data = new_data,aes(x = age, y = predicted_poly2_Amyloid), color = "red", size = 1.2) +  # 添加线性回归直线
  labs(title = "polynomial2 Fit vs. Age", 
       x = "Age", 
       y = "DV Amyloid") +
  theme_minimal()

# residual vs fitted
ggplot(merged_data, aes(x = predicted_poly2_Amyloid, y = residual_poly2)) +
  geom_point(color = rgb(0.15, 0.6, 0.96, 0.6), size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "polynomial2 model Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Q-Q plot
ggplot(merged_data, aes(sample = residual_poly2)) +
  stat_qq(alpha = 0.5, pch = 19, col = rgb(0.15,0.6,0.96,0.6), size = 2) +
  stat_qq_line(color = "red", size = 1) +
  labs(title = "polynomial2 model Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()


# Shapiro-Wilk test
shpiro_result <- shapiro.test(merged_data$residual_poly2)
print(shpiro_result)



## predict age from 75 to 110
pre_data <- data.frame(age=seq(75,110,by = 1))
pre_data$amy_linear <- predict(model_linear, newdata=pre_data)
pre_data$amy_log <- predict(model_log, newdata=pre_data)
pre_data$amy_poly2 <- predict(model_poly2, newdata=pre_data)



data_combined <- rbind(
  data.frame(age = data_2$age, DV_amyloid = data_2$DV_amyloid, model = "Observed"),
  data.frame(age = pre_data$age, DV_amyloid = pre_data$amy_linear, model = "Linear"),
  data.frame(age = pre_data$age, DV_amyloid = pre_data$amy_log, model = "Log"),
  data.frame(age = pre_data$age, DV_amyloid = pre_data$amy_poly2, model = "Polynomial2")
)


# 绘制图形
ggplot(data_combined, aes(x = age, y = DV_amyloid, color = model, shape = model), size = 2) +
  geom_point(size = 1.5) +
  labs(title = "Prediction of DV Amyloid vs Age", x = "Age", y = "DV Amyloid") +
  scale_color_manual(values = c("Observed" = rgb(0.15, 0.6, 0.96), 
                                "Linear" = rgb(0.25, 0.8, 0.4), 
                                "Log" = rgb(0.55, 0.25, 0.8), 
                                "Polynomial2" = rgb(0.9, 0.7, 0.1))) +
  scale_shape_manual(values = c("Observed" = 16,  # 圆形
                                "Linear" = 17,   # 三角形
                                "Log" = 15,      # 方形
                                "Polynomial2" = 18)) +  # 菱形
  theme_minimal()




# Task3
# 读取数据
data_3 <- read.csv("data_task3.csv")
eval_data <- merge(pre_data, data_3, by = "age")

data_3$model <- "Ground Truth"
pre_data_label <- subset(data_combined, model != "Observed")

# evaluate the model
eval_data$linear_residual <- eval_data$DV_amyloid - eval_data$amy_linear
eval_data$log_residual <- eval_data$DV_amyloid - eval_data$amy_log
eval_data$poly2_residual <- eval_data$DV_amyloid - eval_data$amy_poly2
MSE_linear <- mean(eval_data$linear_residual^2)
MSE_log <- mean(eval_data$log_residual^2)
MSE_poly2 <- mean(eval_data$poly2_residual^2)


# 绘制散点图
ggplot() +
  geom_point(data = pre_data_label, aes(x = age, y = DV_amyloid, color = model, shape = model), size = 2) +
  geom_point(data = data_3, aes(x = age, y = DV_amyloid, color = model, shape = model), size = 2) +  # 将颜色映射到 "Ground Truth"
  labs(title = "Prediction of DV Amyloid vs Age", x = "Age", y = "DV Amyloid") +
  scale_color_manual(values = c("Linear" = rgb(0.25, 0.8, 0.4), 
                                "Log" = rgb(0.55, 0.25, 0.8), 
                                "Polynomial2" = rgb(0.9, 0.7, 0.1), 
                                "Ground Truth" = rgb(0.15, 0.6, 0.96))) +
  scale_shape_manual(values = c("Ground Truth" = 16,  # 圆形
                                "Linear" = 17,   # 三角形
                                "Log" = 15,      # 方形
                                "Polynomial2" = 18)) +  # 菱形
  theme_minimal()



# improved model
# construct polynomial model(3)
model_poly3 <- lm(DV_amyloid ~ poly(age,3), data = data_2)
summary(model_poly3)

# plot regression
new_data$predicted_poly3_Amyloid <- predict(model_poly3, newdata = new_data)
temp <- merge(data_2, new_data, by = "age")  # find the corresponding predicted value
merged_data$predicted_poly3_Amyloid <- temp$predicted_poly3_Amyloid
merged_data$residual_poly3 <- merged_data$DV_amyloid - merged_data$predicted_poly3_Amyloid

ggplot() +
  geom_point(data = data_2, aes(x= age, y=DV_amyloid), color = rgb(0.15, 0.6, 0.96, 0.6), size = 2) +  # 绘制散点
  geom_line(data = new_data,aes(x = age, y = predicted_poly3_Amyloid), color = "red", size = 1.2) +  # 添加线性回归直线
  labs(title = "polynomial3 Fit vs. Age", 
       x = "Age", 
       y = "DV Amyloid") +
  theme_minimal()

# residual vs fitted
ggplot(merged_data, aes(x = predicted_poly3_Amyloid, y = residual_poly3)) +
  geom_point(color = rgb(0.15, 0.6, 0.96, 0.6), size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "polynomial3 model Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Q-Q plot
ggplot(merged_data, aes(sample = residual_poly3)) +
  stat_qq(alpha = 0.5, pch = 19, col = rgb(0.15,0.6,0.96,0.6), size = 2) +
  stat_qq_line(color = "red", size = 1) +
  labs(title = "polynomial3 model Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()

# Shapiro-Wilk test
shpiro_result <- shapiro.test(merged_data$residual_poly3)
print(shpiro_result)


# predict age from 75 to 110
temp <- data.frame(age=seq(75,110,by = 1))
temp$DV_amyloid <- predict(model_poly3, newdata=temp)
temp$model <- "Polynomial3"
pre_data_label <- rbind(pre_data_label, temp)

ggplot() +
  geom_point(data = data_3, aes(x = age, y = DV_amyloid, color = model, shape = model), size = 2) +  # 绘制散点
  geom_point(data = pre_data_label, aes(x = age, y = DV_amyloid, color = model, shape = model), size = 2) +  # 添加多项式拟合
  labs(title = "Polynomial 3 Fit vs. Age", 
       x = "Age", 
       y = "Mean DV Amyloid") +
  scale_color_manual(values = c("Linear" = rgb(0.25, 0.8, 0.4,), 
                                "Log" = rgb(0.55, 0.25, 0.8), 
                                "Polynomial2" = rgb(0.9, 0.7, 0.1), 
                                "Ground Truth" = rgb(0.15, 0.6, 0.96),
                                "Polynomial3" = "red")) +
  scale_shape_manual(values = c("Ground Truth" = 16,  # 圆形
                                "Linear" = 17,   # 三角形
                                "Log" = 15,      # 方形
                                "Polynomial2" = 18, # 菱形
                                "Polynomial3" = 20)) + 
  theme_minimal()

temp <- data.frame(age=seq(75,110,by = 1))
temp$amy_poly3 <- predict(model_poly3, newdata=temp)
eval_data <- merge(eval_data, temp, by = "age")
eval_data$poly3_residual <- eval_data$DV_amyloid - eval_data$amy_poly3
MSE_poly3 <- mean(eval_data$poly3_residual^2)
summary(model_poly3)


