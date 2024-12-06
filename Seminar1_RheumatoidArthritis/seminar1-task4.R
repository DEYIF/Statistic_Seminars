library(ggplot2)
library(dplyr)
library(tidyr)

# read file
setwd("E:/桌面/KTH/Year1_KTH/Statistics for Medical Engineering/Seminar1")
data <- read.csv("conctimedata_reduced.csv")
# extract essential data
data_subset <- data[, c("ID", "TIME", "DV")]
data_subset$ID <- as.factor(data_subset$ID)
data_subset$ID <- factor(data_subset$ID, labels = paste0("Patient", 1:12))
patient_colors <- setNames(rainbow(12), paste0("Patient", 1:12))
# draw DV-TIME curve
p <- ggplot(data_subset, aes(x = TIME, y = DV, group = ID, color = ID)) +
  geom_line(size = 0.5) + 
  labs(title = "Concentration-Time Profiles for 12 Patients",
       x = "Time (hours)", y = "Concentration (mg/L)") +
  scale_color_manual(values = patient_colors, 
                     labels = paste0("Patient", 1:12)) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 12, face="bold", hjust = 0.5))
# save and adjust windows width & height
ggsave("concentration_profiles.jpg", plot = p, 
       width = 1600/300, height = 1200/300, units = "in", dpi = 300)
print(p)

# draw boxplot
p_box <- ggplot(data_subset, aes(x = ID, y = DV, fill = ID)) +
  geom_boxplot() +
  labs(title = "Concentration Distribution by Patient",
       x = "Patient ID", 
       y = "Concentration (mg/L)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 12, face="bold", hjust = 0.5))
# save and adjust windows width & height
ggsave("concentration_boxplot.jpg", plot = p_box,
       width = 1600/300, height = 1200/300, units = "in", dpi = 300)
print(p_box)


# Cubic Spline Interpolation
# create a new container to save data
interpolated_data <- data.frame(TIME = numeric(), DV = numeric(), ID = character())
# Cubic Spline Interpolation
for (id in levels(data_subset$ID)) {
  subset_data <- data_subset[data_subset$ID == id, ]
  subset_data <- subset_data[!(subset_data$TIME == 0 & subset_data$DV == 0), ]
  interpolated <- spline(subset_data$TIME, subset_data$DV, xout = seq(min(subset_data$TIME), max(subset_data$TIME), length.out = 100))
  interpolated_data <- rbind(interpolated_data, data.frame(TIME = interpolated$x, DV = interpolated$y, ID = id))
}
interpolated_data$ID <- factor(interpolated_data$ID, levels = levels(data_subset$ID))
# draw a new curve
p_interpolated <- ggplot(interpolated_data, aes(x = TIME, y = DV, group = ID, color = factor(ID))) +
  geom_line(size = 0.5) +
  labs(title = "Interpolated Concentration-Time Profiles for 12 Patients",
       x = "Time (hours)", y = "Concentration (mg/L)") +
  scale_color_manual(values = patient_colors,
                     labels = paste0("Patient", 1:12)) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 12, face="bold", hjust = 0.5)) +
  coord_cartesian(ylim = c(0, NA))

ggsave("interpolated_concentration_profiles.jpg", plot = p_interpolated, 
       width = 1600/300, height = 1200/300, units = "in", dpi = 300)
print(p_interpolated)


# initialize the container for storing integral results
integrated_results <- data.frame(ID = character(), Integral = numeric(), stringsAsFactors = FALSE)

# set integral interval
lower_bound <- 1
upper_bound <- 166

# Integral after interpolation
for (id in levels(interpolated_data$ID)) {
  subset_data <- interpolated_data[interpolated_data$ID == id, ]
  
  # if there are enough data points to do the integration
  if (nrow(subset_data) > 1) {
    # create cubic spline interpolation function based on the interpolated data
    spline_function <- splinefun(subset_data$TIME, subset_data$DV)
    
    # perform the integral over the interval [1, 166]
    integral_value <- integrate(spline_function, lower = lower_bound, upper = upper_bound)$value
    
    # save the integral result
    integrated_results <- rbind(integrated_results, data.frame(ID = id, Integral = integral_value))
  } else {
    message(paste("ID", id, "error"))
  }
}
# print the integral results
print(integrated_results)


# read file
data <- read.csv("data_task4.csv", sep = ",")
# multiple linear regression
model <- lm(AUC ~ WGT + BSA + AGE + HGT + DOSE + GFR, data = data)
summary(model)

# 模型诊断
par(mfrow = c(2, 2)) # 创建多个图形窗口

# 残差与拟合值图
plot(fitted(model), rstandard(model), main = "Residuals vs Fitted", xlab = "Fitted values", ylab = "Standardized residuals")
abline(h = 0, col = "red")

# 正态Q-Q图
qqnorm(rstandard(model), main = "Normal Q-Q")
qqline(rstandard(model), col = "red")

# 残差与杠杆值图
plot(hatvalues(model), rstandard(model), 
     main = "Residuals vs Leverage", 
     xlab = "Leverage", 
     ylab = "Standardized residuals", 
     ylim = c(-3, 3))  # 调整y轴范围为-3到3

abline(h = 0, col = "black", lty = 1)  # 在y=0处画实线
abline(h = c(-2, 2), col = "red", lty = 2)  # 在y=±2处画虚线


# Cook's距离图
plot(cooks.distance(model), 
     main = "Cook's Distance", 
     ylab = "Cook's Distance", 
     xlab = "Index", 
     ylim = c(-0.1, 0.7))  # 调整y轴范围为-3到3
abline(h = 4 / length(data$AUC), col = "red")
