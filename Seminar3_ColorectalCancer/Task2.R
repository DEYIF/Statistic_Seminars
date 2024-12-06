# 加载必要的包
library(tidyverse)
library(MASS)
library(car)
library(ggplot2)
library(gridExtra)
library(nnet)

# 读取数据
data <- read.csv("data_task2.csv")
names(data) <- c("Index", "Stage", "Age", "Ethnicity", "Partner", "Site")

# 将Stage转换为因子
data$Stage <- as.factor(data$Stage)
data$Ethnicity <- as.factor(data$Ethnicity)
data$Partner <- as.factor(data$Partner)
data$Site <- as.factor(data$Site)

# 1. 描述性统计
# Stage distribution
stage_dist <- table(data$Stage)
print("Stage Distribution:")
print(stage_dist)

# Age statistics by stage
age_stats <- data %>%
  group_by(Stage) %>%
  summarise(
    mean_age = mean(Age),
    sd_age = sd(Age),
    min_age = min(Age),
    max_age = max(Age)
  )
print("\nAge Statistics by Stage:")
print(age_stats)

# Cross tabulations
ethn_stage <- table(data$Ethnicity, data$Stage)
print("\nEthnicity by Stage:")
print(ethn_stage)

part_stage <- table(data$Partner, data$Stage)
print("\nPartner Status by Stage:")
print(part_stage)

site_stage <- table(data$Site, data$Stage)
print("\nCancer Site by Stage:")
print(site_stage)

# 2. 统计检验
# Chi-square tests
chi2_ethn <- chisq.test(ethn_stage)
chi2_part <- chisq.test(part_stage)
chi2_site <- chisq.test(site_stage)

print("\nChi-square Test Results:")
print("Ethnicity vs Stage:")
print(chi2_ethn)
print("\nPartner Status vs Stage:")
print(chi2_part)
print("\nCancer Site vs Stage:")
print(chi2_site)

# ANOVA for age differences
age_anova <- aov(Age ~ Stage, data = data)
print("\nANOVA Results (Age vs Stage):")
print(summary(age_anova))

# 3. 有序逻辑回归
# 转换Stage为有序因子
data$Stage <- ordered(data$Stage)
model <- polr(Stage ~ Age + Ethnicity + Partner + Site, data = data)
summary_model <- summary(model)
print("\nOrdered Logistic Regression Results:")
print(summary_model)

# 4. 可视化
# Stage distribution plot
p1 <- ggplot(data, aes(x = Stage)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribution of CRC Stages")

# Age distribution by stage
p2 <- ggplot(data, aes(x = Stage, y = Age)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Age Distribution by Stage")

# Stage distribution by site
p3 <- ggplot(data, aes(x = Site, fill = Stage)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Stage Distribution by Site") +
  theme(axis.text.x = element_text(angle = 45))

# Stage distribution by ethnicity
p4 <- ggplot(data, aes(x = Ethnicity, fill = Stage)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Stage Distribution by Ethnicity") +
  theme(axis.text.x = element_text(angle = 45))

# Stage distribution by lifestyle
ggplot(data, aes(x = Partner, fill = Stage)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Stage Distribution by Lifestyle") +
  theme(axis.text.x = element_text(angle = 45))

# Combine plots
grid.arrange(p1, p2, p3, p4, ncol = 2)

# 5. 预测概率分析
# 计算不同特征组合的预测概率
newdata <- expand.grid(
  Age = c(25, 30, 35),
  Ethnicity = unique(data$Ethnicity),
  Partner = unique(data$Partner),
  Site = unique(data$Site)
)

# 预测概率
pred_probs <- predict(model, newdata, type = "probs")


# multinom Logistic Regression
model <- multinom(Stage ~ Age + Ethnicity + Partner + Site, data = data)
summary(model)
z_values <- summary(model)$coefficients / summary(model)$standard.errors
p_values <- (1 - pnorm(abs(z_values), 0, 1)) * 2
p_values
z_values
