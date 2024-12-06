# Load necessary libraries
library(ggplot2)
library(dplyr)
library(car)      # For ANOVA
library(multcomp) # For pairwise comparisons

# Load the dataset
data <- read.csv("data_task4.csv")

# Remove unnecessary index column
data <- data[, -1]


# 1. 正态性检验
# 使用 Shapiro-Wilk 检验每个组的正态性
shapiro_test_control <- shapiro.test(data$AMYLOIDB[data$GROUP == "control"])
shapiro_test_gene_exp1 <- shapiro.test(data$AMYLOIDB[data$GROUP == "gene_exp1"])
shapiro_test_gene_exp2 <- shapiro.test(data$AMYLOIDB[data$GROUP == "gene_exp2"])

print(shapiro_test_control)
print(shapiro_test_gene_exp1)
print(shapiro_test_gene_exp2)

# 也可以通过 QQ 图来可视化正态性
ggplot(data, aes(sample = AMYLOIDB, color = GROUP)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~GROUP) +
  theme_minimal()

# 移除data中的control group 中AmyloidVB值最高的两行数据
data <- data %>%
  filter(!(GROUP == "control" & AMYLOIDB %in% tail(sort(AMYLOIDB), 2)))

#再次分析正态性
shapiro_test_control <- shapiro.test(data$AMYLOIDB[data$GROUP == "control"])
shapiro_test_gene_exp1 <- shapiro.test(data$AMYLOIDB[data$GROUP == "gene_exp1"])
shapiro_test_gene_exp2 <- shapiro.test(data$AMYLOIDB[data$GROUP == "gene_exp2"])

print(shapiro_test_control)
print(shapiro_test_gene_exp1)
print(shapiro_test_gene_exp2)

#QQ plot
ggplot(data, aes(sample = AMYLOIDB, color = GROUP)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~GROUP) +
  theme_minimal()

# 正态性不满足，使用Wilcoxon秩和检验
wilcox_test1 <- wilcox.test(AMYLOIDB ~ GROUP, data = data, subset = GROUP %in% c("control", "gene_exp1"))
print(wilcox_test1)

wilcox_test2 <- wilcox.test(AMYLOIDB ~ GROUP, data = data, subset = GROUP %in% c("control", "gene_exp2"))
print(wilcox_test2)

wilcox_test3 <- wilcox.test(AMYLOIDB ~ GROUP, data = data, subset = GROUP %in% c("gene_exp1", "gene_exp2"))
print(wilcox_test3)


# 以age为自变量，amyloidb为因变量，查看散点图
ggplot(data, aes(x = AGE, y = AMYLOIDB, color = GROUP)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Scatterplot of Amyloid Beta Levels by Age and Gene Expression Group", 
       x = "Age", y = "Amyloid Beta (pg/mL)")

# 以diagnosis_yrs为自变量，amyloidb为因变量，查看散点图
ggplot(data, aes(x = DIAGNOSIS_YRS, y = AMYLOIDB, color = GROUP)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Scatterplot of Amyloid Beta Levels by Years Since Diagnosis and Gene Expression Group", 
       x = "Years Since Diagnosis", y = "Amyloid Beta (pg/mL)")



# 运行两因素 ANOVA，Group 和 Age 作为因素
anova_result <- aov(AMYLOIDB ~ GROUP * AGE * DIAGNOSIS_YRS, data = data)
summary(anova_result)
