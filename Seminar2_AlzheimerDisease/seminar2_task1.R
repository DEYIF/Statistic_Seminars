library(ggplot2)
library(gridExtra)
library(tidyr)
library(reshape2)

setwd("D:/Code/R_code/seminar2")
data <- read.csv("data_task1.csv")

#######################################################
# histogram
plot_memory <- ggplot(data, aes(x = MEMORY, fill = as.factor(N_LABEL))) +
  geom_histogram(position = "identity", bins = 20, alpha = 0.5) + 
  labs(title = "MEMORY Distribution", x = "MEMORY", fill = "N_LABEL") +
  theme_minimal()

plot_execfunc <- ggplot(data, aes(x = EXECFUNC, fill = as.factor(N_LABEL))) +
  geom_histogram(position = "identity", bins = 20, alpha = 0.5) + 
  labs(title = "EXECFUNC Distribution", x = "EXECFUNC", fill = "N_LABEL") +
  theme_minimal()

plot_procspeed <- ggplot(data, aes(x = PROCSPEED, fill = as.factor(N_LABEL))) +
  geom_histogram(position = "identity", bins = 20, alpha = 0.5) + 
  labs(title = "PROCSPEED Distribution", x = "PROCSPEED", fill = "N_LABEL") +
  theme_minimal()

p <- grid.arrange(plot_memory, plot_execfunc, plot_procspeed, ncol = 3)

ggsave("Histogram.jpg", plot = p, 
       width = 3200/300, height = 1200/300, units = "in", dpi = 300)

print(p)


#######################################################
# boxplot
plot_memory <- ggplot(data, aes(x = as.factor(N_LABEL), y = MEMORY, fill = as.factor(N_LABEL))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "MEMORY", x = "N_LABEL", y = "MEMORY", fill = "N_LABEL") +
  theme_minimal()

plot_execfunc <- ggplot(data, aes(x = as.factor(N_LABEL), y = EXECFUNC, fill = as.factor(N_LABEL))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "EXECFUNC", x = "N_LABEL", y = "EXECFUNC", fill = "N_LABEL") +
  theme_minimal()

plot_procspeed <- ggplot(data, aes(x = as.factor(N_LABEL), y = PROCSPEED, fill = as.factor(N_LABEL))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "PROCSPEED", x = "N_LABEL", y = "PROCSPEED", fill = "N_LABEL") +
  theme_minimal()

p <- grid.arrange(plot_memory, plot_execfunc, plot_procspeed, ncol = 3)

ggsave("Boxplot.jpg", plot = p, 
       width = 3200/300, height = 1200/300, units = "in", dpi = 300)

print(p)


#######################################################
# try sum of these three value above, nothing happened
data$total_score <- rowSums(data[, c("MEMORY", "EXECFUNC", "PROCSPEED")], na.rm = TRUE)

plot_total <- ggplot(data, aes(x = total_score, fill = as.factor(N_LABEL))) +
  geom_histogram(position = "identity", bins = 20, alpha = 0.5) + 
  labs(title = "Total Score Distribution", x = "Total Score", fill = "N_LABEL") +
  theme_minimal()

print(plot_total)


#######################################################
# chi-squared test
data$N_LABEL <- as.factor(data$N_LABEL)

variables <- c("SEX", "HYPERTENSION", "DIABETES", "ATRIALFIBR", "INFARCTION", "AMYLOIDVIS")

for (var in variables) {
  contingency_table <- table(data$N_LABEL, data[[var]])
  chisq_result <- chisq.test(contingency_table)

  cat("Contingency table for", var, ":\n")
  print(contingency_table)
  cat("\n")
  
  chisq_result <- chisq.test(contingency_table)
  
  cat("Chi-squared test for", var, ":\n")
  print(chisq_result)
  cat("\n")
}


#######################################################
# fisher test replacing the chi-squared test to deal with the small frequency
data$N_LABEL <- as.factor(data$N_LABEL)

variables <- c("SEX", "HYPERTENSION", "DIABETES", "ATRIALFIBR", "INFARCTION", "AMYLOIDVIS")

for (var in variables) {

  contingency_table <- table(data$N_LABEL, data[[var]])
  #cat("Contingency table for", var, ":\n")
  #print(contingency_table)
  #cat("\n")
  
  fisher_result <- fisher.test(contingency_table)
  
  cat("Fisher's exact test for", var, ":\n")
  print(fisher_result)
  cat("\n") 
}


#######################################################
# Wilcoxon rank-sum test
variables <- c("AGE", "EDUCATION", "MMSE", "CERAD", "PASTCAQ", "HANDGRIP", "MUSCLE", "SPPB", 
               "MNA", "BMI", "CRP", "LEUK", "BPSYS", "BPDIA", "HBA1C", "CHOLESTEROL", 
               "WMHVOL", "HIPPOVOL", "AMYLOIDBIND")

results <- data.frame(Variable = character(), p.value = numeric(), stringsAsFactors = FALSE)

for (var in variables) {
  test_result <- wilcox.test(data[[var]] ~ data$N_LABEL)
  results <- rbind(results, data.frame(Variable = var, p.value = test_result$p.value))
}
print(results)


#######################################################
# Multiple Testing Correction
results <- data.frame(
  Variable = c("AGE", "EDUCATION", "MMSE", "CERAD", "PASTCAQ", "HANDGRIP", "MUSCLE",
               "SPPB", "MNA", "BMI", "CRP", "LEUK", "BPSYS", "BPDIA", "HBA1C",
               "CHOLESTEROL", "WMHVOL", "HIPPOVOL", "AMYLOIDBIND"),
  p.value = c(1.937940e-05, 1.856402e-01, 1.162747e-12, 5.055813e-14, 8.234381e-01,
              1.122971e-04, 5.138574e-01, 7.370349e-02, 4.660691e-04, 5.860135e-01,
              5.972583e-02, 3.994737e-02, 5.394276e-01, 5.213018e-01, 1.624442e-01,
              2.614635e-01, 1.560449e-02, 5.087277e-06, 3.661085e-04)
)

# cal Bonferroni corrections
results$Bonferroni_p <- p.adjust(results$p.value, method = "bonferroni")

# cal BH corrections
results$BH_p <- p.adjust(results$p.value, method = "BH")

results$Significant_Bonferroni <- results$Bonferroni_p < 0.05
results$Significant_BH <- results$BH_p < 0.05

print(results)

#######################################################
# Visiualization
results <- data.frame(
  Variable = c("AGE", "EDUCATION", "MMSE", "CERAD", "PASTCAQ", "HANDGRIP", "MUSCLE",
               "SPPB", "MNA", "BMI", "CRP", "LEUK", "BPSYS", "BPDIA", "HBA1C",
               "CHOLESTEROL", "WMHVOL", "HIPPOVOL", "AMYLOIDBIND"),
  P_value = c(1.937940e-05, 1.856402e-01, 1.162747e-12, 5.055813e-14, 8.234381e-01,
              1.122971e-04, 5.138574e-01, 7.370349e-02, 4.660691e-04, 5.860135e-01,
              5.972583e-02, 3.994737e-02, 5.394276e-01, 5.213018e-01, 1.624442e-01,
              2.614635e-01, 1.560449e-02, 5.087277e-06, 3.661085e-04),
  Bonferroni_p = c(3.682086e-04, 1.000000e+00, 2.209219e-11, 9.606045e-13, 1.000000e+00,
                   2.133645e-03, 1.000000e+00, 1.000000e+00, 8.855313e-03, 1.000000e+00,
                   1.000000e+00, 7.590000e-01, 1.000000e+00, 1.000000e+00, 1.000000e+00,
                   1.000000e+00, 2.964853e-01, 9.665826e-05, 6.956061e-03),
  BH_p = c(9.205215e-05, 2.713203e-01, 1.104610e-11, 9.606045e-13, 8.234381e-01,
           4.267290e-04, 6.028897e-01, 1.273060e-01, 1.265045e-03, 6.185698e-01,
           1.134791e-01, 8.433334e-02, 6.028897e-01, 6.028897e-01, 2.572033e-01,
           3.548433e-01, 3.706066e-02, 3.221942e-05, 1.159344e-03)
)

results_melted <- melt(results, id.vars = "Variable", variable.name = "Type", value.name = "P_value")

results_melted$Significant <- ifelse(results_melted$P_value < 0.05, "Significant", "Not Significant")

ggplot(results_melted, aes(x = Type, y = Variable, fill = Significant)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("Significant" = "#90EE90", "Not Significant" = "#FF6347")) +
  geom_text(aes(label = sprintf("%.2e", P_value)), color = "black", size = 6) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 18),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16)) +
  labs(title = "P-values with Bonferroni and BH Adjustments")


#######################################################
# significant variables box plots
variables <- c("AGE", "MMSE", "CERAD", "HANDGRIP", "MNA", "LEUK", "WMHVOL", "HIPPOVOL", "AMYLOIDBIND")

plots <- list()
# 创建 boxplot
for (var in variables) {
  p <- ggplot(data, aes(x = as.factor(N_LABEL), y = get(var), fill = as.factor(N_LABEL))) +
    geom_boxplot() +
    labs(title = var, x = "N_LABEL", y = var) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), # 标题居中
          axis.text = element_text(size = 10),    # 坐标轴标签字体大小
          axis.title = element_text(size = 14),   # 坐标轴标题字体大小
          legend.position = "none")               # 隐藏图例
  plots[[var]] <- p
}

# 保存图片并调整尺寸
png("boxplot_grid.png", width = 12, height = 18, units = "in", res = 300) # 设置图像大小和分辨率
grid.arrange(grobs = plots, ncol = 3)
dev.off()


#######################################################
# unsignificant variables box plots
variables_non_significant <- c("EDUCATION", "PASTCAQ", "MUSCLE", "SPPB", "BMI", "CRP", "BPSYS", "BPDIA", "HBA1C", "CHOLESTEROL")

# 创建 boxplot
plots_non_significant <- lapply(variables_non_significant, function(var) {
  ggplot(data, aes(x = as.factor(N_LABEL), y = get(var), fill = as.factor(N_LABEL))) +
    geom_boxplot() +
    labs(title = var, x = "N_LABEL", y = var) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), # 标题居中
          axis.text = element_text(size = 10),    # 坐标轴标签字体大小
          axis.title = element_text(size = 14),   # 坐标轴标题字体大小
          legend.position = "none")                # 隐藏图例
})

# 保存图片并调整尺寸
png("non_significant_boxplot_grid.png", width = 12, height = 24, units = "in", res = 300) # 设置图像大小和分辨率
grid.arrange(grobs = plots_non_significant, ncol = 3, nrow = 4) # 指定 4 行
dev.off()


#######################################################
# Correlation Matrix
library(corrplot)
library(reshape2)
library(ggplot2)

data_subset <- data[, !(names(data) %in% c("SEX", "HYPERTENSION", "DIABETES", 
                                            "ATRIALFIBR", "INFARCTION", 
                                            "AMYLOIDVIS", "N_LABEL", 
                                            "MEMORY", "EXECFUNC", "PROCSPEED"))]

cor_matrix <- cor(data_subset)

print(cor_matrix)

jpeg("correlation_matrix.jpg", width = 1200, height = 1200, units = "px", quality = 300)
par(oma = c(4, 4, 4, 4))
p_correlation <- corrplot(cor_matrix, 
         method = "color", 
         tl.cex = 1.5, 
         number.cex = 1.7, 
         addCoef.col = "black", 
         cl.pos = "n", 
         tl.col = "black",
         tl.pos = "t")
dev.off()

cor_matrix_melted <- melt(cor_matrix, varnames = c("Variable1", "Variable2"))

ggplot(cor_matrix_melted, aes(x = Variable1, y = Variable2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  geom_text(aes(label = sprintf("%.2f", value)), vjust = 1, color = "black", size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "Correlation Matrix") +
  coord_fixed()


#######################################################
# Linear regression
# 对MEMORY分数进行线性回归
model_memory <- lm(MEMORY ~ N_LABEL + AGE + SEX + EDUCATION + MMSE + CERAD + HYPERTENSION + DIABETES + ATRIALFIBR + INFARCTION +
                   AMYLOIDVIS + PASTCAQ + HANDGRIP + MUSCLE + SPPB + MNA + BMI + CRP + LEUK + BPSYS + BPDIA +
                   HBA1C + CHOLESTEROL + WMHVOL + HIPPOVOL + AMYLOIDBIND, data = data)
summary(model_memory)

# 对EXECFUNC分数进行线性回归
model_execfunc <- lm(EXECFUNC ~ N_LABEL + AGE + SEX + EDUCATION + MMSE + CERAD + HYPERTENSION + DIABETES + ATRIALFIBR + INFARCTION +
                     AMYLOIDVIS + PASTCAQ + HANDGRIP + MUSCLE + SPPB + MNA + BMI + CRP + LEUK + BPSYS + BPDIA +
                     HBA1C + CHOLESTEROL + WMHVOL + HIPPOVOL + AMYLOIDBIND, data = data)
summary(model_execfunc)

# 对PROCSPEED分数进行线性回归
model_procspeed <- lm(PROCSPEED ~ N_LABEL + AGE + SEX + EDUCATION + MMSE + CERAD + HYPERTENSION + DIABETES + ATRIALFIBR + INFARCTION +
                      AMYLOIDVIS + PASTCAQ + HANDGRIP + MUSCLE + SPPB + MNA + BMI + CRP + LEUK + BPSYS + BPDIA +
                      HBA1C + CHOLESTEROL + WMHVOL + HIPPOVOL + AMYLOIDBIND, data = data)
summary(model_procspeed)



#######################################################
# Linear regression without irrelated bool variables
# 对MEMORY分数进行线性回归
model_memory <- lm(MEMORY ~ MUSCLE + SPPB + AGE + MMSE + CERAD +  HANDGRIP + MNA + LEUK +  WMHVOL + HIPPOVOL + AMYLOIDBIND + ATRIALFIBR, data = data)
summary(model_memory)

# 对EXECFUNC分数进行线性回归
model_execfunc <- lm(EXECFUNC ~ AGE + MMSE + CERAD +  HANDGRIP + MNA + LEUK +  WMHVOL + HIPPOVOL + AMYLOIDBIND + ATRIALFIBR, data = data)
summary(model_execfunc)

# 对PROCSPEED分数进行线性回归
model_procspeed <- lm(PROCSPEED ~ AGE + MMSE + CERAD +  HANDGRIP + MNA + LEUK +  WMHVOL + HIPPOVOL + AMYLOIDBIND + ATRIALFIBR, data = data)
summary(model_procspeed)



#######################################################
# AIC
full_model_memory <- lm(MEMORY ~ N_LABEL + AGE + EDUCATION + MMSE + CERAD + PASTCAQ + HANDGRIP + 
                 MUSCLE + SPPB + MNA + BMI + CRP + LEUK + BPSYS + BPDIA + HBA1C + 
                 CHOLESTEROL + WMHVOL + HIPPOVOL + AMYLOIDBIND, data = data)

optimized_model_memory <- step(full_model_memory, direction = "both", trace = FALSE)

summary(optimized_model_memory)

full_model_execfunc <- lm(EXECFUNC ~ N_LABEL + AGE + EDUCATION + MMSE + CERAD + PASTCAQ + HANDGRIP + 
                 MUSCLE + SPPB + MNA + BMI + CRP + LEUK + BPSYS + BPDIA + HBA1C + 
                 CHOLESTEROL + WMHVOL + HIPPOVOL + AMYLOIDBIND, data = data)

optimized_model_execfunc <- step(full_model_execfunc, direction = "both", trace = FALSE)

summary(optimized_model_execfunc)

full_model_procspeed <- lm(PROCSPEED ~ N_LABEL + AGE + EDUCATION + MMSE + CERAD + PASTCAQ + HANDGRIP + 
                 MUSCLE + SPPB + MNA + BMI + CRP + LEUK + BPSYS + BPDIA + HBA1C + 
                 CHOLESTEROL + WMHVOL + HIPPOVOL + AMYLOIDBIND, data = data)

optimized_model_procspeed <- step(full_model_procspeed, direction = "both", trace = FALSE)

summary(optimized_model_procspeed)


#######################################################
# Linear regression without irrelated bool variables
# 对MEMORY分数进行线性回归

model_memory <- lm(MEMORY ~ MUSCLE + SPPB + MNA + HBA1C + WMHVOL + HIPPOVOL + AMYLOIDBIND, data = data)
summary(model_memory)

# 对EXECFUNC分数进行线性回归
model_execfunc <- lm(EXECFUNC ~ MUSCLE + SPPB + MNA + WMHVOL + HIPPOVOL, data = data)
summary(model_execfunc)


# 对PROCSPEED分数进行线性回归
model_procspeed <- lm(PROCSPEED ~ N_LABEL + CERAD + SPPB + MNA + BMI + CRP + LEUK + WMHVOL + HIPPOVOL + AMYLOIDBIND, data = data)
summary(model_procspeed)


#######################################################
# AIC again
full_model_memory <- lm(MEMORY ~ MUSCLE + SPPB + MNA + HBA1C + WMHVOL + HIPPOVOL + AMYLOIDBIND, data = data)

optimized_model_memory <- step(full_model_memory, direction = "both", trace = FALSE)

summary(optimized_model_memory)

full_model_execfunc <- lm(EXECFUNC ~ MUSCLE + SPPB + MNA + WMHVOL + HIPPOVOL, data = data)

optimized_model_execfunc <- step(full_model_execfunc, direction = "both", trace = FALSE)

summary(optimized_model_execfunc)

full_model_procspeed <- lm(PROCSPEED ~ N_LABEL + CERAD + SPPB + MNA + BMI + CRP + LEUK + WMHVOL + HIPPOVOL + AMYLOIDBIND, data = data)

optimized_model_procspeed <- step(full_model_procspeed, direction = "both", trace = FALSE)

summary(optimized_model_procspeed)



# 将数据按N_LABEL分组
normal_data <- subset(data, N_LABEL == 0)
impaired_data <- subset(data, N_LABEL == 1)

# 对认知正常组进行模型优化
full_model_memory_normal <- lm(MEMORY ~ AGE + EDUCATION + MMSE + CERAD + PASTCAQ + HANDGRIP + 
                 MUSCLE + SPPB + MNA + BMI + CRP + LEUK + BPSYS + BPDIA + HBA1C + 
                 CHOLESTEROL + WMHVOL + HIPPOVOL + AMYLOIDBIND, data = normal_data)

optimized_model_memory_normal <- step(full_model_memory_normal, direction = "both", trace = FALSE)
summary(optimized_model_memory_normal)

# 对认知受损组进行模型优化
full_model_memory_impaired <- lm(MEMORY ~ AGE + EDUCATION + MMSE + CERAD + PASTCAQ + HANDGRIP + 
                 MUSCLE + SPPB + MNA + BMI + CRP + LEUK + BPSYS + BPDIA + HBA1C + 
                 CHOLESTEROL + WMHVOL + HIPPOVOL + AMYLOIDBIND, data = impaired_data)

optimized_model_memory_impaired <- step(full_model_memory_impaired, direction = "both", trace = FALSE)
summary(optimized_model_memory_impaired)

# 对EXECFUNC模型执行同样的操作

# 认知正常组
full_model_execfunc_normal <- lm(EXECFUNC ~ AGE + EDUCATION + MMSE + CERAD + PASTCAQ + HANDGRIP + 
                 MUSCLE + SPPB + MNA + BMI + CRP + LEUK + BPSYS + BPDIA + HBA1C + 
                 CHOLESTEROL + WMHVOL + HIPPOVOL + AMYLOIDBIND, data = normal_data)

optimized_model_execfunc_normal <- step(full_model_execfunc_normal, direction = "both", trace = FALSE)
summary(optimized_model_execfunc_normal)

# 认知受损组
full_model_execfunc_impaired <- lm(EXECFUNC ~ AGE + EDUCATION + MMSE + CERAD + PASTCAQ + HANDGRIP + 
                 MUSCLE + SPPB + MNA + BMI + CRP + LEUK + BPSYS + BPDIA + HBA1C + 
                 CHOLESTEROL + WMHVOL + HIPPOVOL + AMYLOIDBIND, data = impaired_data)

optimized_model_execfunc_impaired <- step(full_model_execfunc_impaired, direction = "both", trace = FALSE)
summary(optimized_model_execfunc_impaired)

# 对PROCSPEED模型执行同样的操作

# 认知正常组
full_model_procspeed_normal <- lm(PROCSPEED ~ AGE + EDUCATION + MMSE + CERAD + PASTCAQ + HANDGRIP + 
                 MUSCLE + SPPB + MNA + BMI + CRP + LEUK + BPSYS + BPDIA + HBA1C + 
                 CHOLESTEROL + WMHVOL + HIPPOVOL + AMYLOIDBIND, data = normal_data)

optimized_model_procspeed_normal <- step(full_model_procspeed_normal, direction = "both", trace = FALSE)
summary(optimized_model_procspeed_normal)

# 认知受损组
full_model_procspeed_impaired <- lm(PROCSPEED ~ AGE + EDUCATION + MMSE + CERAD + PASTCAQ + HANDGRIP + 
                 MUSCLE + SPPB + MNA + BMI + CRP + LEUK + BPSYS + BPDIA + HBA1C + 
                 CHOLESTEROL + WMHVOL + HIPPOVOL + AMYLOIDBIND, data = impaired_data)

optimized_model_procspeed_impaired <- step(full_model_procspeed_impaired, direction = "both", trace = FALSE)
summary(optimized_model_procspeed_impaired)
