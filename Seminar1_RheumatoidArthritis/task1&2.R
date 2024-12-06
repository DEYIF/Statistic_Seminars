##Task1
setwd("E:/桌面/KTH/Year1_KTH/Statistics for Medical Engineering/Seminar1")
#import data
data <- read.csv("data_task1.csv")
head(data)
placebo <- data$placebo
interve <- data$interve

#Shapiro-Wilk test
shap_pla <- shapiro.test(placebo)
shap_int <- shapiro.test(interve)
print(shap_pla)
print(shap_int)


library(ggplot2)
ggplot(data, aes(sample = placebo)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "QQ plot for placebo",
       x = "norm Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

ggplot(data, aes(sample = interve)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "QQ plot for interve",
       x = "norm Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

#F-test
var.test(placebo, interve)
#t-test
t_test_result <- t.test(placebo, interve, var.equal = TRUE)
print(t_test_result)

##Task2
mean_placebo <- mean(placebo)
sd_placebo <- sd(placebo)

mean_interve <- mean(interve)
sd_interve <- sd(interve)

#sample size
n_new <- 10

#generate samples
new_placebo <- rnorm(n_new, mean = mean_placebo, sd = sd_placebo)
new_interve <- rnorm(n_new, mean = mean_interve, sd = sd_interve)

new_t_test_result <- pwr.t.test(new_placebo, new_interve, var.equal = TRUE)

library(pwr)
# Assume Cohen's d = 0.5, alpha = 0.05
effect_size <- 0.5
alpha <- 0.05

sample_sizes <- seq(10, 200, by = 10)
powers <- sapply(sample_sizes, function(n) {
  pwr.t.test(n = n, d = effect_size, sig.level = alpha, type = "two.sample", alternative = "two.sided")$power
})

plot(sample_sizes, powers, type = "b", xlab = "Sample Size (per group)", ylab = "Power",
     main = "Power vs. Sample Size")


# 创建样本量和效应量组合的功效表格
sample_sizes <- seq(10, 200, by = 10)
effect_sizes <- c(0.1, 0.3, 0.5, 0.7, 0.9)

power_matrix <- outer(sample_sizes, effect_sizes, function(n, d) {
  pwr.t.test(n = n, d = d, sig.level = alpha, type = "two.sample", alternative = "two.sided")$power
})

# 转换为数据框并绘制图形
power_df <- data.frame(Sample_Size = rep(sample_sizes, times = length(effect_sizes)),
                       Effect_Size = rep(effect_sizes, each = length(sample_sizes)),
                       Power = as.vector(power_matrix))

library(ggplot2)
ggplot(power_df, aes(x = Sample_Size, y = Power, color = as.factor(Effect_Size))) +
  geom_line() +
  labs(x = "Sample Size (per group)", y = "Power", color = "Effect Size",
       title = "Power vs. Sample Size for Different Effect Sizes") +
  theme_minimal()


