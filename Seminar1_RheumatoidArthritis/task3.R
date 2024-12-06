# 加载数据
setwd("E:/桌面/KTH/Year1_KTH/Statistics for Medical Engineering/Seminar1")
data <- read.csv("data_task3_crp.csv")
plac <- data$crp_placebo
int1 <- data$crp_intervention_1
int2 <- data$crp_intervention_2

# 加载tidyr包
#library(tidyr)

# 将数据从宽格式转换为长格式
#data_long <- gather(data, key = "group", value = "crp_value", crp_placebo, crp_intervention_1, crp_intervention_2)

# 查看转换后的长格式数据
#head(data_long)

#Shapiro-Wilk test
shap_plac <- shapiro.test(plac)
shap_int1 <- shapiro.test(int1)
shap_int2 <- shapiro.test(int2)
print(shap_plac)
print(shap_int1)
print(shap_int2)

# 将数据合并为一个数据框
crp_data <- data.frame(
  group = factor(rep(c("Placebo", "Intervention_1", "Intervention_2"), each = 30)),
  crp = c(plac, int1, int2)
)

# 进行 Kruskal-Wallis 检验
kruskal.test(crp ~ group, data = crp_data)

pairwise.wilcox.test(crp_data$crp, crp_data$group, p.adjust.method = "bonferroni")

mean(plac)
mean(int2)

