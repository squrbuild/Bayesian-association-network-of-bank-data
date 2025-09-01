# 步骤1: 加载所有必需的库
install.packages("igraph") # 如果您尚未安装，请务必先运行此行
library(igraph)
library(bnlearn)
library(dplyr)
library(stringr)
library(readr)
setwd("C:/Users/华硕/Desktop/study/毕业论文/code")

# 步骤2: 加载数据文件
cat("--- 1. 加载数据文件 ---\n")
if (!file.exists("rules_success.csv") || !file.exists("prepared_bank_data.csv")) {
  stop("错误：请确保 rules_success.csv 和 prepared_bank_data.csv 都在您的工作目录中！")
}
rules_df <- read_csv("rules_success.csv")
bank_data_full <- read_csv("prepared_bank_data.csv")
cat("所有数据文件加载成功。\n\n")


# 步骤3: 从规则文件中自动提取所有前项变量
cat("--- 2. 从 rules_success.csv 中自动解析变量 ---\n")
lhs_strings <- sub(" => .*", "", rules_df$rules)
cleaned_lhs <- gsub("[\\{\\}]", "", lhs_strings)
items_list <- strsplit(cleaned_lhs, ",")
all_items <- unlist(items_list)
variable_names <- sub("=.*", "", all_items)
unique_antecedent_vars <- unique(variable_names)
cat("成功解析出原始变量名。\n\n")


# 步骤4: 创建名称映射表并重命名 (这是之前成功的关键步骤，我们保留它)
cat("--- 3. 创建名称对应表并准备最终数据 ---\n")
name_map <- data.frame(
  original_name = unique_antecedent_vars,
  new_name = paste0("Var", seq_along(unique_antecedent_vars))
)

data_for_bn_intermediate <- bank_data_full %>%
  select(y, all_of(name_map$original_name))

names(data_for_bn_intermediate)[-1] <- name_map$new_name
data_for_bn_intermediate <- data_for_bn_intermediate %>%
  rename(y_1 = y)

data_for_bn_intermediate$y_1 <- factor(data_for_bn_intermediate$y_1, levels = c("0", "1"))
data_for_bn_intermediate <- data_for_bn_intermediate %>%
  mutate(across(all_of(name_map$new_name), as.factor))

data_for_bn <- as.data.frame(data_for_bn_intermediate)
cat("数据准备完成。\n\n")


# 步骤5: 构建贝叶斯网络 (这部分已经验证是成功的)
cat("--- 4. 构建贝叶斯网络 ---\n")
bn_structure <- hc(data_for_bn, score = "bic")
bn_model <- bn.fit(bn_structure, data = data_for_bn)
cat("模型构建成功！\n\n")


# 步骤6: 使用 igraph 进行可视化 (优化布局、增大尺寸并改为英文)
cat("--- 5. 使用 igraph 包生成网络图 (优化布局) ---\n")
# 将 bnlearn 的网络结构转换为 igraph 对象
graph_obj <- as.igraph(bn_structure)

# 设置节点颜色
V(graph_obj)$color <- "skyblue"
if ("y_1" %in% V(graph_obj)$name) {
  V(graph_obj)$color[V(graph_obj)$name == "y_1"] <- "lightcoral"
}

# 1. 开启一个PNG图形设备，指定文件名和高分辨率尺寸
#    您可以根据需要调整 width 和 height 的数值
png("bayesian_network_plot.png", width = 1600, height = 1200, res = 150)

plot(
  graph_obj,
  layout = layout_as_star,       # 您选择的布局算法
  vertex.label.color = "black",   
  vertex.label.cex = 0.9,         # 调整节点标签文字大小
  vertex.size = 30,               # 增大节点尺寸
  edge.arrow.size = 0.5,          
  main = "Bayesian Network for Bank Marketing (Optimized Layout)" # 主标题改为英文
)

# 3. 添加英文图例
legend(
  "topright", 
  legend = c("Target Variable (Marketing Outcome)", "Influencing Factors"), # 图例改为英文
  fill = c("lightcoral", "skyblue"), 
  bty = "n",
  cex = 0.8 # 调整图例文字大小
)

# 4. 关闭图形设备，这将完成文件的保存
dev.off()

cat("\n--- 图形已成功保存 --- \n")
cat("一张尺寸更大、分辨率更高的网络图已保存到您的工作目录中，文件名为: bayesian_network_plot.png\n")


# 步骤7: 显示名称对应表以供解读
cat("\n\n--- 节点名称对应表 ---\n")
cat("请使用下表来解读网络图中的 VarN 节点:\n")
print(name_map)
cat("\n--- 脚本执行完毕 ---\n")

