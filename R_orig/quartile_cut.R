# 1. 捕获传入的符号并转为字符串（得到的是“传参的名字”）
var_sym <- rlang::ensym(var_name)
var_name_str <- rlang::as_string(var_sym)

# 2. 如果调用环境里存在这个名字，并且该对象是长度为1的字符向量，
#    则把 var_name_str 替换为该对象的值（也就是支持 x <- "wbc" 的情况）
if (exists(var_name_str, envir = parent.frame(), inherits = TRUE)) {
  possible_val <- get(var_name_str, envir = parent.frame(), inherits = TRUE)
  if (is.character(possible_val) && length(possible_val) == 1) {
    var_name_str <- possible_val
  }
}

# 检查 n 是否有效
if (!is.numeric(n) || length(n) != 1 || n < 0) {
  stop("n must be a positive integer")
}

# 如果 n = 0，直接返回原始数据
if (n == 0) {
  return(dataframe)
}

# 检查输入是否为数据框
if (!is.data.frame(dataframe)) {
  stop("The input 'dataframe' must be a data frame.")
}

# 检查变量是否存在且为数值型
var <- dataframe[[var_name_str]]
if (is.null(var)) {
  stop("Variable '", var_name_str, "' not found in dataframe")
}
if (!is.numeric(var)) {
  stop("Variable '", var_name_str, "' must be numeric")
}

# 计算唯一分位数
quantiles <- unique(quantile(var, probs = seq(0, 1, 1/n), na.rm = TRUE))
final_n <- length(quantiles) - 1

if (final_n < n) {
  warning("Only ", final_n, " unique quantile groups created due to ties in the data")
}

# 创建分组变量
new_var_name <- paste0(var_name_str, final_n)
dataframe[[new_var_name]] <- cut(
  var,
  breaks = quantiles,
  include.lowest = TRUE,
  labels = if (reverse) as.character(final_n:1) else as.character(1:final_n)
)
