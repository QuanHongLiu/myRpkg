# 参数验证
method <- match.arg(method)
action <- match.arg(action)
if (!is.numeric(x)) stop("输入x必须是数值向量")
if (method == "iqr" && k <= 0) stop("k必须为正数")

# 计算离群值边界
bounds <- switch(method,
                 "quantile" = {
                   c(quantile(x, probs = lower, na.rm = na.rm),
                     quantile(x, probs = upper, na.rm = na.rm))
                 },
                 "fixed" = {
                   c(lower, upper)
                 },
                 "iqr" = {
                   q <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
                   iqr <- q[2] - q[1]
                   c(q[1] - k * iqr, q[2] + k * iqr)
                 }
)

# 执行处理
if (action == "cap") {
  x[x < bounds[1] & !is.na(x)] <- bounds[1]
  x[x > bounds[2] & !is.na(x)] <- bounds[2]
} else {
  x[x < bounds[1] | x > bounds[2]] <- NA
}
