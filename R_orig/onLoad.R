# 依赖检测
if (!requireNamespace("httr", quietly = TRUE)) {
  stop("请先安装 httr：install.packages('httr')")
}

# 初始化结果变量
network_time_utc <- NULL

for (url in c("https://www.baidu.com", "https://www.google.com", "https://www.cloudflare.com")) {
  # 如果已成功获取时间，跳过后续 URL
  if (!is.null(network_time_utc)) next

  # 尝试连接当前 URL
  res <- try(httr::HEAD(url, httr::timeout(5)), silent = TRUE)
  if (inherits(res, "try-error")) next

  # 提取并解析 Date 头
  d <- httr::headers(res)[["date"]]
  if (!is.null(d)) {
    original_locale <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "C")
    network_time_utc <- as.Date(parse_date_time(d, orders = c("a, d b Y H:M:S", "Y-m-d H:M:S")))
    Sys.setlocale("LC_TIME", original_locale)
  }
}

# 输出结果
if (!is.null(network_time_utc)) {
  message("成功获取网络时间: ", network_time_utc)
} else {
  warning("所有 URL 均无法获取时间")
}

# 设置到期时间（UTC）
expiry_date <- as.Date("2025-12-31")

# 执行到期检查
if (network_time_utc > expiry_date) {
  stop("❌ License expired. Please downlod the latest packages.")
} else {
  message("✅ License check passed. Package loaded successfully.")
}
