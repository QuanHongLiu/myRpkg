# 检查输入
if (!is.data.frame(df)) stop("输入必须是 data.frame")

for (colname in names(df)) {
  x <- df[[colname]]
  labs <- attr(x, "labels")

  if (!is.null(labs)) {
    # 保存旧属性
    old_attrs <- attributes(x)

    # 构造新的 factor
    x <- factor(x,
                levels = names(labs),
                labels = as.character(labs))

    # 恢复原属性（除了 levels / class / labels）
    keep_attrs <- setdiff(names(old_attrs), c("levels", "class", "labels"))
    for (a in keep_attrs) {
      attr(x, a) <- old_attrs[[a]]
    }

    df[[colname]] <- x
    message("✅ 转换变量: ", colname)
  }
}
