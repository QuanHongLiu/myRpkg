.onLoad <- function(libname, pkgname) {
  # 依赖检测
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("请先安装 httr：install.packages('httr')")
  }

  # 定义网络时间获取函数（严格模式）
  get_network_time_multi <- function(
    urls = c("https://www.baidu.com", "https://www.google.com", "https://www.cloudflare.com")
  ) {
    for (url in urls) {
      # message("尝试连接: ", url, " ...")
      res <- try(httr::HEAD(url, httr::timeout(5)), silent = TRUE)
      if (inherits(res, "try-error")) next

      d <- httr::headers(res)[["date"]]
      if (!is.null(d)) {
        original_locale <- Sys.getlocale("LC_TIME")
        Sys.setlocale("LC_TIME", "C")
        t_utc <- as.Date(parse_date_time(d, orders = c("a, d b Y H:M:S", "Y-m-d H:M:S")))
        Sys.setlocale("LC_TIME", original_locale)
        # 注意：此方法要求字符串中不包含星期几（需手动去掉 "Mon, "）
        return(t_utc)
      }
    }
    # 如果所有都失败，直接报错
    stop("❌ 无法从任何网站获取网络时间。请检查网络连接或防火墙设置。")
  }

  # 获取网络时间
  network_time_utc <- get_network_time_multi()

  # 设置到期时间（UTC）
  expiry_date <- as.Date("2025-12-03")

  # 执行到期检查
  if (network_time_utc > expiry_date) {
    stop("❌ License expired. Please downlod the latest packages.")
  } else {
    # message("✅ License check passed. Package loaded successfully.")
  }
}






#' 将数据框中分类变量的原始数字level变为labels记录的level
#'
#'
#' @param df 需要处理的数据框
#'
#' @returns
#' @export
#'
#' @examples
convert_labels_to_levels <- function(df) {
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
                  levels = as.character(labs),
                  labels = names(labs))

      # 恢复原属性（除了 levels / class / labels）
      keep_attrs <- setdiff(names(old_attrs), c("levels", "class", "labels"))
      for (a in keep_attrs) {
        attr(x, a) <- old_attrs[[a]]
      }

      df[[colname]] <- x
      message("✅ 转换变量: ", colname)
    }
  }
  return(df)
}








#' 根据模型和自变量及因变量类型，给结果添加内容
#'
#' @param model_summary 需要添加内容的 summary 数据
#' @param model 模型
#' @param data 原始数据，用来给结果添加内容
#' @param x 自变量
#' @param y 因变量
#'
#' @returns
#' @export
#'
#' @examples
add_model_info <- function(model_summary,
                           model,
                           data,
                           x,
                           y) {
  # 小工具：提取前缀
  get_prefix <- function(var) {
    parts <- unlist(strsplit(var, "_"))  # 使用 unlist 将列表转换为向量

    # 如果第一个部分是 "death"，则取下一个部分
    if (parts[1] == "death") {
      # 确保有下一个部分可用
      if (length(parts) > 1) {
        return(parts[2])
      } else {
        # 如果没有下一个部分，则返回空字符串或NA（根据需求选择）
        return(NA)
      }
    } else {
      # 否则返回第一个部分
      return(parts[1])
    }
  }
  exp_model_type <- needs_exp(model)

  # ---- 情况 1: x 分类 + y 分类 ----
  if (exp_model_type$exponentiate & x %in% names(model$xlevels)) {
    print('add_model_info  情况 1: x 分类 + y 分类--------------')
    # 添加 reference 行
    df_ref <- data.frame(
      x = paste0(x,'0.0'), y = y,
      estimate = 1, conf.low = 1, conf.high = 1
    )

    model_summary <- model_summary %>%
      mutate(
        estimate = exp(estimate),
        conf.low = exp(conf.low),
        conf.high = exp(conf.high)
      ) %>%
      bind_rows(df_ref, .)

    # 针对 Cox 回归
    if (exp_model_type$model_type == 'coxph') {
      var_prefix <- get_prefix(y)
      event_var <- paste0(var_prefix, "_event2")
      time_var  <- paste0(var_prefix, "_time")

      if (event_var %in% names(data)) {
        tab <- as.data.frame.matrix(table(data[[x]], data[[event_var]]))
        model_summary$case    <- tab[,2]
        model_summary$control <- tab[,1]
      }
      if (time_var %in% names(data)) {
        model_summary$person_years <- as.data.frame(
          tapply(data[[time_var]], data[[x]], sum, na.rm = TRUE)
        )[,1]
      }

      event_var_death <- paste0('death_', var_prefix, "_event2")
      time_var_death  <- paste0('death_', var_prefix, "_time")

      if (event_var_death %in% names(data)) {
        tab <- as.data.frame.matrix(table(data[[x]], data[[event_var_death]]))
        model_summary$case    <- tab[,2]
        model_summary$control <- tab[,1]
      }
      if (time_var_death %in% names(data)) {
        model_summary$person_years <- as.data.frame(
          tapply(data[[time_var_death]], data[[x]], sum, na.rm = TRUE)
        )[,1]
      }
    }

    # 针对 Logistic 回归
    else if (exp_model_type$model_type == 'binomial') {
      tab <- as.data.frame.matrix(table(data[[x]], data[[y]]))
      model_summary$case    <- tab[,2]
      model_summary$control <- tab[,1]
    }
  }

  # ---- 情况 2: x 分类 + y 连续 ----
  else if (!exp_model_type$exponentiate & x %in% names(model$xlevels)) {
    print('add_model_info 情况 2: x 分类 + y 连续 --------------')
    df_ref <- data.frame(
      x = paste0(x,'0.0'), y = y,
      estimate = 0, conf.low = 0, conf.high = 0
    )
    model_summary <- bind_rows(df_ref, model_summary)
  }

  # ---- 情况 3: x 连续 + y 分类 ----
  else if (exp_model_type$exponentiate & !(x %in% names(model$xlevels))) {
    print('add_model_info 情况 3: x 连续 + y 分类 --------------')
    var_prefix <- get_prefix(y)

    model_summary <- model_summary %>%
      mutate(
        estimate = exp(estimate),
        conf.low = exp(conf.low),
        conf.high = exp(conf.high)
      )

    # 针对 Cox 回归
    if (exp_model_type$model_type == 'coxph') {
      event_var <- paste0(var_prefix, "_event2")
      time_var  <- paste0(var_prefix, "_time")

      if (event_var %in% names(data)) {
        tab <- as.data.frame(table(data[[event_var]]))
        model_summary$case    <- tab[2,2]   # y==1
        model_summary$control <- tab[1,2]   # y==0
      }
      if (time_var %in% names(data)) {
        model_summary$person_years <- sum(data[[time_var]], na.rm = TRUE)
      }

      event_var_death <- paste0('death_', var_prefix, "_event2")
      time_var_death  <- paste0('death_', var_prefix, "_time")

      if (event_var_death %in% names(data)) {
        tab <- as.data.frame(table(data[[event_var_death]]))
        model_summary$case    <- tab[2,2]   # y==1
        model_summary$control <- tab[1,2]   # y==0
      }
      if (time_var_death %in% names(data)) {
        model_summary$person_years <- sum(data[[time_var_death]], na.rm = TRUE)
      }

    }

    # 针对 Logistic 回归
    else if (exp_model_type$model_type == 'binomial') {
      tab <- as.data.frame(table(data[[y]]))
      model_summary$case    <- tab[2,2]
      model_summary$control <- tab[1,2]
    }
  }

  return(model_summary)
}



#' 将模型结果的列名统一为与 broom::tidy() 一致的格式
#'
#' @param df 需要统一的数据框
#'
#' @returns 处理好的数据框
#' @export
#'
#' @examples
standardize_tidy_names <- function(df) {
  # 定义匹配规则（关键词->目标列名）
  name_map <- list(
    estimate   = c("estimate", "est", "coef", "coefficient"),
    std.error  = c("std.error", "se", "standard error", "std error", 'Std. Error', 'se(coef)'),
    statistic  = c("t value", "z value", "f value", "statistic", "t.value", "z.value",'z'),
    p.value    = c("p.value", "pvalue", "p_value", "Pr(>|z|)", "Pr(>|t|)", "Pr(>|Chi|)"),
    conf.low   = c("2.5 %", "conf.low", "lower", "CI_low"),
    conf.high  = c("97.5 %", "conf.high", "upper", "CI_high")
  )

  # 获取原始列名（保留大小写）
  original_names <- colnames(df)

  # 遍历每个目标列名，精确匹配并替换
  for (target in names(name_map)) {
    # 检查原始列名是否在 name_map 的候选列表中（忽略大小写）
    matched_col <- original_names[tolower(original_names) %in% tolower(name_map[[target]])]
    if (length(matched_col) >= 1) {
      # 只替换第一个匹配的列（避免多列冲突）
      colnames(df)[original_names == matched_col[1]] <- target
    }
  }

  return(df)
}




#' 判断模型系数是否需要取指数（exp(β)）
#'
#' @param model 拟合的模型对象（如glm, coxph等）
#'
#' @returns 逻辑值（TRUE需要取指数，FALSE不需要）
#' @export
#'
#' @examples
needs_exp <- function(model) {
  # 初始化结果列表
  result <- list(
    exponentiate = FALSE,
    model_type = "unknown"
  )

  # 逻辑回归 → 计算OR
  if (inherits(model, "glm") && model$family$family == "binomial") {
    result$exponentiate <- TRUE
    result$model_type <- "binomial"
    return(result)
  }

  # 泊松回归 → 计算IRR
  if (inherits(model, "glm") && model$family$family == "poisson") {
    result$exponentiate <- TRUE
    result$model_type <- "poisson"
    return(result)
  }

  # Cox回归 → 计算HR
  if (inherits(model, "coxph")) {
    result$exponentiate <- TRUE
    result$model_type <- "coxph"
    return(result)
  }

  # 多项逻辑回归 → 计算OR
  if (inherits(model, "multinom")) {
    result$exponentiate <- TRUE
    result$model_type <- "multinomial logistic regression"
    return(result)
  }

  # 有序逻辑回归 → 计算OR
  if (inherits(model, "polr")) {
    result$exponentiate <- TRUE
    result$model_type <- "ordinal logistic regression"
    return(result)
  }

  # 线性回归
  if (inherits(model, "lm") && !inherits(model, "glm")) {
    exponentiate = FALSE
    result$model_type <- "lm"
    return(result)
  }

  # 其他情况
  result$model_type <- class(model)
  return(result)
}







#' 提取模型结果 confint 计算置信区间
#'
#' 提取模型的结果，包括 β、se、confint等等。该函数使用 confint 计算置信区间，当模型为混合模型时，耗时较长
#'
#' 该函数还可根据因变量类型，自动处理为 OR 或 HR
#'
#' @param x 模型自变量
#' @param y 因变量
#' @param model 模型名称；默认 model
#' @param results 收纳结果的数据框；默认 results
#' @param data 原始数据框，用给结果添加些额外内容
#'
#' @return 结果的数据框
#' @export
#'
#' @examples
extract_model_results_conf <- function(x, y, model = model, results = results, data = NULL) {
  # 提取模型系数
  model_summary <- summary(model)$coefficients
  model_summary <- as.data.frame(model_summary) %>% tibble::rownames_to_column(var = "x")
  model_summary <- cbind(y, model_summary)
  model_summary <- model_summary[grepl(x, model_summary$x), ]

  # 提取模型通过 bootstrap 得到的置信区间
  model_confint <- confint(model)
  model_confint <- as.data.frame(model_confint)
  model_confint <- model_confint[grepl(x, rownames(model_confint)),]

  # 合并结果
  res <- cbind(model_summary, model_confint)
  res <- standardize_tidy_names(res)

  # 如果需要exp(β)，转换系数和置信区间; 如果 x 是level 也要; 还可以用于添加case，control，person-years
  res <- add_model_info(model_summary = res,
                                  model = model,
                                  data = data,
                                  x = x,
                                  y = y)

  # 合并到 results
  if (is.data.frame(results)) {
    results <- bind_rows(results, res)
  } else {
    results <- data.frame()
    results <- bind_rows(results, res)
  }
  # 返回结果
  return(results)
}
# extract_model_results_conf <- function(x, y, model = model, results = results) {
#   # 判断是否需要取指数（exp(β)）
#   exponentiate <- needs_exp(model)$exponentiate
#
#   # 提取模型系数
#   model_summary <- broom.mixed::tidy(model, conf.int = FALSE, exponentiate = exponentiate)
#   model_summary <- cbind(y, model_summary)
#   model_summary <- model_summary[grepl(x, model_summary$term), ]
#   names(model_summary)[names(model_summary)=='term'] <- 'x'
#
#   # 提取模型通过 bootstrap 得到的置信区间
#   model_confint <- confint(model)
#   model_confint <- as.data.frame(model_confint)
#   model_confint <- model_confint[grepl(x, rownames(model_confint)),]
#
#   # 合并结果
#   res <- cbind(model_summary, model_confint)
#   res <- standardize_tidy_names(res)
#
#   # 合并到 results
#   if (is.data.frame(results)) {
#     results <- bind_rows(results, res)
#   } else {
#     results <- data.frame()
#     results <- bind_rows(results, res)
#   }
#   # 返回结果
#   return(results)
# }



#' 提取模型结果-用 wald 法计算置信区间
#'
#' 提取模型的结果，包括 β、se、confint等等。该函数使用 wald 计算置信区间，即直接使用 β ± 1.96se 的方式，耗时较短。
#'
#' 该函数还可根据因变量类型，自动处理为 OR 或 HR
#'
#' @param x 模型自变量
#' @param y 因变量
#' @param model 模型名称；默认 model
#' @param results 收纳结果的数据框；默认 results
#' @param data 原始数据框，用给结果添加些额外内容
#'
#' @return 结果的数据框
#' @export
#'
#' @examples
extract_model_results_wald <- function(x, y, model = model, results = results, data = NULL) {
  # 提取模型系数
  model_summary <- summary(model)$coefficients
  model_summary <- as.data.frame(model_summary) %>% tibble::rownames_to_column(var = "x")
  model_summary <- cbind(y, model_summary)
  model_summary <- model_summary[grepl(x, model_summary$x), ]
  model_summary <- standardize_tidy_names(model_summary)

  # 提取模型通过 β ± 1.96 × SE 得到的置信区间
  model_summary$conf.low <- model_summary$estimate - 1.96 * model_summary$std.error
  model_summary$conf.high <- model_summary$estimate + 1.96 * model_summary$std.error

  # 如果需要exp(β)，转换系数和置信区间; 如果 x 是level 也要; 还可以用于添加case，control，person-years
  model_summary <- add_model_info(model_summary = model_summary,
                                  model = model,
                                  data = data,
                                  x = x,
                                  y = y)

  # 合并到 results
  if (is.data.frame(results)) {
    results <- bind_rows(results, model_summary)
  } else {
    results <- data.frame()
    results <- bind_rows(results, model_summary)
  }
  # 返回结果
  return(results)
}
# extract_model_results_tidy <- function(x, y, model = model, results = results) {
#   # 判断是否需要取指数（exp(β)）
#   exponentiate <- needs_exp(model)$exponentiate
#
#   # 提取模型系数
#   model_summary <- broom.mixed::tidy(model, conf.int = TRUE, exponentiate = exponentiate)
#   model_summary <- cbind(y, model_summary)
#   model_summary <- model_summary[grepl(x, model_summary$term), ]
#   names(model_summary)[names(model_summary)=='term'] <- 'x'
#
#   if (is.data.frame(results)) {
#     results <- bind_rows(results, model_summary)
#   } else {
#     results <- data.frame()
#     results <- bind_rows(results, model_summary)
#   }
#   # 返回结果
#   return(results)
# }







#' 统一两个数据框的变量类型
#'
#' 将两个数据框相同名称的变量变为相同变量类型
#'
#' @param target_df 目标数据框
#' @param reference_df 参考数据框
#'
#' @return
#' @export
#'
#' @examples
harmonise_types <- function(target_df, reference_df) {
  # 获取reference_df的列类型
  ref_types <- sapply(reference_df, class)

  # 遍历target_df的每一列
  for (col in names(target_df)) {
    if (col %in% names(ref_types)) {
      # 根据reference_df中的类型转换
      if (ref_types[[col]] == "numeric") {
        target_df[[col]] <- as.numeric(target_df[[col]])
      } else if (ref_types[[col]] == "character") {
        target_df[[col]] <- as.character(target_df[[col]])
      } else if (ref_types[[col]] == "factor") {
        target_df[[col]] <- as.factor(target_df[[col]])
      } else if (ref_types[[col]] == "logical") {
        target_df[[col]] <- as.logical(target_df[[col]])
      } else if (ref_types[[col]] == "integer") {
        target_df[[col]] <- as.integer(target_df[[col]])
      }
    }
  }
  return(target_df)
}




#' 分割变量
#'
#' 分割变量为多分位，默认赋值为1：n
#'
#' @param dataframe 数据框
#' @param var_name 需要分割的变量
#' @param n 将变量分别 n 分位
#' @param reverse 是否分割后反向赋值
#'
#' @returns
#' @export
#'
#' @examples
quartile_cut <- function(dataframe, var_name, n, reverse = FALSE) {
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

  # 检查 n 是否有效
  if (!is.numeric(n) || length(n) != 1 || n < 0) {
    stop("n must be a positive integer")
  }

  # 如果 n = 0，直接返回原始数据
  if (n == 0) {
    return(dataframe)
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
  return(dataframe)
}




#' 输出 xlsx 文件
#'
#' 输出的 xlsx 文件会自动调整列宽
#'
#' @param x 需要写入表格的内容, 如果需要写入多个表格需提供 list，如 list(Sheet1 = results1, Sheet2 = results2)
#' @param file 输出文件的路径
#' @param row_height 行高；默认18
#' @param auto_width 列宽；自动
#' @param font 字体；默认 Arial（中文为微软雅黑）；还可选 Times New Roman（中文为宋体）
#' @param size 字体大小；默认 11
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
write_xlsx <- function(x,
                       file,
                       row_height = 18,
                       auto_width = TRUE,
                       font = "Arial", #  "Times New Roman",
                       size = 11,
                       ...) {
  wb <- openxlsx::createWorkbook()

  style <- openxlsx::createStyle(fontName = font, fontSize = size)

  if (!is.list(x) | is.data.frame(x)) {
    x <- as.data.frame(x)
    x <- list(Sheet1 = x)
  }

  for (nm in names(x)) {
    openxlsx::addWorksheet(wb, sheetName = nm)
    data <- x[[nm]]
    openxlsx::writeData(wb, sheet = nm, x = data, ...)
    openxlsx::addStyle(wb, nm, style = style, rows = 1:(nrow(data)+1), cols = 1:ncol(data), gridExpand = TRUE)
    openxlsx::setRowHeights(wb, nm, rows = 1:(nrow(data)+1), heights = row_height)
    if (auto_width) {
      openxlsx::setColWidths(wb, nm, cols = 1:ncol(data), widths = "auto")
    }
  }
  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
}



#' 前处理 ukb 数据
#'
#' 根据 ukb 的 data showcase，变更变量类型、添加变量标签
#'
#' @param data 需要处理的 ukb 数据框
#'
#' @returns
#' @export
#'
#' @examples
process_ukb_data <- function(data){
  # 0. 读取 data_showcase 文件
  data_showcase <- read_csv(paste0(system.file(package = 'myRpkg'),"/extdata/Data_Dictionary_Showcase.csv"))
  # 1. 读取 codings_showcase 文件
  codings_showcase <- read_csv(paste0(system.file(package = 'myRpkg'),"/extdata/Codings_Showcase.csv"))
  # 2. 更改变量类型
  var_type <- setNames(as.list(data_showcase$ValueType), data_showcase$FieldID)
  var_coding <- setNames(as.list(data_showcase$Coding), data_showcase$FieldID)

  data[] <- lapply(names(data), function(col_name) {
    field_id <-  strsplit(col_name, "_")[[1]][2]
    if (field_id %in% names(var_type)) {
      if (var_type[[field_id]] %in% c("Categorical multiple", "Categorical single")) {
        data[[col_name]] <- as.factor(data[[col_name]])
        coding_map <- codings_showcase[codings_showcase$coding_id == var_coding[[field_id]],]
        # 分类变量 label
        attr(data[[col_name]],"labels") <- setNames(coding_map$coding, coding_map$meaning)
        return(data[[col_name]])
      } else if (var_type[[field_id]] %in%  c("Integer", "Continuous")){
        return(as.numeric(data[[col_name]]))
      } else if (var_type[[field_id]] %in%  c("Date") && grepl("^\\d+$", data[[col_name]][1])){
        return(as.Date(data[[col_name]],origin = "1960-01-01"))
      } else if (var_type[[field_id]] %in%  c("Date") && !grepl("^\\d+$", data[[col_name]][1])){
        return(as.Date(data[[col_name]], format = "%d%b%Y"))
      } else if (var_type[[field_id]] %in%  c("Time") && grepl("^\\d+$", data[[col_name]][1])){
        return(as.POSIXct(data[[col_name]], origin="1960-01-01 00:00:00", tz="UTC"))
      } else if (var_type[[field_id]] %in%  c("Time") && !grepl("^\\d+$", data[[col_name]][1])){
        return(as.POSIXct(data[[col_name]], format = "%d%b%Y %H:%M:%S"))
      } else {
        return(data[[col_name]])
      }
    } else if (field_id %in% "eid"){
      return(as.numeric(data[[col_name]]))
    }
    return(data[[col_name]]) # 如果不满足条件，返回原列
  })

  # 2. 添加变量标签（精确匹配）
  var_labels <- setNames(
    as.list(paste0(data_showcase$Field,
                   ifelse(is.na(data_showcase$Units), "",
                          paste0(", ", data_showcase$Units)))),
    data_showcase$FieldID
  )

  data[] <- lapply(names(data), function(col_name) {
    field_id <- strsplit(col_name, "_")[[1]][2]

    if (field_id %in% names(var_labels)) {
      attr(data[[col_name]],"label") <- var_labels[[field_id]]
      return(data[[col_name]])
    } else if (field_id %in% "eid") {
      attr(data[[col_name]],"label") <- "Encoded anonymised participant ID"
      return(data[[col_name]])
    } else {
      attr(data[[col_name]],"label") <- attr(data[[col_name]],"label")
      return(data[[col_name]])
    }
  })
  return(data)
}




#' 生成提取 ukb 数据的 sas 代码
#'
#' 生成提取 ukb 数据的 sas 代码。需要的 ukb 数据的 field id list，然后生成 sas 代码
#'
#' @param field_list 所需的 ukb 变量的 list
#' @param output_dir_prefix 生成 sas 脚本路径前缀
#'
#' @returns
#' @export
#'
#' @examples
format_sas_code <- function(field_list,output_dir_prefix){
  # 提取用户需要的 filed 的详细信息
  Dictionary_Showcase <- read_csv(paste0(system.file(package = 'myRpkg'),"/extdata/Data_Dictionary_Showcase.csv"))
  Dictionary_Showcase <- Dictionary_Showcase[Dictionary_Showcase$FieldID %in% field_list, c("FieldID","Field","Field_zh","Notes_zh","ValueType","Units","Stability","Instances","Array")]
  write_xlsx(x = Dictionary_Showcase, file = paste0(output_dir_prefix,"_showcase.xlsx"))


  # 读取 UKB_variable_dictionary 文件
  data_showcase <- read_csv(paste0(system.file(package = 'myRpkg'),"/extdata/UKB_variable_dictionary.csv"))

  # 排除掉数据框里根本没有的变量
  data_showcase <- data_showcase[!is.na(data_showcase$Start_pos),]

  # 把所需的挑出来
  data_showcase <- data_showcase[sapply(strsplit(data_showcase$UDI, "-"), function(x) x[1]) %in% field_list,]

  print(paste0("请求提取 ",length(field_list),"个变量"))
  print(paste0("共有 ",sum(field_list %in% sapply(strsplit(data_showcase$UDI, "-"), function(x) x[1]))," 个变量在数据库中"))
  missing_vars <- field_list[!field_list %in% sapply(strsplit(data_showcase$UDI, "-"), function(x) x[1])]
  if (length(missing_vars) > 0) {
    cat(paste0("数据库不包含变量：", paste(missing_vars, collapse = ", "), "\n"))
  } else {
    cat("所有变量均存在。\n")
  }

  # 生成变量类型
  data_showcase$code <- paste0("@",data_showcase$Start_pos," ",data_showcase$Variable_name," ",data_showcase$Variable_Type)
  ukb_20201126 <- data_showcase$code[data_showcase$Download_date == "2020-11-26"]
  ukb_20201222 <- data_showcase$code[data_showcase$Download_date == "2020-12-22"]
  ukb_20211013 <- data_showcase$code[data_showcase$Download_date == "2021-10-13"]
  ukb_20220705 <- data_showcase$code[data_showcase$Download_date == "2022-07-05"]
  ukb_20250412 <- data_showcase$code[data_showcase$Download_date == "2025-04-12"]

  # 打印 meta 信息
  # 生成提取数据的 SAS 代码
  print("正在生成提取数据的 SAS 代码")
  # 重复的变量
  data_duplicated <- data_showcase[,c("UDI","Download_date","Count")]
  data_duplicated <- data_duplicated[!data_duplicated$UDI == "eid",]
  print("重复的变量包括如下：")
  data_duplicated <- data_duplicated[duplicated(data_duplicated$UDI) | duplicated(data_duplicated$UDI, fromLast = TRUE),]
  print(data_duplicated[order(data_duplicated$UDI), ])

  # 定义模板
  sas_template <- "
  filename fsjeeno 'E:/rawdata/UKB_Data/UKB_data_20201126/ukb44656.sd2';
  data raw_sjeeno;
    infile fsjeeno RECFM=V LRECL=137496;
    input {ukb_20201126}
  ;
  run;


  filename fsrxlso 'E:/rawdata/UKB_Data/UKB_data_20201222/ukb44921.sd2';
  data raw_srxlso;
    infile fsrxlso RECFM=V LRECL=15741;
    input {ukb_20201222}
  ;
  run;

  filename fjujmir 'E:/rawdata/UKB_Data/UKB_data_20211013/ukb48833.sd2';
  data raw_jujmir;
    infile fjujmir RECFM=V LRECL=19790;
    input {ukb_20211013}
  ;
  run;

  filename fexucrt 'E:/rawdata/UKB_Data/UKB_data_20220705/ukb52673.sd2';
  data raw_exucrt;
    infile fexucrt RECFM=V LRECL=34662;
    input {ukb_20220705}
  ;
  run;

  filename fdxzlqh 'E:/rawdata/UKB_Data/UKB_data_20250412/metabolism.sd2';
  data raw_dxzlqh;
    infile fdxzlqh RECFM=V LRECL=137496;
    input {ukb_20250412}
  ;
  run;

  /* 先对每个数据集按n_eid排序 */
  proc sort data=raw_sjeeno; by n_eid; run;
  proc sort data=raw_srxlso; by n_eid; run;
  proc sort data=raw_jujmir; by n_eid; run;
  proc sort data=raw_exucrt; by n_eid; run;
  proc sort data=raw_dxzlqh; by n_eid; run;

  /* 合并数据集 */
  data merged_data;
    merge raw_sjeeno(in=a)
          raw_srxlso(in=b)
          raw_jujmir(in=c)
          raw_exucrt(in=d)
          raw_dxzlqh(in=e);
    by n_eid;
    /* 保留所有n_eid */
    if a or b or c or d or e;
  run;


  /* 导出为 CSV */
  PROC EXPORT DATA=merged_data  /* 要导出的数据集 */
      OUTFILE='E:/rawdata/UKB_Data/{prefix}_data.csv'  /* 输出路径 */
      DBMS=CSV REPLACE;  /* 指定格式为 CSV，REPLACE 表示覆盖已有文件 */
  RUN;"


  # 插入到模板中
  final_code <- glue(sas_template,
                     ukb_20201126 = paste(ukb_20201126, collapse = "\n  "),
                     ukb_20201222 = paste(ukb_20201222, collapse = "\n  "),
                     ukb_20211013 = paste(ukb_20211013, collapse = "\n  "),
                     ukb_20220705 = paste(ukb_20220705, collapse = "\n  "),
                     ukb_20250412 = paste(ukb_20250412, collapse = "\n  "),
                     prefix = sub(".*/", "", output_dir_prefix))

  # 输出为SAS文件
  writeLines(final_code, paste0(output_dir_prefix,".sas"))
}





#' rename_ukb_data <- function(data, field_list) {
#'   sapply(names(data), function(col_name) {
#'     # 按下划线分割当前列名
#'     x <- unlist(strsplit(col_name, "_"))
#'
#'     # 提取第二部分作为字段名（如果有）
#'     key <- if (length(x) >= 2) x[2] else x[1]
#'
#'     # 查找 field_list 中对应的中文名
#'     if (key %in% names(field_list) && field_list[[key]] != "") {
#'       paste0(field_list[[key]],
#'              ifelse(is.na(x[3]), "", x[3]),
#'              ifelse(is.na(x[4]), "", x[4]))
#'     } else {
#'       col_name  # 找不到就返回原列名
#'     }
#'   })
#' }






#' 合并两个数据框
#'
#' 定义合并函数，自动去除前一个df中与新df重复的变量
#'
#' @param df1 第一个数据框
#' @param df2 第二个数据框
#' @param by 通过哪个变量合并
#'
#' @returns
#' @export
#'
#' @examples
smart_merge <- function(df1, df2, by) {
  # 找到除合并键以外的重复列
  common_cols <- intersect(names(df1), names(df2))
  common_cols <- setdiff(common_cols, by)

  # 去掉 df1 中的重复列
  df1 <- df1 %>% select(-all_of(common_cols))

  # 合并
  full_join(df1, df2, by = by)
}






#' 提取 ukb 数据
#'
#' 从 ukb 数据库中提取所需 fields 的 csv 文件和提取的 fields 的 xlsx 文件
#'
#' @param field_list 所需变量的 field id list
#' @param ukb_data_dir ukb 原始数据的路径；默认 "~/rawdata/"
#' @param output_dir_prefix 原 csv 数据和 field_list xlsx 的路径及文件前缀
#'
#' @returns
#' @export
#'
#' @examples
extract_ukb_data <- function(field_list,ukb_data_dir="~/rawdata/",output_dir_prefix=""){
  # 用户不输入"eid"也可以
  field_list <- c("eid",field_list)

  # 提取用户需要的 filed 的详细信息
  Dictionary_Showcase <- read_csv(paste0(system.file(package = 'myRpkg'),"/extdata/Data_Dictionary_Showcase.csv"))
  Dictionary_Showcase <- Dictionary_Showcase[Dictionary_Showcase$FieldID %in% field_list, c("FieldID","Field","Field_zh","Notes_zh","ValueType","Units","Stability","Instances","Array")]

  # 读取 UKB_variable_dictionary 文件
  data_showcase <- read_csv(paste0(system.file(package = 'myRpkg'),"/extdata/UKB_variable_dictionary.csv"))
  data_showcase <- data_showcase[!is.na(data_showcase$Start_pos),]


  # 提取每个数据的 data showcase
  ukb_20201126 <- data_showcase[data_showcase$Download_date == "2020-11-26", ]
  ukb_20201222 <- data_showcase[data_showcase$Download_date == "2020-12-22", ]
  ukb_20211013 <- data_showcase[data_showcase$Download_date == "2021-10-13", ]
  ukb_20220705 <- data_showcase[data_showcase$Download_date == "2022-07-05", ]
  ukb_20250412 <- data_showcase[data_showcase$Download_date == "2025-04-12", ]


  # 把所需的挑出来
  data_showcase <- data_showcase[sapply(strsplit(data_showcase$UDI, "-"), function(x) x[1]) %in% field_list,]

  print(paste0("请求提取 ",length(field_list),"个变量"))
  print(paste0("共有 ",sum(field_list %in% sapply(strsplit(data_showcase$UDI, "-"), function(x) x[1]))," 个变量在数据库中"))
  missing_vars <- field_list[!field_list %in% sapply(strsplit(data_showcase$UDI, "-"), function(x) x[1])]
  if (length(missing_vars) > 0) {
    cat(paste0("数据库不包含变量：", paste(missing_vars, collapse = ", "), "\n"))
  } else {
    cat("所有变量均存在。\n")
  }

  # 重复的变量
  data_duplicated <- data_showcase[,c("UDI","Download_date","Count")]
  data_duplicated <- data_duplicated[!data_duplicated$UDI == "eid",]
  print("重复的变量包括如下：")
  data_duplicated <- data_duplicated[duplicated(data_duplicated$UDI) | duplicated(data_duplicated$UDI, fromLast = TRUE),]
  print(data_duplicated[order(data_duplicated$UDI), ])
  print("正在提取变量...")

  # 提取每个项目的数据
  laf1 <- laf_open_fwf(
    filename = paste0(ukb_data_dir,"/UKB_Data/UKB_data_20201126/ukb44656.sd2"),
    column_widths = ukb_20201126$Length,
    column_types = rep("string", length(ukb_20201126$Length)),
    column_names = ukb_20201126$Variable_name
  )
  df1 <- laf1[, ukb_20201126$Variable_name[sapply(strsplit(ukb_20201126$UDI, "-"),function(x) x[1]) %in% field_list]]

  laf2 <- laf_open_fwf(
    filename = paste0(ukb_data_dir,"/UKB_Data/UKB_data_20201222/ukb44921.sd2"),
    column_widths = ukb_20201222$Length,
    column_types = rep("string", length(ukb_20201222$Length)),
    column_names = ukb_20201222$Variable_name
  )
  df2 <- laf2[, ukb_20201222$Variable_name[sapply(strsplit(ukb_20201222$UDI, "-"),function(x) x[1]) %in% field_list]]

  laf3 <- laf_open_fwf(
    filename = paste0(ukb_data_dir,"/UKB_Data/UKB_data_20211013/ukb48833.sd2"),
    column_widths = ukb_20211013$Length,
    column_types = rep("string", length(ukb_20211013$Length)),
    column_names = ukb_20211013$Variable_name
  )
  df3 <- laf3[, ukb_20211013$Variable_name[sapply(strsplit(ukb_20211013$UDI, "-"),function(x) x[1]) %in% field_list]]

  laf4 <- laf_open_fwf(
    filename = paste0(ukb_data_dir,"/UKB_Data/UKB_data_20220705/ukb52673.sd2"),
    column_widths = ukb_20220705$Length,
    column_types = rep("string", length(ukb_20220705$Length)),
    column_names = ukb_20220705$Variable_name
  )
  df4 <- laf4[, ukb_20220705$Variable_name[sapply(strsplit(ukb_20220705$UDI, "-"),function(x) x[1]) %in% field_list]]

  laf5 <- laf_open_fwf(
    filename = paste0(ukb_data_dir,"/UKB_Data/UKB_data_20250412/metabolism.sd2"),
    column_widths = ukb_20250412$Length,
    column_types = rep("string", length(ukb_20250412$Length)),
    column_names = ukb_20250412$Variable_name
  )
  df5 <- laf5[, ukb_20250412$Variable_name[sapply(strsplit(ukb_20250412$UDI, "-"),function(x) x[1]) %in% field_list]]

  # 合并数据
  df_list <- list(df1, df2, df3, df4, df5)

  # 示例合并，保留 df5 中的重复列
  merged_df <- df1 %>%
    smart_merge(df2, by = "n_eid") %>%
    smart_merge(df3, by = "n_eid") %>%
    smart_merge(df4, by = "n_eid") %>%
    smart_merge(df5, by = "n_eid")

  # 如果 output_dir_prefix 为 NA 则不输出，只加载数据
  if (!missing(output_dir_prefix) & nchar(output_dir_prefix) > 0) {
    if (file.exists(paste0(output_dir_prefix,".csv"))) {
      # 合并（完全合并）去重（去掉前面的变量）
      library(data.table)
      data_orig <- fread(paste0(output_dir_prefix,".csv"), colClasses = "character")
      merged_df <- data_orig %>% smart_merge(merged_df, by = "n_eid")
      # 合并xlsx
      library(readxl)
      showcase_orig <- read_excel(paste0(output_dir_prefix, "_showcase.xlsx"))
      Dictionary_Showcase <- bind_rows(showcase_orig, Dictionary_Showcase) %>%
        distinct(FieldID, .keep_all = TRUE) %>%  # 按 FieldID 去重，保留所有列
        arrange(FieldID)
      write_csv(merged_df,paste0(output_dir_prefix,".csv"))
      write_xlsx(x = Dictionary_Showcase, file = paste0(output_dir_prefix,"_showcase.xlsx"))
      # 输出
      merged_df[merged_df == ""] <- NA
      return(merged_df)
    } else {
      write_csv(merged_df,paste0(output_dir_prefix,".csv"))
      write_xlsx(x = Dictionary_Showcase, file = paste0(output_dir_prefix,"_showcase.xlsx"))
      merged_df[merged_df == ""] <- NA
      return(merged_df)
    }
  } else {
    merged_df[merged_df == ""] <- NA
    return(merged_df)
  }
}





#' 生成 field id 的 list
#'
#' 根据用户的英文变量，寻找 field id，并生成 R 脚本文件
#'
#' @param input_vec 用户输入的变量向量
#' @param ukb_data_dir  ukb 原始数据的路径；默认 "~/rawdata/"
#' @param output_dir_prefix 前处理数据的 source 的 R 脚本文件路径前缀
#'
#' @returns res$field_list 和 res$code 是 source 的代码 和 res$add_code 新添加处理的变量
#' @export
#'
#' @examples
generate_fieldids_code <- function(input_vec, ukb_data_dir="~/rawdata/", output_dir_prefix="") {
  # 用户可不传入 eid
  input_vec <- c("eid",input_vec)

  # 获取所有非.R文件路径（递归搜索）
  dir_path <- paste0(ukb_data_dir,"/UKB_Data/vars/")
  file_list <- dir(dir_path, pattern = "[^.R]$")
  var_names <- sapply(strsplit(file_list, "\\."), function(x) x[3])
  file_var <- setNames(as.list(file_list), var_names)

  # 变量和 fieldid的列表
  var_field_list <- list()

  for (element in input_vec) {
    vars <- var_names[grepl(element, var_names)]   # 0或多个
    if (length(vars) == 0) {
      var_field_list[element] <- list(NULL)
      next  # 继续下一个 element
    }

    for (var in vars) {
      var_field_list[[var]] <- readLines(paste0(dir_path,file_var[var]))
    }
  }

  # 初始化结果
  field_list <- c()
  output_str <- ""
  add_str <- ""
  for (var in names(var_field_list)) {
    val <- var_field_list[[var]]
    if (is.null(val)) {
      field_list <- c(field_list, var)
    } else {
      field_list <- c(field_list, val)
      # 新增需要处理的code
      add_str <- paste0(add_str,readLines(paste0(dir_path,file_var[[var]],".R"), n = 1), "\n")
      add_str <- paste0(add_str,"source(","\"",dir_path,file_var[[var]],".R","\"",")", "\n")

      # 判断是否保存
      if (!missing(output_dir_prefix) & nchar(output_dir_prefix) > 0){
        # 并且有这么一个代码文件
        if (file.exists(paste0(output_dir_prefix,".R"))) {
          output_str <- readChar(paste0(output_dir_prefix,".R"), file.info(paste0(output_dir_prefix,".R"))$size)
        }
        # 将var对应的文件第一行输出到一个字符里保存
        output_str <- paste0(output_str,readLines(paste0(dir_path,file_var[[var]],".R"), n = 1), "\n")
        output_str <- paste0(output_str,"source(","\"",dir_path,file_var[[var]],".R","\"",")")
        writeLines(output_str, paste0(output_dir_prefix,".R"))   # 写入
        writeLines(unique(readLines(paste0(output_dir_prefix,".R"))), paste0(output_dir_prefix,".R"))   # 读取去重后输出
        output_str <- readChar(paste0(output_dir_prefix,".R"), file.info(paste0(output_dir_prefix,".R"))$size)  # 输出
      } else {
        output_str <- paste0(output_str,readLines(paste0(dir_path,file_var[[var]],".R"), n = 1), "\n")
        output_str <- paste0(output_str,"source(","\"",dir_path,file_var[[var]],".R","\"",")", "\n")
      }
    }
  }
  # 导出结果
  return(list(field_list = field_list, code = output_str, add_code = add_str))
}





#' 前处理 ukb 数据的 pipeline
#'
#' 提取数据、保存数据、前处理数据、改变变量名等等。相应会输出 field_list xlsx、csv 原始文件、Rdata 文件
#'
#' @param input_vec 需要提取的变量 list
#' @param ukb_data_dir  ukb 原始数据的路径；默认 "~/rawdata/"
#' @param output_dir_prefix 输出内容的路径及前缀
#'
#' @returns
#' @export
#'
#' @examples
preprocess_ukb_pipline <- function(input_vec,ukb_data_dir="~/rawdata/",output_dir_prefix="") {
  # 用户提供field list，找到filed——list和代码
  print(search())
  print("step1 generate_fieldids_code")
  print(Sys.time())
  res <- generate_fieldids_code(input_vec = input_vec,
                                ukb_data_dir = ukb_data_dir,
                                output_dir_prefix = output_dir_prefix)

  # 利用代码提取到环境中（提取后是否保存rawdata）
  print("step2 extract_ukb_data")
  print(Sys.time())
  all <- extract_ukb_data(res$field_list,
                          ukb_data_dir = ukb_data_dir,
                          output_dir_prefix = output_dir_prefix)

  # 处理数据
  print("step3 process_ukb_data")
  print(Sys.time())
  all <- process_ukb_data(data = all)


  # 如果需要保存则保存
  if (!missing(output_dir_prefix) & nchar(output_dir_prefix) > 0){
    # 并且有这么一个代码文件
    if (file.exists(paste0(output_dir_prefix,".Rdata"))) {
      # 加载这个文件
      all_new <- all
      load(paste0(output_dir_prefix,".Rdata"))
      # 找到除合并键以外的重复列
      common_cols <- intersect(names(all_new), names(all))
      common_cols <- setdiff(common_cols, "n_eid")
      # 去掉 原始 all 中的重复列
      all <- all %>% select(-all_of(common_cols))
      # 合并
      all <- full_join(all, all_new, by = "n_eid")
      # 前处理
      print("step4 pasrse")
      print(Sys.time())
      eval(parse(text = gsub("\\.R\"\\)", ".R\", local = TRUE)", res$add_code)))
      # 保存
      save(all,file = paste0(output_dir_prefix,".Rdata"))
    } else {
      # 不存在这个文件就创建一个
      print("step4 pasrse")
      print(Sys.time())
      eval(parse(text = gsub("\\.R\"\\)", ".R\", local = TRUE)", res$code)))
      # 保存
      save(all,file = paste0(output_dir_prefix,".Rdata"))
    }
  } else {
    # 返回 数据框
    print("step4 pasrse")
    print(Sys.time())
    eval(parse(text = gsub("\\.R\"\\)", ".R\", local = TRUE)", res$code)))
    return(all)
  }
  return(all)
}





#' 处理离群值
#'
#' 提供三种寻找离群值的方法，以及两种处理离群值的方法
#'
#' 离群值定义方法："quantile"（分位数）、"fixed"（固定阈值）或 "iqr"（三倍IQR）
#'
#' 处理动作："cap"（封顶，默认）或 "na"（替换为NA）
#'
#' @param x 数值向量（需处理的数据列）
#' @param method 处理离群值的方法；quantile（分位数）/ fixed 固定阈值 / iqr（三倍IQR）
#' @param action 离群值是要如何处理; cap 离群值封顶/ na 变为 NA
#' @param lower 下限值（分位数法时为下分位数，固定阈值法时为下限值，IQR法时不使用）
#' @param upper 上限值（分位数法时为上分位数，固定阈值法时为上限值，IQR法时不使用）
#' @param k IQR法的倍数；默认3
#' @param na.rm 是否忽略缺失值；默认TRUE
#'
#' @returns
#' @export
#'
#' @examples
#' # 示例数据：含有极端值
#' set.seed(123)
#' x <- c(rnorm(20, mean = 10, sd = 2), 50, -5)
#'
#' # 方法1: 分位数法（2.5% ~ 97.5%），超出范围的值替换为上下限
#' handle_outliers(x, method = "quantile", action = "cap")
#'
#' # 方法2: 分位数法（改为替换为 NA）
#' handle_outliers(x, method = "quantile", action = "na")
#'
#' # 方法3: 固定阈值（上下限 = 5 和 20），超出部分封顶
#' handle_outliers(x, method = "fixed", lower = 5, upper = 20, action = "cap")
#'
#' # 方法4: IQR 法（3 倍 IQR），超出范围的值替换为 NA
#' handle_outliers(x, method = "iqr", action = "na", k = 3)
#'
#' # 方法5: IQR 法（2 倍 IQR），改为封顶
#' handle_outliers(x, method = "iqr", action = "cap", k = 2)
handle_outliers <- function(x,
                            method = c("quantile", "fixed", "iqr"),
                            action = c("cap", "na"),
                            lower = 0.025,
                            upper = 0.975,
                            k = 3,
                            na.rm = TRUE) {

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

  return(x)
}




#' 回归分析（自变量为连续型）
#'
#' 可以循环自变量和因变量并按一般格式提取结果
#'
#' 该函数会执行以下操作：
#'
#' 1、遍历 x（暴露）和 y（结局），做连续 + 分类 + 趋势三种回归；如果 n = 0 只做连续型模型
#'
#' 2、提取结果；
#'
#' 3、整理成 Sheet1 (常用格式) ; Sheet2 (画图格式); Sheet3 (原始格式)
#'
#' 4、输出 Excel
#'
#' @param data 传入的数据框；默认为 all
#' @param x_vars 自变量向量
#' @param y_vars 因变量向量
#' @param covariates 协变量向量
#' @param n 分类回归类别数；默认为 4; 如果为 0 则只做连续型模型
#' @param file 结果输出文件
#' @param model_fun 模型名；默认 lm 模型，可改为 glm, lmer 等
#' @param model_args 额外传递给模型的参数
#' @param extract_fun 提取模型结果时选用的函数方法；默认 extract_model_results_wald，还可选 extract_model_results_conf
#' @param scale_x 是否对连续型模型的 x 去中心化
#' @param font 输出 xlsx 时的字体；详见 write_xlsx 函数
#' @param strata_var 分层变量；默认 NULL, 即不分层
#' @param results 数据框；默认为一个空的数据框; 还可传入之前结果的 results, 以达到不同分层有不同协变量, 但结果又在同一个表格中
#'
#' @returns
#' @export
#'
#' @examples
#' # 线性回归（默认）
#' out <- run_continuous_regression(data = all,
#'                                x_vars = c("wbc","neuc"),
#'                                y_vars = c("fev1","fvc"),
#'                                covariates = c("age","sex","height"),
#'                                n = 0,
#'                                file = "~/exp_lm.xlsx")
#'
#' # 广义线性回归（比如 logistic 回归）
#' out <- run_continuous_regression(data = all,
#'                                x_vars = c("wbc","neuc"),
#'                                y_vars = c("disease"),
#'                                covariates = c("age","sex","height"),
#'                                n = 4,
#'                                file = "~/exp_glm.xlsx",
#'                                model_fun = glm,
#'                                model_args = list(family = binomial),
#'                                extract_fun = extract_model_results_conf)
#'
#' # 混合效应模型（lme4::lmer）
#' out <- run_continuous_regression(data = all,
#'                                x_vars = c("wbc"),
#'                                y_vars = c("fev1"),
#'                                covariates = c("nl","xb","sg"),
#'                                n = 4,
#'                                file = "~/opp_lmer.xlsx",
#'                                model_fun = lme4::lmer,
#'                                model_args = list(REML = FALSE),
#'                                extract_fun = extract_model_results_wald)
run_continuous_regression <- function(data = NULL,
                                      x_vars,
                                      y_vars,
                                      covariates,
                                      n = 4,
                                      file = NULL,
                                      model_fun = lm,
                                      model_args = list(),
                                      extract_fun = extract_model_results_wald,
                                      scale_x = FALSE,
                                      font = "Times New Roman",
                                      strata_var = NULL,
                                      results = data.frame()) {

  # 如果有分层变量
  if (!is.null(strata_var)) {
    strata_levels <- levels(data[[strata_var]])

    for (x in x_vars) {
      for (y in y_vars) {
        for (index in strata_levels) {
          # 过程显示
          print(paste0(Sys.time(),' -- 开始 自变量：', x,' 因变量：', y, ' 分层：', strata_var, ' = ', index))

          # 分层数据
          tmp_df <- data[data[[strata_var]] == index, ]
          tmp_df <- quartile_cut(tmp_df, x, n)

          # 标准化 x
          if (scale_x) {tmp_df[[x]] <- scale(tmp_df[[x]])}

          # 临时结果
          tmp_results <- data.frame()

          # 连续 x
          formula <- as.formula(paste0(y,'~',x,"+", paste(covariates, collapse = "+")))
          model <- do.call(model_fun, c(list(formula, data = tmp_df), model_args))
          tmp_results <- do.call(extract_fun, list(x = x, y = y, model = model, results = tmp_results, data = tmp_df))

          if (!n == 0) {
            # 分类 x
            formula <- as.formula(paste0(y,'~',x,n,"+", paste(covariates, collapse = "+")))
            model <- do.call(model_fun, c(list(formula, data = tmp_df), model_args))
            tmp_results <- do.call(extract_fun, list(x = paste0(x,n), y = y, model = model, results = tmp_results, data = tmp_df))

            # P for trend
            tmp_df[[paste0(x,n)]] <- as.numeric(tmp_df[[paste0(x,n)]])
            formula <- as.formula(paste0(y,'~',x,n,"+", paste(covariates, collapse = "+")))
            model <- do.call(model_fun, c(list(formula, data = tmp_df), model_args))
            tmp_results <- do.call(extract_fun, list(x = paste0(x,n), y = y, model = model, results = tmp_results, data = tmp_df))
            tmp_df[[paste0(x,n)]] <- as.factor(tmp_df[[paste0(x,n)]])
          }

          # p modification for numeric-numeric
          origin_formula <- as.formula(paste0(y, '~', x, '*', strata_var, "+", paste(covariates, collapse = "+")))
          origin_model <- do.call(model_fun, c(list(origin_formula, data = data), model_args))
          crude_formula <- as.formula(paste0(y, '~', x, '+', strata_var, "+", paste(covariates, collapse = "+")))
          crude_model <- do.call(model_fun, c(list(crude_formula, data = data), model_args))
          anova_result <- anova(crude_model, origin_model)
          anova_result <- as.data.frame(anova_result)
          anova_result <- standardize_tidy_names(anova_result)
          # 从 ANOVA 结果中提取 p 值
          tmp_results$p_nn <- na.omit(anova_result$p.value)[1]

          # p modification for numeric-factor
          if (!n == 0) {
            data <- quartile_cut(data, x, n)

            origin_formula <- as.formula(paste0(y, '~', paste0(x,n), '*', strata_var, "+", paste(covariates, collapse = "+")))
            origin_model <- do.call(model_fun, c(list(origin_formula, data = data), model_args))
            crude_formula <- as.formula(paste0(y, '~', paste0(x,n), '+', strata_var, "+", paste(covariates, collapse = "+")))
            crude_model <- do.call(model_fun, c(list(crude_formula, data = data), model_args))
            anova_result <- anova(crude_model, origin_model)
            anova_result <- as.data.frame(anova_result)
            anova_result <- standardize_tidy_names(anova_result)
            # 从 ANOVA 结果中提取 p 值
            tmp_results$p_nf <- na.omit(anova_result$p.value)[1]
          }

          # 添加分层信息
          labels <- attr(data[[strata_var]], "labels")
          label <- names(labels)[labels == index]
          tmp_results$strata_var <- paste0(index, ' - ',label)
          tmp_results$n_strata <- nrow(tmp_df)

          # 合并结果
          results <- dplyr::bind_rows(results, tmp_results)
        }
      }
    }
  } else {
    # 无分层变量的原始逻辑
    for (x in x_vars) {
      for (y in y_vars) {
        # 过程显示
        print(paste0(Sys.time(),' -- 开始 自变量：', x,' 因变量：',y))
        data <- quartile_cut(data, x, n)

        # 标准化 x
        if (scale_x) {data[[x]] <- scale(data[[x]])}

        # 连续 x
        formula <- as.formula(paste0(y,'~',x,"+", paste(covariates, collapse = "+")))
        model <- do.call(model_fun, c(list(formula, data = data), model_args))
        results <- do.call(extract_fun, list(x = x, y = y, model = model, results = results, data = data))

        if (!n == 0) {
          # 分类 x
          formula <- as.formula(paste0(y,'~',x,n,"+", paste(covariates, collapse = "+")))
          model <- do.call(model_fun, c(list(formula, data = data), model_args))
          results <- do.call(extract_fun, list(x = paste0(x,n), y = y, model = model, results = results, data = data))

          # P for trend
          data[[paste0(x,n)]] <- as.numeric(data[[paste0(x,n)]])
          formula <- as.formula(paste0(y,'~',x,n,"+", paste(covariates, collapse = "+")))
          model <- do.call(model_fun, c(list(formula, data = data), model_args))
          results <- do.call(extract_fun, list(x = paste0(x,n), y = y, model = model, results = results, data = data))
          data[[paste0(x,n)]] <- as.factor(data[[paste0(x,n)]])
        }
      }
    }
  }
  # 格式化
  results$beta_CI_tidy <- sprintf("%.2f (%.2f, %.2f)", results$estimate, results$conf.low, results$conf.high)
  if ("p.value" %in% colnames(results)) {results <- results %>% mutate(p.value3 = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value)))}
  if ("p_nn" %in% colnames(results)) {results <- results %>% mutate(p_nn3 = ifelse(p_nn < 0.001, "<0.001", sprintf("%.3f", p_nn)))}
  if ("p_nf" %in% colnames(results)) {results <- results %>% mutate(p_nf3 = ifelse(p_nf < 0.001, "<0.001", sprintf("%.3f", p_nf)))}

  # 整理宽表
  results_data <- dplyr::select(results, any_of(c("x", "y", "beta_CI_tidy", "p.value3",'case','control','person_years', "strata_var", "n_strata", "p_nn3", "p_nf3")))
  results_plot <- dplyr::select(results, any_of(c("x", "y", 'estimate', 'conf.low', 'conf.high', 'p.value3','case','control','person_years', "strata_var", "n_strata", "p_nn3", "p_nf3")))


  # 无分层变量的原始宽表整理逻辑
  raw_names_data <- names(results_data)
  raw_names_plot <- names(results_plot)

  # 重置 n
  if (n == 0) {n = -1}

  results_data <- as.data.frame(matrix(t(results_data), ncol = (n+2)*ncol(results_data), byrow = TRUE))
  results_plot <- as.data.frame(matrix(t(results_plot), ncol = (n+2)*ncol(results_plot), byrow = TRUE))


  names(results_data) <- unlist(lapply(1:(n+2), function(i) paste(raw_names_data, i, sep = "_")))
  names(results_plot) <- unlist(lapply(1:(n+2), function(i) paste(raw_names_plot, i, sep = "_")))

  results_data <- dplyr::select(results_data, any_of(c('x_1','y_1', "strata_var_1", "n_strata_1",'beta_CI_tidy_1','p.value3_1','beta_CI_tidy_2',
                                                       paste0("beta_CI_tidy_", 3:(n+1)), paste0("p.value3_", n+2), 'case_1', 'control_1','person_years_1', "p_nn3_1", "p_nf3_1")))
  results_plot <- dplyr::select(results_plot, any_of(c('x_1','y_1', "strata_var_1", "n_strata_1",'estimate_1','conf.low_1','conf.high_1','p.value3_1','beta_CI_tidy_2',
                                                       as.vector(outer(c("estimate", "conf.low", "conf.high"), 3:(n+1), paste, sep = "_")),
                                                       paste0("p.value3_", n+2), 'case_1', 'control_1','person_years_1', "p_nn3_1", "p_nf3_1")))


  # 导出
  if (!is.null(file)) {
    write_xlsx(list(Sheet1 = results_data,
                    Sheet2 = results_plot,
                    Sheet3 = results),
               file = file,
               font = font)
  }

  return(list(results_data = results_data,
              results_plot = results_plot,
              results = results))
}





#' 回归分析（自变量为分类型）
#'
#' 可以循环自变量和因变量并按一般格式提取结果
#'
#' 该函数会执行以下操作：
#'
#' 1、遍历 x（暴露）和 y（结局）
#'
#' 2、提取结果；
#'
#' 3、整理成 Sheet1 (常用格式) ; Sheet2 (画图格式); Sheet3 (原始格式)
#'
#' 4、输出 Excel
#'
#' 注意！！！
#'
#' 如果结局为 surv 变量，必须有个事件和时间；如 diabetes_surv, 则需要有个 diabetes_event2, diabetes_time
#'
#' @param data 传入的数据框；默认为 all
#' @param x_vars 自变量向量
#' @param y_vars 因变量向量
#' @param covariates 协变量向量
#' @param file 结果输出文件; 以 .xlsx 结尾
#' @param model_fun 模型名；默认 lm 模型，可改为 glm, lmer 等
#' @param model_args 额外传递给模型的参数
#' @param extract_fun 提取模型结果时选用的函数方法；默认 extract_model_results_wald，还可选 extract_model_results_conf
#' @param font 输出 xlsx 时的字体；详见 write_xlsx 函数
#' @param strata_var 分层变量；默认 NULL, 即不分层
#' @param results 数据框；默认为一个空的数据框; 还可传入之前结果的 results, 以达到不同分层有不同协变量, 但结果又在同一个表格中
#'
#' @returns
#' @export
#'
#' @examples
run_categorical_regression <- function(data = NULL,
                                       x_vars,
                                       y_vars,
                                       covariates,
                                       file = NULL,
                                       model_fun = lm,
                                       model_args = list(),
                                       extract_fun = extract_model_results_wald,
                                       font = "Times New Roman",
                                       strata_var = NULL,
                                       results = data.frame()) {

  # 如果有分层变量
  if (!is.null(strata_var)) {
    strata_levels <- levels(data[[strata_var]])

    for (x in x_vars) {
      for (y in y_vars) {
        for (index in strata_levels) {
          # 过程显示
          print(paste0(Sys.time(),' -- 开始 自变量：', x,' 因变量：', y, ' 分层：', strata_var, ' = ', index))

          # 分层数据
          tmp_df <- data[data[[strata_var]] == index, ]

          # 确保自变量是因子类型
          if (!is.factor(tmp_df[[x]])) {
            tmp_df[[x]] <- as.factor(tmp_df[[x]])
          }

          # 临时结果
          tmp_results <- data.frame()

          # 运行回归模型
          formula <- as.formula(paste0(y, '~', x, "+", paste(covariates, collapse = "+")))
          model <- do.call(model_fun, c(list(formula, data = tmp_df), model_args))
          tmp_results <- do.call(extract_fun, list(x = x, y = y, model = model, results = tmp_results, data = tmp_df))

          # P for trend
          data[[x]] <- as.numeric(data[[x]])
          formula <- as.formula(paste0(y, '~', x, "+", paste(covariates, collapse = "+")))
          model <- do.call(model_fun, c(list(formula, data = data), model_args))
          tmp_results <- do.call(extract_fun, list(x = x, y = y, model = model, results = tmp_results, data = data))
          data[[x]] <- as.factor(data[[x]])

          # p modification for factor
          origin_formula <- as.formula(paste0(y, '~', x, '*', strata_var, "+", paste(covariates, collapse = "+")))
          origin_model <- do.call(model_fun, c(list(origin_formula, data = data), model_args))
          crude_formula <- as.formula(paste0(y, '~', x, '+', strata_var, "+", paste(covariates, collapse = "+")))
          crude_model <- do.call(model_fun, c(list(crude_formula, data = data), model_args))
          anova_result <- anova(crude_model, origin_model)
          anova_result <- as.data.frame(anova_result)
          anova_result <- standardize_tidy_names(anova_result)
          # 从 ANOVA 结果中提取 p 值
          tmp_results$p_ff <- na.omit(anova_result$p.value)[1]

          # 添加分层信息
          labels <- attr(data[[strata_var]], "labels")
          label <- names(labels)[labels == index]
          tmp_results$strata_var <- paste0(index, ' - ',label)
          tmp_results$n_strata <- nrow(tmp_df)

          # 合并结果
          results <- dplyr::bind_rows(results, tmp_results)
        }
      }
    }
  } else {
    # 无分层变量的原始逻辑
    for (x in x_vars) {
      for (y in y_vars) {
        # 过程显示
        print(paste0(Sys.time(),' -- 开始 自变量：', x,' 因变量：',y))

        # 确保自变量是因子类型
        if (!is.factor(data[[x]])) {
          data[[x]] <- as.factor(data[[x]])
        }

        # 运行回归模型
        formula <- as.formula(paste0(y, '~', x, "+", paste(covariates, collapse = "+")))
        model <- do.call(model_fun, c(list(formula, data = data), model_args))
        results <- do.call(extract_fun, list(x = x, y = y, model = model, results = results, data = data))

        # P for trend
        data[[x]] <- as.numeric(data[[x]])
        formula <- as.formula(paste0(y, '~', x, "+", paste(covariates, collapse = "+")))
        model <- do.call(model_fun, c(list(formula, data = data), model_args))
        results <- do.call(extract_fun, list(x = x, y = y, model = model, results = results, data = data))
        data[[x]] <- as.factor(data[[x]])
      }
    }
  }

  # 格式化结果
  results$beta_CI_tidy <- sprintf("%.2f (%.2f, %.2f)", results$estimate, results$conf.low, results$conf.high)
  if ("p.value" %in% colnames(results)) {results <- results %>% mutate(p.value3 = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value)))}
  if ("p_ff" %in% colnames(results)) {results <- results %>% mutate(p_ff3 = ifelse(p_ff < 0.001, "<0.001", sprintf("%.3f", p_ff)))}

  # 整理宽表
  results_data <- dplyr::select(results, any_of(c("x", "y", "strata_var", "n_strata", "beta_CI_tidy", "p.value3", 'case', 'control', 'person_years', "p_ff3")))
  results_plot <- dplyr::select(results, any_of(c("x", "y", "strata_var", "n_strata", 'estimate', 'conf.low', 'conf.high', 'p.value3', 'case', 'control', 'person_years', "p_ff3")))


  # 导出
  if (!is.null(file)) {
    write_xlsx(list(Sheet1 = results_data,
                    Sheet2 = results_plot,
                    Sheet3 = results),
               file = file,
               font = font)
  }

  return(list(results_data = results_data,
              results_plot = results_plot,
              results = results))
}





#' 提取疾病第一次发病函数提取发病时间
#'
#' 提取 UKB 中疾病的第一次发病时间
#'
#' 可以单独只使用 icd 10 或 icd 9, 不用的空着就行。
#'
#' 生成的变量名为 paste0(disease, '_first_date')。返回的结果为数据框。
#'
#' @param disease_names 疾病字符串或疾病向量；自定义某种疾病的名称
#' @param disease_10_codes_list 单个疾病 icd10 向量，或多个疾病 icd10 向量的 list
#' @param disease_9_codes_list 单个疾病 icd9 向量，或多个疾病 icd9 向量的 list
#' @param data 需要处理数据
#'
#' @returns 处理好的数据框
#' @export
#'
#' @examples
#' # 单个疾病
#' all <- get_first_diseases_date(
#'   disease_names  = "hemopathy",
#'   disease_10_codes_list = c("D70", "D71", "D72", "D75", "D76", "D77"),
#'   disease_9_codes_list  = c("288", "289"),
#'   data = all)
#'
#' # 多个疾病
#' all <- get_first_diseases_date(
#'   disease_names = c("hemopathy", "respiratory"),
#'   disease_10_codes_list = list(
#'     c("D70", "D71", "D72", "D75", "D76", "D77"),
#'     c("J40", "J41", "J42")),
#'   disease_9_codes_list = list(
#'     c("288", "289"),
#'     c("490", "491")),
#'   data = all)
get_first_diseases_date <- function(disease_names,
                                    disease_10_codes_list = list(),
                                    disease_9_codes_list = list(),
                                    data = all) {
  # 兼容：如果不是 list，则转为 list
  if (!is.list(disease_10_codes_list)) {
    disease_10_codes_list <- list(disease_10_codes_list)
  }
  if (!is.list(disease_9_codes_list)) {
    disease_9_codes_list <- list(disease_9_codes_list)
  }
  if (!is.vector(disease_names)) {
    disease_names <- c(disease_names)
  }

  data <- as.data.frame(data)   # 防止 data.table 报错

  # icd 10 矩阵
  icd_mat_10 <- as.matrix(data[, c(grep("^s_41270_0_", names(data), value = TRUE),
                                   grep("^s_40006_(0|[1-9]|1[0-9]|20)_0$", names(data), value = TRUE)
  )])

  date_mat_10 <- as.matrix(data[, c(grep("^s_41280_0_", names(data), value = TRUE),
                                    grep("^s_40005_(0|[1-9]|1[0-9]|20)_0$", names(data), value = TRUE)
  )])

  # icd 9 矩阵
  icd_mat_9 <- as.matrix(data[, grep("^s_41271_0_", names(data), value = TRUE)])
  date_mat_9 <- as.matrix(data[, grep("^s_41281_0_", names(data), value = TRUE)])

  # 加载 icd 数据
  data(icd)

  for (i in seq_along(disease_names)) {
    x <- disease_names[i]
    codes_10 <- icd$ICD10[icd$ICD10_cls %in% disease_10_codes_list[[i]]]
    codes_9  <- icd$ICD9[icd$ICD9_cls %in% disease_9_codes_list[[i]]]

    # 筛选 ICD10
    dates_icd10 <- matrix(NA, nrow = nrow(icd_mat_10), ncol = ncol(icd_mat_10))
    dates_icd10[icd_mat_10 %in% codes_10] <- date_mat_10[icd_mat_10 %in% codes_10]

    # 筛选 ICD9
    dates_icd9 <- matrix(NA, nrow = nrow(icd_mat_9), ncol = ncol(icd_mat_9))
    dates_icd9[icd_mat_9 %in% codes_9] <- date_mat_9[icd_mat_9 %in% codes_9]

    # 取最早日期
    first_date <- do.call(
      pmin,
      c(as.data.frame(dates_icd10),
        as.data.frame(dates_icd9),
        na.rm = TRUE)
    )

    # 写入结果
    data[[paste0(x, '_first_date')]] <- as.Date(first_date)
    attr(data[[paste0(x, '_first_date')]], "label") <- paste0("Date of first reported ", x)
    attr(data[[paste0(x, '_first_date')]], "source_field") <- "s_41270_*_*, s_41271_*_*, s_41280_*_*, s_41281_*_*, s_40005_*_*, s_40006_*_*"
  }

  return(data)
}






#' 提取死亡原因的死亡时间
#'
#' 提取 UKB 中某种死亡原因的死亡时间
#'
#' @param death_causes 死亡原因字符串或向量；自定义死亡原因的名称
#' @param death_codes_list 单个死亡原因 icd10 向量，或多个死亡原因 icd10 向量的 list
#' @param primary_secondary 字符；是选择主要死亡原因 "primary"，还是次要死亡原因 "secondary"，或两者都选 "both"
#' @param data 需要处理数据
#'
#' @returns 处理好的数据框
#' @export
#'
#' @examples
#' # 单个疾病
#' all <- get_death_causes_date(
#'   death_causes  = "hemopathy",
#'   death_codes_list = c("D70", "D71", "D72", "D75", "D76", "D77"),
#'   primary_secondary = "primary",
#'   data = all)
#'
#' # 多个疾病
#' all <- get_death_causes_date(
#'   death_causes = c("hemopathy", "respiratory"),
#'   death_codes_list = list(
#'     c("D70", "D71", "D72", "D75", "D76", "D77"),
#'     c("J40", "J41", "J42")),
#'   primary_secondary = "primary",
#'   data = all)
get_death_causes_date <- function(death_causes,
                                  death_codes_list = list(),
                                  primary_secondary = c("primary", "secondary", "both"),
                                  data = all) {
  # 参数检查
  primary_secondary <- match.arg(primary_secondary)
  if (!is.list(death_codes_list)) {
    death_codes_list <- list(death_codes_list)
  }
  if (!is.vector(death_causes)) {
    death_causes <- c(death_causes)
  }
  data <- as.data.frame(data)

  # 加载 icd 数据
  data(icd)

  # 主要死因（优先用 s_40001_1_0 覆盖 s_40001_0_0）
  death_primary <- as.character(data$s_40001_0_0)
  death_primary[!is.na(data$s_40001_1_0)] <- as.character(data$s_40001_1_0[!is.na(data$s_40001_1_0)])


  # secondary cause of death
  death_secondary <- as.matrix(data[, grep("^s_40002_0_", names(data), value = TRUE)])
  death_secondary_new <- as.matrix(data[, grep("^s_40002_1_", names(data), value = TRUE)])
  na_cols <- matrix(NA, nrow = nrow(death_secondary_new), ncol = 5)
  death_secondary_new <- cbind(death_secondary_new, na_cols)
  death_secondary[!is.na(data$s_40001_1_0), ] <- death_secondary_new[!is.na(data$s_40001_1_0), ]

  for (i in seq_along(death_causes)) {
    cause <- death_causes[i]
    codes <- icd$ICD10[icd$ICD10_cls %in% death_codes_list[[i]]]  # 使用 ICD10 编码
    codes <- na.omit(codes)

    # primary_secondary
    primary_match <- death_primary %in% codes
    secondary_match <- matrix(death_secondary %in% codes, nrow=nrow(death_secondary), ncol=ncol(death_secondary))
    secondary_match <- apply(secondary_match, 1, any)


    # 根据 primary_secondary 参数选择策略
    target_rows <- switch(
      primary_secondary,
      "primary" = primary_match,
      "secondary" = secondary_match,
      "both" = primary_match | secondary_match
    )

    # 填充日期字段
    data[[paste0('death_', cause, '_date')]] <- as.Date(NA)
    data[[paste0('death_', cause, '_date')]][target_rows] <- data$s_40000_0_0[target_rows]
    class(data$death_diabetes_date)

    # 写入结果
    attr(data[[paste0('death_', cause, '_date')]], "label") <- paste0("Date of death caused by ", cause)
    attr(data[[paste0('death_', cause, '_date')]], "source_field") <- "s_40000_*_*, s_40001_*_*, s_40002_*_*"
  }
  return(data)
}




