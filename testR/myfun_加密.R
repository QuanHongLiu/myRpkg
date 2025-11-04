


.onLoad <- function(libname, pkgname) {
  .onLoad_fun(libname, pkgname)
}

.onLoad_fun <- function (libname, pkgname)
  UseMethod(".onLoad_fun")

.onLoad_fun.default <- function(libname, pkgname) {
  exe_Rc <- system.file("bin/onLoad.Rc", package = "myRpkg")
  if (exe_Rc == "") {
    exe_Rc <- "./inst/bin/onLoad.Rc"
  }
  compiler::loadcmp(exe_Rc, env = environment())
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
  .convert_labels_to_levels(df)
}

.convert_labels_to_levels <- function (df)
  UseMethod(".convert_labels_to_levels")

.convert_labels_to_levels.default <- function(df) {
  exe_Rc <- system.file("bin/convert_labels_to_levels.Rc", package = "myRpkg")
  compiler::loadcmp(exe_Rc, env = environment())
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
#'
#' @examples
add_model_info <- function(model_summary, model, data, x, y) {
  .add_model_info(model_summary, model, data, x, y)
}

.add_model_info <- function(model_summary, model, data, x, y)
  UseMethod(".add_model_info")


.add_model_info.default <- function(model_summary, model, data, x, y) {
  exe_Rc <- system.file("bin/add_model_info.Rc", package = "myRpkg")
  compiler::loadcmp(exe_Rc, env = environment())
  return(model_summary)
}



#' 将模型结果的列名统一为与 broom::tidy() 一致的格式
#'
#' @param df 需要统一的数据框
#'
#' @returns 处理好的数据框
#'
#' @examples
standardize_tidy_names <- function(df) {
  .standardize_tidy_names(df)
}

.standardize_tidy_names <- function(df)
  UseMethod(".standardize_tidy_names")


.standardize_tidy_names.default <- function(df) {
  exe_Rc <- system.file("bin/standardize_tidy_names.Rc", package = "myRpkg")
  compiler::loadcmp(exe_Rc, env = environment())
  return(df)
}




#' 判断模型系数是否需要取指数（exp(β)）
#'
#' @param model 拟合的模型对象（如glm, coxph等）
#'
#' @returns 逻辑值（TRUE需要取指数，FALSE不需要）
#'
#' @examples
needs_exp <- function(model) {
  .needs_exp(model)
}

.needs_exp <- function(model)
  UseMethod(".needs_exp")


.needs_exp.default <- function(model) {
  exe_Rc <- system.file("bin/needs_exp.Rc", package = "myRpkg")
  compiler::loadcmp(exe_Rc, env = environment())
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
#'
#' @examples
extract_model_results_conf <- function(x, y, model = model, results = results, data = NULL) {
  .extract_model_results_conf(x, y, model, results, data)
}

.extract_model_results_conf <- function(x, y, model, results, data)
  UseMethod(".extract_model_results_conf")


.extract_model_results_conf.default <- function(x, y, model, results, data) {
  exe_Rc <- system.file("bin/extract_model_results_conf.Rc", package = "myRpkg")
  compiler::loadcmp(exe_Rc, env = environment())
  return(results)
}



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
#'
#' @examples
extract_model_results_wald <- function(x, y, model = model, results = results, data = NULL) {
  .extract_model_results_wald(x, y, model, results, data)
}

.extract_model_results_wald <- function(x, y, model, results, data)
  UseMethod(".extract_model_results_wald")


.extract_model_results_wald.default <- function(x, y, model, results, data) {
  exe_Rc <- system.file("bin/extract_model_results_wald.Rc", package = "myRpkg")
  compiler::loadcmp(exe_Rc, env = environment())
  return(results)
}






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
  .harmonise_types(target_df, reference_df)
}

.harmonise_types <- function(target_df, reference_df)
  UseMethod(".harmonise_types")


.harmonise_types.default <- function(target_df, reference_df) {
  exe_Rc <- system.file("bin/harmonise_types.Rc", package = "myRpkg")
  compiler::loadcmp(exe_Rc, env = environment())
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
  .quartile_cut(dataframe, var_name, n, reverse)
}

.quartile_cut <- function(dataframe, var_name, n, reverse)
  UseMethod(".quartile_cut")


.quartile_cut.default <- function(dataframe, var_name, n, reverse) {
  exe_Rc <- system.file("bin/quartile_cut.Rc", package = "myRpkg")
  compiler::loadcmp(exe_Rc, env = environment())
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
write_xlsx <- function(x, file, row_height = 18, auto_width = TRUE, font = "Arial", size = 11, ...) {
  .write_xlsx(x, file, row_height, auto_width, font, size, ...)
}

.write_xlsx <- function(x, file, row_height, auto_width, font, size, ...)
  UseMethod(".write_xlsx")


.write_xlsx.default <- function(x, file, row_height, auto_width, font, size, ...) {
  exe_Rc <- system.file("bin/write_xlsx.Rc", package = "myRpkg")
  compiler::loadcmp(exe_Rc, env = environment())
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
process_ukb_data <- function(data) {
  .process_ukb_data(data)
}

.process_ukb_data <- function(data)
  UseMethod(".process_ukb_data")


.process_ukb_data.default <- function(data) {
  exe_Rc <- system.file("bin/process_ukb_data.Rc", package = "myRpkg")
  compiler::loadcmp(exe_Rc, env = environment())
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
#'
#' @examples
format_sas_code <- function(field_list,output_dir_prefix) {
  .format_sas_code(field_list,output_dir_prefix)
}

.format_sas_code <- function(field_list,output_dir_prefix)
  UseMethod(".format_sas_code")


.format_sas_code.default <- function(field_list,output_dir_prefix) {
  exe_Rc <- system.file("bin/format_sas_code.Rc", package = "myRpkg")
  compiler::loadcmp(exe_Rc, env = environment())
}






#' 合并两个数据框
#'
#' 定义合并函数，自动去除前一个df中与新df重复的变量, 再 full_join
#'
#' @param df1 第一个数据框
#' @param df2 第二个数据框
#' @param by 通过哪个变量合并
#'
#' @returns
#' @export
#'
#' @examples
smart_merge <- function(df1, df2, by = "n_eid") {
  .smart_merge(df1, df2, by)
}

.smart_merge <- function(df1, df2, by)
  UseMethod(".smart_merge")


.smart_merge.default <- function(df1, df2, by) {
  exe_Rc <- system.file("bin/smart_merge.Rc", package = "myRpkg")
  compiler::loadcmp(exe_Rc, env = environment())
  return(data)
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
#'
#' @examples
extract_ukb_data <- function(field_list, ukb_data_dir="~/rawdata/", output_dir_prefix="") {
  .extract_ukb_data(field_list, ukb_data_dir, output_dir_prefix)
}

.extract_ukb_data <- function(field_list, ukb_data_dir, output_dir_prefix)
  UseMethod(".extract_ukb_data")


.extract_ukb_data.default <- function(field_list, ukb_data_dir, output_dir_prefix) {
  exe_Rc <- system.file("bin/extract_ukb_data.Rc", package = "myRpkg")
  compiler::loadcmp(exe_Rc, env = environment())
  return(merged_df)
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
#'
#' @examples
generate_fieldids_code <- function(input_vec, ukb_data_dir="~/rawdata/", output_dir_prefix="") {
  .generate_fieldids_code(input_vec, ukb_data_dir, output_dir_prefix)
}

.generate_fieldids_code <- function(input_vec, ukb_data_dir, output_dir_prefix)
  UseMethod(".generate_fieldids_code")


.generate_fieldids_code.default <- function(input_vec, ukb_data_dir, output_dir_prefix) {
  exe_Rc <- system.file("bin/generate_fieldids_code.Rc", package = "myRpkg")
  compiler::loadcmp(exe_Rc, env = environment())
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
preprocess_ukb_pipline <- function(input_vec, ukb_data_dir="~/rawdata/", output_dir_prefix="") {
  .preprocess_ukb_pipline(input_vec, ukb_data_dir, output_dir_prefix)
}

.preprocess_ukb_pipline <- function(input_vec, ukb_data_dir, output_dir_prefix)
  UseMethod(".preprocess_ukb_pipline")


.preprocess_ukb_pipline.default <- function(input_vec, ukb_data_dir, output_dir_prefix) {
  exe_Rc <- system.file("bin/preprocess_ukb_pipline.Rc", package = "myRpkg")
  compiler::loadcmp(exe_Rc, env = environment())
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
  .handle_outliers(x, method, action, lower, upper, k, na.rm)
}

.handle_outliers <- function(x, method, action, lower, upper, k, na.rm)
  UseMethod(".handle_outliers")


.handle_outliers.default <- function(x, method, action, lower, upper, k, na.rm) {
  exe_Rc <- system.file("bin/handle_outliers.Rc", package = "myRpkg")
  compiler::loadcmp(exe_Rc, env = environment())
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
#' @param all_x_in 逻辑值；是否将所有 x 全部纳入模型, 即当循环某个 x 时, 其它 x 作为协变量；默认 FALSE
#' @param digits 整数型; 效应值保留小数位数
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
                                      results = data.frame(),
                                      all_x_in = FALSE,
                                      digits = 2) {
  .run_continuous_regression(data, x_vars, y_vars, covariates, n, file, model_fun,
                             model_args, extract_fun, scale_x, font, strata_var,
                             results, all_x_in, digits)
}

.run_continuous_regression <- function(data, x_vars, y_vars, covariates, n, file, model_fun,
                                       model_args, extract_fun, scale_x, font, strata_var,
                                       results, all_x_in, digits)
  UseMethod(".run_continuous_regression")


.run_continuous_regression.default <- function(data, x_vars, y_vars, covariates, n, file, model_fun,
                                               model_args, extract_fun, scale_x, font, strata_var,
                                               results, all_x_in, digits) {
  exe_Rc <- system.file("bin/run_continuous_regression.Rc", package = "myRpkg")
  compiler::loadcmp(exe_Rc, env = environment())
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
#' @param all_x_in 逻辑值；是否将所有 x 全部纳入模型, 即当循环某个 x 时, 其它 x 作为协变量；默认 FALSE
#' @param digits 整数型; 效应值保留小数位数
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
                                       results = data.frame(),
                                       all_x_in = FALSE,
                                       digits = 2) {
  .run_categorical_regression(data, x_vars, y_vars, covariates, file, model_fun,
                              model_args, extract_fun, font, strata_var,
                              results, all_x_in, digits)
}

.run_categorical_regression <- function(data, x_vars, y_vars, covariates, file, model_fun,
                                        model_args, extract_fun, font, strata_var,
                                        results, all_x_in, digits)
  UseMethod(".run_categorical_regression")


.run_categorical_regression.default <- function(data, x_vars, y_vars, covariates, file, model_fun,
                                                model_args, extract_fun, font, strata_var,
                                                results, all_x_in, digits) {
  exe_Rc <- system.file("bin/run_categorical_regression.Rc", package = "myRpkg")
  compiler::loadcmp(exe_Rc, env = environment())
  return(list(results_data = results_data,
              results_plot = results_plot,
              results = results))
}














#' 循环运行中介分析
#'
#' @param data 传入的数据框；默认为 NULL
#' @param x_vars 自变量向量
#' @param m_vars 中介变量向量
#' @param y_vars 因变量向量
#' @param covariates 协变量向量
#' @param strata_var 分层变量；默认 NULL, 即不分层
#' @param mediator_model_fun X → M 使用的模型名；默认 lm 模型，可改为 glm, lmer 等
#' @param outcome_model_fun X + M → Y 使用的模型名；默认 lm 模型，可改为 glm, lmer 等
#' @param mediator_model_args 额外传递给 X → M 模型的参数
#' @param outcome_model_args 额外传递给 X + M → Y 模型的参数
#' @param sims 蒙特卡罗抽样次数，用于 bootstrap 或准贝叶斯近似法；默认 1000
#' @param keep_models 逻辑值；输出的结果是否包含模型原始内容
#' @param file 结果输出文件; 以 .xlsx 结尾
#'
#' @returns
#' @export
#'
#' @examples
run_mediation_analysis <- function(data = NULL,
                                   x_vars,
                                   m_vars,
                                   y_vars,
                                   covariates = NULL,
                                   strata_var = NULL,
                                   mediator_model_fun = stats::lm,
                                   outcome_model_fun = stats::lm,
                                   mediator_model_args = list(),
                                   outcome_model_args = list(),
                                   sims = 1000,
                                   keep_models = TRUE,
                                   file = NULL) {
  .run_mediation_analysis(data, x_vars, m_vars, y_vars, covariates, strata_var,
                          mediator_model_fun, outcome_model_fun, mediator_model_args,
                          outcome_model_args, sims, keep_models, file)
}

.run_mediation_analysis <- function(data, x_vars, m_vars, y_vars, covariates, strata_var,
                                    mediator_model_fun, outcome_model_fun, mediator_model_args,
                                    outcome_model_args, sims, keep_models, file)
  UseMethod(".run_mediation_analysis")


.run_mediation_analysis.default <- function(data, x_vars, m_vars, y_vars, covariates, strata_var,
                                            mediator_model_fun, outcome_model_fun, mediator_model_args,
                                            outcome_model_args, sims, keep_models, file) {
  exe_Rc <- system.file("bin/run_mediation_analysis.Rc", package = "myRpkg")
  compiler::loadcmp(exe_Rc, env = environment())
  return(list(summary = results_summary, details = results_list))
}








#' 提取疾病第一次发病
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
#' @param by 字符串；使用住院 summary data, 还是 first occurrences 计算; 默认 first, 还可选 summary
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
                                    data = all,
                                    by = 'first') {
  .get_first_diseases_date(disease_names, disease_10_codes_list, disease_9_codes_list, data, by)
}

.get_first_diseases_date <- function(disease_names, disease_10_codes_list, disease_9_codes_list, data, by)
  UseMethod(".get_first_diseases_date")


.get_first_diseases_date.default <- function(disease_names, disease_10_codes_list, disease_9_codes_list, data, by) {
  exe_Rc <- system.file("bin/get_first_diseases_date.Rc", package = "myRpkg")
  compiler::loadcmp(exe_Rc, env = environment())
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
                                  primary_secondary = "primary",
                                  data = all) {
  .get_death_causes_date(death_causes, death_codes_list, primary_secondary, data)
}

.get_death_causes_date <- function(death_causes, death_codes_list, primary_secondary, data)
  UseMethod(".get_death_causes_date")


.get_death_causes_date.default <- function(death_causes, death_codes_list, primary_secondary, data) {
  exe_Rc <- system.file("bin/get_death_causes_date.Rc", package = "myRpkg")
  compiler::loadcmp(exe_Rc, env = environment())
  return(data)
}









