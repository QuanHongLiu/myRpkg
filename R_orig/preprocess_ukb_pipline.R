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
