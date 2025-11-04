

setwd("~/myRpkg/")

# 定义输入输出目录
src_dir <- "./R_orig"
dst_dir <- "./inst/bin"

# 列出所有 R_orig 下的 .R 文件
r_files <- list.files(src_dir, pattern = "\\.R$", full.names = TRUE)

# 执行批量编译
for (r_file in r_files) {
  # 获取文件名（不带路径）
  base_name <- basename(r_file)
  # 目标 Rc 文件路径
  rc_file <- file.path(dst_dir, sub("\\.R$", ".Rc", base_name))

  # 编译
  compiler::cmpfile(r_file, rc_file, options = list(suppressAll = TRUE))

  message("✅ Compiled: ", base_name)
}

message("🎉 All files compiled successfully!")




