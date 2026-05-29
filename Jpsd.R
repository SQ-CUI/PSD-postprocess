library(gsignal)

# 读取数据（请根据实际情况调整分隔符和编码）
data <- read.csv("数据.csv", fileEncoding = "UTF-8", header = FALSE)
# 如果读入后只有一列，说明分隔符不对，可尝试 sep=";" 或 "\t"
# 若仍失败，用 data.table::fread 处理

# 重命名列（确保数据至少有两列）
if (ncol(data) >= 2) {
  colnames(data)[1:2] <- c("Time", "WaveHeight")
} else {
  stop("数据列数不足，请检查CSV文件格式")
}

time <- data$Time
wave_height <- data$WaveHeight

Fs <- 1 / (time[2] - time[1])
wave_height_detrended <- wave_height - mean(wave_height)

window_length <- floor(length(wave_height_detrended) / 30)
noverlap_points <- floor(window_length / 2)
noverlap_ratio <- noverlap_points / window_length   # 关键修改

pwelch_out <- pwelch(
  wave_height_detrended,
  hanning(window_length),
  noverlap_ratio,   # 使用比例
  window_length,
  Fs
)

f_wave <- pwelch_out$freq
P_wave <- pwelch_out$spec

# 创建数据框（两列：频率、功率谱密度）
psd_data <- data.frame(Frequency_Hz = f_wave, PSD_m2_per_Hz = P_wave)

# 写入 CSV 文件（不保存行名，使用 UTF-8 编码）
write.csv(psd_data, "wave_psd_output.csv", row.names = FALSE, fileEncoding = "UTF-8")