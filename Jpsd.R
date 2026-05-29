library(gsignal)

data <- read.csv("数据.csv", fileEncoding = "UTF-8", header = FALSE)

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
noverlap_ratio <- noverlap_points / window_length

pwelch_out <- pwelch(
  wave_height_detrended,
  hanning(window_length),
  noverlap_ratio,  
  window_length,
  Fs
)

f_wave <- pwelch_out$freq
P_wave <- pwelch_out$spec

psd_data <- data.frame(Frequency_Hz = f_wave, PSD_m2_per_Hz = P_wave)
write.csv(psd_data, "wave_psd_output.csv", row.names = FALSE, fileEncoding = "UTF-8")
