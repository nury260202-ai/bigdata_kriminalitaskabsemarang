# ============================================================
# LOAD PAKET
# ============================================================

library(sf)
library(mgwnbr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(viridis)
library(tmap)

cat("\n========================================\n")
cat("PAKET BERHASIL DI-LOAD!\n")
cat("========================================\n")

# ============================================================
# INPUT DATA
# ============================================================

data_semarang <- data.frame(
  kecamatan = c("Getasan", "Tengaran", "Susukan", "Kaliwungu", "Suruh",
                "Pabelan", "Tuntang", "Banyubiru", "Jambu", "Sumowono",
                "Ambarawa", "Bandungan", "Bawen", "Bringin", "Bancak",
                "Pringapus", "Bergas", "Ungaran Barat", "Ungaran Timur"),
  
  kriminalitas = c(8, 8, 5, 5, 4, 9, 0, 6, 8, 1, 
                   10, 18, 8, 5, 1, 0, 2, 2, 2),
  
  penduduk = c(54546, 74748, 52055, 31889, 74450,
               47574, 71836, 45596, 42031, 35620,
               65402, 61053, 61683, 48474, 25676,
               59515, 78658, 83212, 84554),
  
  luas_wilayah = c(68.03, 49.95, 50.31, 31.08, 66.21,
                   51.86, 61.18, 51.85, 52.06, 58.86,
                   29.79, 47.41, 46.99, 68.19, 45.51,
                   84.27, 45.81, 48.79, 61.12),
  
  pengangguran = c(164, 422, 259, 171, 344,
                   192, 285, 158, 141, 100,
                   278, 174, 288, 261, 82,
                   489, 524, 1026, 554),
  
  pos_siskamling = c(120, 115, 103, 110, 185,
                     175, 201, 125, 195, 98,
                     157, 159, 215, 65, 60,
                     260, 323, 320, 165),
  
  X = c(110.440711, 110.522245, 110.592061, 110.616430, 110.572687,
        110.511849, 110.453618, 110.404019, 110.371920, 110.320582,
        110.404555, 110.366525, 110.430463, 110.520259, 110.591839,
        110.464667, 110.426771, 110.386462, 110.437172),
  
  Y = c(-7.376397, -7.420193, -7.410219, -7.461543, -7.367290,
        -7.296055, -7.266857, -7.293527, -7.275389, -7.224514,
        -7.255641, -7.222625, -7.223682, -7.253077, -7.238344,
        -7.189225, -7.186676, -7.129417, -7.133881)
)

data_semarang$kepadatan <- round(data_semarang$penduduk / data_semarang$luas_wilayah, 2)

# ============================================================
# UJI OVERDISPERSI
# ============================================================

mean_krim <- mean(data_semarang$kriminalitas)
var_krim <- var(data_semarang$kriminalitas)

cat("\n========================================\n")
cat("UJI OVERDISPERSI\n")
cat("========================================\n")
cat("Mean Kriminalitas:", mean_krim, "\n")
cat("Variance Kriminalitas:", var_krim, "\n")
cat("Rasio Variance/Mean:", var_krim/mean_krim, "\n")

if(var_krim/mean_krim > 1) {
  cat(">>> Terjadi OVERDISPERSI (rasio > 1)\n")
  cat(">>> GWBNR adalah metode yang tepat!\n")
} else {
  cat(">>> Tidak terjadi overdispersi\n")
}

# ============================================================
# VISUALISASI DATA
# ============================================================

ggplot(data_semarang, aes(x = kriminalitas)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  labs(title = "Distribusi Jumlah Kriminalitas per Kecamatan",
       subtitle = "Kabupaten Semarang 2023",
       x = "Jumlah Kejahatan", y = "Frekuensi") +
  theme_minimal()

ggplot(data_semarang, aes(y = kriminalitas)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "Boxplot Kriminalitas per Kecamatan",
       y = "Jumlah Kejahatan") +
  theme_minimal()

# ============================================================
# MODEL GWBNR (DIPERCEPAT)
# ============================================================

cat("\n========================================\n")
cat("MODEL GWBNR (SINGLE-SCALE + AIC + 20 ITERASI)\n")
cat("========================================\n")
cat("Proses ini membutuhkan waktu 2-5 menit...\n")
cat("Mohon tunggu...\n\n")

start_time <- Sys.time()

model_gwnbr <- mgwnbr(
  data = data_semarang,
  formula = kriminalitas ~ kepadatan + pengangguran + pos_siskamling,
  long = "X",
  lat = "Y",
  band_method = "adaptive_bsq",
  band_criterion = "aic",
  distribution = "negbin",
  multiscale = FALSE,
  globalmin = TRUE,
  max_int = 20
)

end_time <- Sys.time()
durasi <- round(difftime(end_time, start_time, units = "mins"), 2)

cat("\n========================================\n")
cat("HASIL GWBNR\n")
cat("========================================\n")
cat("Waktu komputasi:", durasi, "menit\n\n")

# ============================================================
# BANDWIDTH OPTIMAL
# ============================================================

cat("BANDWIDTH OPTIMAL:\n")
cat("----------------------------------------\n")

if (is.list(model_gwnbr$band)) {
  bandwidth_values <- as.numeric(unlist(model_gwnbr$band))
  bandwidth_names <- names(model_gwnbr$band)
} else {
  bandwidth_values <- as.numeric(model_gwnbr$band)
  bandwidth_names <- colnames(model_gwnbr$mgwr_param_estimates)
}

for (i in 1:length(bandwidth_names)) {
  interpretasi <- ifelse(bandwidth_values[i] < 5, "Sangat Lokal",
                         ifelse(bandwidth_values[i] < 10, "Lokal", "Global"))
  cat(sprintf("%-20s : %6.2f (%s)\n", 
              bandwidth_names[i], bandwidth_values[i], interpretasi))
}

# ============================================================
# GOODNESS OF FIT
# ============================================================

cat("\nGOODNESS OF FIT:\n")
cat("----------------------------------------\n")

if (is.list(model_gwnbr$measures)) {
  cat("AIC          :", model_gwnbr$measures$aic, "\n")
  cat("Deviance     :", model_gwnbr$measures$deviance, "\n")
  cat("Log Likelihood:", model_gwnbr$measures$loglik, "\n")
} else {
  cat("AIC          :", model_gwnbr$measures[1], "\n")
  cat("Deviance     :", model_gwnbr$measures[2], "\n")
  cat("Log Likelihood:", model_gwnbr$measures[3], "\n")
}

# ============================================================
# KOEFISIEN LOKAL
# ============================================================

cat("\nKOEFISIEN LOKAL PER KECAMATAN:\n")
cat("----------------------------------------\n")

if (is.matrix(model_gwnbr$mgwr_param_estimates)) {
  koefisien <- as.data.frame(model_gwnbr$mgwr_param_estimates)
  koefisien$kecamatan <- data_semarang$kecamatan
  kable(koefisien, caption = "Koefisien Lokal GWBNR per Kecamatan")
} else {
  print(head(model_gwnbr$mgwr_param_estimates))
}

# ============================================================
# RANGKUMAN KOEFISIEN
# ============================================================

cat("\nRANGKUMAN KOEFISIEN LOKAL:\n")
cat("----------------------------------------\n")

if (is.matrix(model_gwnbr$mgwr_param_estimates)) {
  summary_coef <- data.frame(
    Variabel = colnames(model_gwnbr$mgwr_param_estimates),
    Min = apply(model_gwnbr$mgwr_param_estimates, 2, min, na.rm = TRUE),
    Mean = apply(model_gwnbr$mgwr_param_estimates, 2, mean, na.rm = TRUE),
    Max = apply(model_gwnbr$mgwr_param_estimates, 2, max, na.rm = TRUE),
    SD = apply(model_gwnbr$mgwr_param_estimates, 2, sd, na.rm = TRUE)
  )
  kable(summary_coef, caption = "Ringkasan Koefisien Lokal")
}

# ============================================================
# 🔍 DEBUGGING: LIHAT STRUKTUR fitted_values
# ============================================================

cat("\n========================================\n")
cat("DEBUGGING: STRUKTUR fitted_values\n")
cat("========================================\n")

cat("Panjang fitted_values:", length(model_gwnbr$fitted_values), "\n")
cat("Kelas fitted_values:", class(model_gwnbr$fitted_values), "\n")
cat("Apakah list?", is.list(model_gwnbr$fitted_values), "\n")

if (is.list(model_gwnbr$fitted_values)) {
  cat("Jumlah elemen dalam list:", length(model_gwnbr$fitted_values), "\n")
  cat("Panjang elemen pertama:", length(model_gwnbr$fitted_values[[1]]), "\n")
  cat("Panjang elemen terakhir:", length(model_gwnbr$fitted_values[[length(model_gwnbr$fitted_values)]]), "\n")
}

# ============================================================
# ⚡ SOLUSI: AMBIL FITTED VALUES DENGAN CARA YANG BENAR ⚡
# ============================================================

data_hasil <- data_semarang

# Coba ambil fitted_values dengan berbagai cara
fitted_final <- NULL

# CARA 1: Jika ada elemen "final" atau "best"
if (!is.null(model_gwnbr$fitted_values)) {
  if (is.list(model_gwnbr$fitted_values)) {
    # Ambil elemen terakhir dari list (biasanya hasil final)
    last_elem <- model_gwnbr$fitted_values[[length(model_gwnbr$fitted_values)]]
    if (length(last_elem) == nrow(data_semarang)) {
      fitted_final <- as.numeric(last_elem)
    } else {
      # Coba ambil dari semua elemen yang panjangnya = 19
      for (i in 1:length(model_gwnbr$fitted_values)) {
        if (length(model_gwnbr$fitted_values[[i]]) == nrow(data_semarang)) {
          fitted_final <- as.numeric(model_gwnbr$fitted_values[[i]])
          break
        }
      }
    }
  } else {
    # Jika vektor, ambil 19 terakhir
    n_total <- length(model_gwnbr$fitted_values)
    n_lokasi <- nrow(data_semarang)
    if (n_total >= n_lokasi) {
      fitted_final <- as.numeric(model_gwnbr$fitted_values[(n_total - n_lokasi + 1):n_total])
    }
  }
}

# CARA 2: Jika masih NULL, hitung manual dari koefisien
if (is.null(fitted_final) || length(fitted_final) != nrow(data_semarang)) {
  cat("\n>>> fitted_values tidak ditemukan, menghitung manual...\n")
  if (is.matrix(model_gwnbr$mgwr_param_estimates)) {
    beta0 <- model_gwnbr$mgwr_param_estimates[, "(Intercept)"]
    beta1 <- model_gwnbr$mgwr_param_estimates[, "kepadatan"]
    beta2 <- model_gwnbr$mgwr_param_estimates[, "pengangguran"]
    beta3 <- model_gwnbr$mgwr_param_estimates[, "pos_siskamling"]
    
    linear_pred <- beta0 + beta1 * data_semarang$kepadatan + 
      beta2 * data_semarang$pengangguran + 
      beta3 * data_semarang$pos_siskamling
    fitted_final <- exp(linear_pred)
  }
}

# Masukkan ke data_hasil
if (!is.null(fitted_final) && length(fitted_final) == nrow(data_semarang)) {
  data_hasil$fitted <- fitted_final
} else {
  data_hasil$fitted <- NA
  cat("\n>>> PERINGATAN: Gagal mendapatkan fitted_values\n")
}

# Hitung residual
data_hasil$residual <- data_semarang$kriminalitas - data_hasil$fitted

# ============================================================
# CEK DATA
# ============================================================

cat("\nCEK DATA HASIL:\n")
cat("----------------------------------------\n")
cat("Jumlah baris data:", nrow(data_hasil), "\n")
cat("Jumlah fitted values:", length(data_hasil$fitted), "\n")
cat("Contoh fitted values:\n")
print(head(data_hasil[, c("kecamatan", "kriminalitas", "fitted", "residual")]))

# ============================================================
# TAMBAHKAN KOEFISIEN KE DATA
# ============================================================

if (is.matrix(model_gwnbr$mgwr_param_estimates)) {
  data_hasil$coef_kepadatan <- as.numeric(model_gwnbr$mgwr_param_estimates[, "kepadatan"])
  data_hasil$coef_pengangguran <- as.numeric(model_gwnbr$mgwr_param_estimates[, "pengangguran"])
  data_hasil$coef_siskamling <- as.numeric(model_gwnbr$mgwr_param_estimates[, "pos_siskamling"])
}

# ============================================================
# VISUALISASI KOEFISIEN LOKAL
# ============================================================

if ("coef_kepadatan" %in% colnames(data_hasil)) {
  coef_long <- data_hasil %>%
    select(kecamatan, coef_kepadatan, coef_pengangguran, coef_siskamling) %>%
    pivot_longer(cols = starts_with("coef_"),
                 names_to = "Variabel",
                 values_to = "Koefisien")
  
  ggplot(coef_long, aes(x = reorder(kecamatan, Koefisien), y = Koefisien, fill = Variabel)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(title = "Koefisien Lokal GWBNR per Kecamatan",
         subtitle = "Kabupaten Semarang 2023",
         x = "Kecamatan", y = "Koefisien") +
    theme_minimal() +
    scale_fill_viridis_d()
}

# ============================================================
# AKTUAL VS PREDIKSI
# ============================================================

if (!all(is.na(data_hasil$fitted))) {
  data_hasil$fitted <- as.numeric(data_hasil$fitted)
  data_hasil$kriminalitas <- as.numeric(data_hasil$kriminalitas)
  
  ggplot(data_hasil, aes(x = fitted, y = kriminalitas, label = kecamatan)) +
    geom_point(size = 3, color = "steelblue") +
    geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    labs(title = "Aktual vs Prediksi Kriminalitas",
         x = "Nilai Prediksi", y = "Nilai Aktual") +
    theme_minimal()
}

# ============================================================
# PETA RESIDUAL
# ============================================================

if (!all(is.na(data_hasil$fitted))) {
  data_hasil$residual <- as.numeric(data_hasil$residual)
  sf_hasil <- st_as_sf(data_hasil, coords = c("X", "Y"), crs = 4326)
  
  tm_shape(sf_hasil) +
    tm_dots(col = "residual",
            size = 0.6,
            palette = "-RdBu",
            title = "Residual") +
    tm_text("kecamatan", size = 0.6, col = "black", 
            xmod = 0.5, ymod = -0.5) +
    tm_layout(title = "Peta Residual GWBNR",
              legend.position = c("right", "bottom"))
}

# ============================================================
# INTERPRETASI HASIL
# ============================================================

cat("\n========================================\n")
cat("INTERPRETASI HASIL\n")
cat("========================================\n")

cat("\n1. INTERPRETASI BANDWIDTH:\n")
cat("   - Bandwidth kecil (< 5)  : Pengaruh sangat lokal\n")
cat("   - Bandwidth sedang (5-10): Pengaruh lokal\n")
cat("   - Bandwidth besar (> 10) : Pengaruh global\n\n")

for (i in 1:length(bandwidth_names)) {
  interpretasi <- ifelse(bandwidth_values[i] < 5, "Sangat Lokal",
                         ifelse(bandwidth_values[i] < 10, "Lokal", "Global"))
  cat(sprintf("   - %-20s: Bandwidth = %6.2f (%s)\n", 
              bandwidth_names[i], bandwidth_values[i], interpretasi))
}

cat("\n2. INTERPRETASI KOEFISIEN:\n")
cat("   - Koefisien positif (+) : Peningkatan variabel meningkatkan kriminalitas\n")
cat("   - Koefisien negatif (-) : Peningkatan variabel menurunkan kriminalitas\n")

# ============================================================
# SIMPAN HASIL
# ============================================================

write.csv(data_hasil, "hasil_gwnbr_semarang.csv", row.names = FALSE)

cat("\n========================================\n")
cat("ANALISIS GWBNR SELESAI!\n")
cat("========================================\n")
cat("File hasil disimpan sebagai: hasil_gwnbr_semarang.csv\n")