# 🦠 Dashboard Interaktif Malaria & Perubahan Iklim di Indonesia 🌧️
### 🇮🇩 Bahasa Indonesia  
Dashboard ini bertujuan untuk menganalisis keterkaitan antara kasus malaria dengan faktor perubahan iklim seperti suhu, curah hujan, dan kelembapan udara di Indonesia selama tahun 2001–2023. Dikembangkan menggunakan R Shiny, dashboard ini menyajikan visualisasi data spasial-temporal untuk mendukung pengambilan keputusan berbasis data.

### 🇬🇧 English  
This dashboard aims to analyze the relationship between malaria cases and climate change factors such as temperature, rainfall, and humidity in Indonesia during 2001–2023. Built using R Shiny, it provides spatio-temporal data visualizations to support data-driven decision making.

---

## 🎯 Tujuan Proyek | Project Objectives

### 🇮🇩 Bahasa Indonesia  
- Menganalisis hubungan spasial dan temporal antara faktor iklim & lingkungan dengan penyebaran malaria  
- Menelusuri distribusi nyamuk Anopheles berdasarkan faktor lingkungan  
- Mendeteksi tren tahunan kasus malaria
  
### 🇬🇧 English  
- Analyze the spatial and temporal relationship between climate and environmental factors with malaria spread  
- Track Anopheles mosquito distribution based on environmental conditions  
- Detect yearly trends of malaria cases  

---

## 📊 Data & Metode | Data & Methods

### 🇮🇩 Bahasa Indonesia  
Data diambil dari Publikasi BPS dan NASA, mencakup:  
- Kasus malaria  
- Curah hujan  
- Suhu udara  
- Kelembapan udara  

Metode analisis:  
- Time series  
- Korelasi  
- Regresi linier sederhana dan polinomial ordo 2

### 🇬🇧 English  
Data was sourced from BPS and NASA, including:  
- Malaria cases  
- Rainfall  
- Temperature  
- Humidity  

Analytical methods:  
- Time series  
- Correlation  
- Simple linear regression and second-order polynomial regression

---

## 🖥️ Struktur Dashboard | Dashboard Features

### 🇮🇩 Bahasa Indonesia  
Dashboard terdiri dari:  
- **Home**: Ringkasan proyek dan tutorial  
- **Time Series**: Tren tahunan kasus & iklim  
- **Peta Interaktif**: Visualisasi spasial  
- **Analisis Regresi**: Exploration of mathematical relationships between variables through graphs and models 
- **Metadata**: Informasi sumber data, struktur, dan metodologi analisis 
- **Unduh Data**: Download dataset custom

### 🇬🇧 English  
The dashboard includes:  
- **Home**: Project overview and tutorial  
- **Time Series**: Yearly trends of malaria & climate  
- **Interactive Map**: Spatial visualization  
- **Regression Analysis**: Relationship evaluation  
- **Metadata**: Details on dataset origin, data structure, and analytical approach  
- **Download Data**: Custom data export

---

## 🚀 Cara Menjalankan | How to Run

```bash
# Clone repo
git clone https://github.com/falizza/malaria-iklim.git

# Jalankan di R
shiny::runApp("malaria-iklim")
```

# Akses di link berikut.
https://faliza.shinyapps.io/malaria-iklim/

---

## 📦 Library yang Digunakan

Berikut adalah daftar package/library R yang digunakan dalam dashboard ini beserta fungsinya:

| Library         | Fungsi                                                                 |
|----------------|------------------------------------------------------------------------|
| `shiny`        | Framework utama untuk membangun aplikasi web interaktif berbasis R     |
| `leaflet`      | Menampilkan peta interaktif dengan fitur zoom, marker, dan layer       |
| `sf`           | Mengelola dan memanipulasi data spasial berbasis geometri (shapefile)  |
| `dplyr`        | Data wrangling: filter, select, mutate, group_by, dan summarize        |
| `haven`        | Membaca file dari SPSS (`.sav`) dan software statistik lainnya         |
| `shinydashboard` | Membuat layout dashboard yang responsif dan terstruktur              |
| `tidyr`        | Merapikan data, misalnya dari format wide ke long                      |
| `writexl`      | Menulis/mengekspor data ke file Excel (`.xlsx`)                        |
| `DT`           | Membuat tabel interaktif (sortable, searchable) dalam dashboard        |
| `ggplot2`      | Visualisasi data dengan grammar of graphics                            |
| `broom`        | Merapikan output model (seperti regresi) ke dalam bentuk dataframe     |
| `plotly`       | Membuat grafik interaktif dari objek `ggplot2` atau langsung dari data |
| `rmarkdown`    | Membuat laporan dinamis dalam format HTML, PDF, atau Word              |
| `knitr`        | Engine untuk rendering dokumen `rmarkdown`, khususnya saat knitting    |
| `gridExtra`    | Menata banyak grafik (grid layout) dalam satu tampilan visual          |

---

## 👥 Kontribusi Tim | Team Contributions

- **Faliza Maulidina Syarief (222313077)**  
- **Muhammad Imaddudin Zaki (222313244)**
- **Triangga Hafid Rifa'i (222313408)**

---

## 📚 Referensi
- Badan Pusat Statistik Indonesia. (2005–2024). *Statistik Lingkungan Hidup Indonesia*.  
  [https://www.bps.go.id](https://www.bps.go.id)  
- NASA Langley Research Center. POWER Data Access Viewer.  
  [https://power.larc.nasa.gov/data-access-viewer/](https://power.larc.nasa.gov/data-access-viewer/)
- Mardiana, M., & Anwar, D. (2012). *Pengaruh perubahan iklim terhadap insiden malaria di Kabupaten Bintan dan Banggai*. Jurnal Ekologi Kesehatan, 11(1), 1–10.  
- Apriliana, A. (2017). *Pengaruh iklim terhadap insidensi malaria di Provinsi Lampung*. Cermin Dunia Kedokteran, 44(7), 464–467.  
- Sari, R. S., et al. (2024). *Penerapan Model Regresi Logistik untuk Faktor Risiko Malaria di Sumatera Utara*. Leibniz: Jurnal Matematika, 4(2), 56–70.  

---
