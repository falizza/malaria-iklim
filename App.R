# =====================================
# MALARIA DASHBOARD INDONESIA - CLEAN VERSION
# =====================================

# Load Required Libraries ----
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(haven)
library(shinydashboard)
library(tidyr)
library(writexl)
library(DT)
library(ggplot2)
library(broom)
library(plotly)
library(rmarkdown)
library(knitr)
library(gridExtra)

# =====================================
# DATA PREPARATION
# =====================================

# Load spatial data ----
provinsi_indonesia <- st_read('38 Provinsi Indonesia - Provinsi.json')

# Load datasets ----
data_malaria <- read_sav('datamalaria-lengkap.sav')
data_suhu <- read_sav('Data_suhu.sav')
data_hujan <- read_sav('Data_curah_hujan.sav')
data_lembap <- read_sav('Data_kelembapan.sav')

# Clean province names ----
clean_province_names <- function(data) {
  data$PROVINSI <- trimws(data$PROVINSI)
  return(data)
}

data_malaria <- clean_province_names(data_malaria)
data_suhu <- clean_province_names(data_suhu)
data_hujan <- clean_province_names(data_hujan)
data_lembap <- clean_province_names(data_lembap)

# Join all datasets with spatial data ----
provinsi_indonesia <- provinsi_indonesia %>%
  left_join(data_malaria, by = "PROVINSI") %>%
  left_join(data_suhu, by = "PROVINSI", suffix = c("", "_suhu")) %>%
  left_join(data_hujan, by = "PROVINSI", suffix = c("", "_hujan")) %>%
  left_join(data_lembap, by = "PROVINSI", suffix = c("", "_lembap"))

# =====================================
# HELPER FUNCTIONS
# =====================================

# Function to handle province administrative changes over time ----
gabung_provinsi_dinamis <- function(sf_data, tahun) {
  tahun <- as.integer(tahun)
  
  sf_data <- sf_data %>%
    mutate(Provinsi_Gabungan = case_when(
      PROVINSI == "Maluku Utara" & tahun < 2001 ~ "Maluku",
      PROVINSI == "Banten" & tahun < 2001 ~ "Jawa Barat",
      PROVINSI == "Gorontalo" & tahun < 2001 ~ "Sulawesi Utara",
      PROVINSI == "Kepulauan Bangka Belitung" & tahun < 2002 ~ "Sumatera Selatan",
      PROVINSI == "Papua Barat" & tahun < 2004 ~ "Papua",
      PROVINSI == "Kepulauan Riau" & tahun < 2004 ~ "Riau",
      PROVINSI == "Sulawesi Barat" & tahun < 2004 ~ "Sulawesi Selatan",
      PROVINSI == "Kalimantan Utara" & tahun < 2012 ~ "Kalimantan Timur",
      PROVINSI == "Papua Tengah" & tahun < 2022 ~ "Papua",
      PROVINSI == "Papua Pegunungan" & tahun < 2022 ~ "Papua",
      PROVINSI == "Papua Selatan" & tahun < 2022 ~ "Papua",
      PROVINSI == "Papua Barat Daya" & tahun < 2022 ~ "Papua Barat",
      TRUE ~ PROVINSI
    ))
  
  result <- sf_data %>%
    group_by(Provinsi_Gabungan) %>%
    summarise(geometry = st_combine(geometry), .groups = "drop")
  
  return(result)
}

# Regression analysis functions ----
analyze_both_regressions <- function(data, x_var, y_var) {
  valid_data <- data %>%
    filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))
  
  if(nrow(valid_data) < 4) {
    return(list(
      sufficient_data = FALSE, 
      message = "Data tidak cukup untuk analisis (minimal 4 titik data)"
    ))
  }
  
  # Linear model
  model_lin <- lm(paste(y_var, "~", x_var), data = valid_data)
  summary_lin <- summary(model_lin)
  glance_lin <- broom::glance(model_lin)
  
  # Polynomial model
  model_poly <- lm(paste(y_var, "~ poly(", x_var, ", 2)"), data = valid_data)
  summary_poly <- summary(model_poly)
  glance_poly <- broom::glance(model_poly)
  
  return(list(
    sufficient_data = TRUE,
    data = valid_data,
    linear = list(model = model_lin, summary = summary_lin, glance = glance_lin),
    polynomial = list(model = model_poly, summary = summary_poly, glance = glance_poly)
  ))
}

create_single_interpretation <- function(model_result, model_type) {
  glance_data <- model_result$glance
  
  adj_r_squared <- round(glance_data$adj.r.squared, 3)
  p_value_model <- glance_data$p.value
  significance <- ifelse(p_value_model < 0.05, "signifikan", "tidak signifikan")
  
  interpretation_praktis <- ""
  if (model_type == "Linear") {
    tidy_data <- broom::tidy(model_result$model)
    coef_slope <- round(tidy_data$estimate[2], 3)
    p_value_slope <- tidy_data$p.value[2]
    
    interpretation_praktis <- paste0(
      "<li><b>Interpretasi Koefisien:</b> ",
      ifelse(p_value_slope < 0.05,
             paste("Variabel iklim secara statistik <strong>signifikan</strong> mempengaruhi kasus malaria. Setiap kenaikan 1 unit pada variabel iklim berasosiasi dengan", 
                   ifelse(coef_slope >= 0, "kenaikan", "penurunan"), 
                   "rata-rata", abs(coef_slope), "kasus malaria."),
             "Variabel iklim secara statistik <strong>tidak signifikan</strong> mempengaruhi kasus malaria pada model ini."
      ),
      "</li>"
    )
  } else {
    interpretation_praktis <- "<li><b>Interpretasi Kurva:</b> Hubungan antara variabel iklim dan malaria bersifat non-linear (melengkung). Bentuk kurva pada grafik menunjukkan bagaimana hubungan tersebut berubah.</li>"
  }
  
  return(paste0(
    "<div style='padding: 15px; background-color: #ffffff; border-left: 4px solid #007bff; border-radius: 5px;'>",
    "<h4>Interpretasi Model ", model_type, "</h4>",
    "<ul style='list-style-type: disc; margin-left: 20px;'>",
    "<li><b>Kecocokan Model (Adjusted R²):</b> ", adj_r_squared, ". Metrik ini menjelaskan ", round(adj_r_squared*100, 1), "% variasi kasus malaria, dengan mempertimbangkan kompleksitas model.</li>",
    "<li><b>Signifikansi Model:</b> Secara keseluruhan, model ini secara statistik <strong>", significance, "</strong> dalam menjelaskan hubungan (p-value = ", format(p_value_model, scientific = FALSE, digits=4), ").</li>",
    interpretation_praktis,
    "</ul></div>"
  ))
}

# Data processing functions ----
prepare_long_data <- function(provinsi_data) {
  provinsi_data %>%
    st_drop_geometry() %>%
    rename_with(~ gsub("^(t\\d+)$", "\\1_malaria", .x), matches("^t\\d+$")) %>%
    select(PROVINSI, matches("^t\\d+_")) %>%
    pivot_longer(cols = -PROVINSI, names_to = "temp", values_to = "value") %>%
    separate(temp, into = c("Tahun", "Variable"), sep = "_") %>%
    mutate(Tahun = as.integer(gsub("t", "", Tahun))) %>%
    pivot_wider(names_from = Variable, values_from = value) %>%
    rename(
      Kasus_Malaria = malaria,
      Suhu = suhu,
      Curah_Hujan = hujan, 
      Kelembapan = lembap
    )
}

calculate_summary_stats <- function(longdata) {
  longdata %>%
    filter(!is.na(Kasus_Malaria)) %>%
    summarise(
      total_kasus = sum(Kasus_Malaria, na.rm = TRUE),
      rata_rata = mean(Kasus_Malaria, na.rm = TRUE),
      kasus_tertinggi = max(Kasus_Malaria, na.rm = TRUE),
      kasus_terendah = min(Kasus_Malaria, na.rm = TRUE),
      jumlah_provinsi = 38,
      periode_data = paste(min(Tahun, na.rm = TRUE), "-", max(Tahun, na.rm = TRUE))
    )
}

get_top_provinces <- function(longdata, n = 5) {
  longdata %>%
    filter(!is.na(Kasus_Malaria)) %>%
    group_by(PROVINSI) %>%
    summarise(total_kasus = sum(Kasus_Malaria, na.rm = TRUE)) %>%
    arrange(desc(total_kasus)) %>%
    head(n)
}

# =====================================
# DATA PROCESSING
# =====================================

# Create long format data ----
longdata <- prepare_long_data(provinsi_indonesia)

# Prepare data for download section ----
cleantable <- provinsi_indonesia %>%
  st_drop_geometry() %>%
  select(PROVINSI, starts_with("t"))

# Constants ----
listtahun <- as.character(2001:2023)
max_kasus <- max(longdata$Kasus_Malaria, na.rm = TRUE)

# Summary statistics ----
summary_stats <- calculate_summary_stats(longdata)
top_provinsi <- get_top_provinces(longdata)

# =====================================
# CSS STYLES
# =====================================

custom_css <- "
  input[type='number']{max-width: 80%;}
  div.outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0;}
  body, label, input, button, select {font-family: 'Helvetica Neue', Helvetica; font-weight: 200;}
  h1, h2, h3, h4 {font-weight: 400;}
  
  #controls {
    background-color: white; padding: 0 20px 20px 20px; cursor: move;
    opacity: 0.65; zoom: 0.9; transition: opacity 500ms 1s;
  }
  #controls:hover {opacity: 0.95; transition-delay: 0;}
  
  .leaflet-container {background-color: white !important;}
  
  .main-title {text-align: center; color: #2c3e50; margin: 30px 0 40px 0; font-size: 2.5em; font-weight: 300;}
  .section-title {color: #34495e; margin: 40px 0 25px 0; font-size: 1.8em; font-weight: 400; border-bottom: 3px solid #3498db; padding-bottom: 10px;}
  
  .stats-box {
    background: linear-gradient(135deg, #3498db 0%, #2980b9 100%); color: white; border-radius: 15px;
    padding: 30px 20px; margin-bottom: 20px; text-align: center;
    box-shadow: 0 8px 16px rgba(52, 152, 219, 0.3); transition: transform 0.3s ease, box-shadow 0.3s ease;
    height: 120px; display: flex; flex-direction: column; justify-content: center;
  }
  .stats-box:hover {transform: translateY(-5px); box-shadow: 0 12px 24px rgba(52, 152, 219, 0.4);}
  .stats-box h3 {margin: 0 0 5px 0; font-size: 2.2em; font-weight: bold;}
  .stats-box p {margin: 0; font-size: 1em; opacity: 0.9;}
  
  .timeline-container {
    position: relative; padding: 40px 0; background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
    border-radius: 20px; box-shadow: 0 10px 30px rgba(0,0,0,0.1); margin: 30px 0;
  }
  .timeline-arrow {
    position: absolute; top: 50%; left: 0; right: 0; height: 6px;
    background: linear-gradient(90deg, #3498db 0%, #2980b9 100%); transform: translateY(-50%); z-index: 1;
  }
  .timeline-arrow::after {
    content: ''; position: absolute; right: -20px; top: 50%; width: 0; height: 0;
    border-left: 20px solid #2980b9; border-top: 15px solid transparent; border-bottom: 15px solid transparent;
    transform: translateY(-50%);
  }
  .timeline-weeks {display: flex; justify-content: space-between; align-items: center; position: relative; z-index: 2; padding: 0 40px;}
  .timeline-week {
    background: linear-gradient(135deg, #4a90e2 0%, #357abd 100%); color: white; border-radius: 15px;
    padding: 20px 15px; text-align: center; box-shadow: 0 8px 16px rgba(74, 144, 226, 0.3);
    transition: all 0.3s ease; min-height: 120px; display: flex; flex-direction: column; justify-content: center;
    flex: 1; margin: 0 10px; position: relative; border: 3px solid white;
  }
  .timeline-week:hover {transform: translateY(-8px) scale(1.05); box-shadow: 0 15px 30px rgba(74, 144, 226, 0.4); z-index: 10;}
  .timeline-week h4 {margin: 0 0 10px 0; font-size: 1.1em; font-weight: bold; color: #fff;}
  .timeline-week p {margin: 0; font-size: 0.85em; line-height: 1.3; opacity: 0.95;}
  
  .timeline-week:nth-child(1) {background: linear-gradient(135deg, #e74c3c 0%, #c0392b 100%);}
  .timeline-week:nth-child(2) {background: linear-gradient(135deg, #f39c12 0%, #d68910 100%);}
  .timeline-week:nth-child(3) {background: linear-gradient(135deg, #27ae60 0%, #229954 100%);}
  .timeline-week:nth-child(4) {background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);}
  .timeline-week:nth-child(5) {background: linear-gradient(135deg, #9b59b6 0%, #8e44ad 100%);}
  .timeline-week:nth-child(6) {background: linear-gradient(135deg, #34495e 0%, #2c3e50 100%);}
  
  .custom-box {border-radius: 15px; box-shadow: 0 6px 12px rgba(0,0,0,0.1); margin-bottom: 30px;}
  .custom-box .box-header {border-radius: 15px 15px 0 0; background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);}
  
  .about-content {background: #f8f9fa; border-radius: 15px; padding: 30px; box-shadow: 0 6px 12px rgba(0,0,0,0.05);}
  .about-content ul {padding-left: 20px;}
  .about-content li {margin-bottom: 8px; color: #555;}
  
  .video-container {
    position: relative; width: 100%; height: 0; padding-bottom: 56.25%;
    border-radius: 15px; overflow: hidden; box-shadow: 0 6px 12px rgba(0,0,0,0.1);
  }
  .video-container iframe {position: absolute; top: 0; left: 0; width: 100%; height: 100%;}
  
  .info.legend {
    background: rgba(255,255,255,0.8);
    box-shadow: 0 0 15px rgba(0,0,0,0.2);
    border-radius: 5px;
    padding: 8px;
    font: 12px/14px Arial, Helvetica, sans-serif;
    color: #555;
  }
  .info.legend .legend-item {
    display: flex;
    align-items: center;
    margin-bottom: 5px;
  }
  .info.legend i {
    width: 18px;
    height: 18px;
    margin-right: 8px;
    opacity: 0.7;
    flex-shrink: 0;
  }
  
  /* User Guide Button Styles */
  .user-guide-btn {
    background: linear-gradient(135deg, #28a745 0%, #20c997 100%);
    color: white;
    border: none;
    border-radius: 25px;
    padding: 15px 30px;
    font-size: 16px;
    font-weight: 600;
    cursor: pointer;
    transition: all 0.3s ease;
    box-shadow: 0 4px 15px rgba(40, 167, 69, 0.3);
    display: flex;
    align-items: center;
    justify-content: center;
    text-decoration: none;
    margin: 20px auto;
    width: fit-content;
  }
  
  .user-guide-btn:hover {
    transform: translateY(-3px);
    box-shadow: 0 6px 20px rgba(40, 167, 69, 0.4);
    background: linear-gradient(135deg, #218838 0%, #1db584 100%);
    color: white;
    text-decoration: none;
  }
  
  .user-guide-btn i {
    margin-right: 10px;
    font-size: 18px;
  }
  
  .user-guide-section {
    background: linear-gradient(135deg, #e8f5e8 0%, #d4edda 100%);
    border-radius: 15px;
    padding: 30px;
    margin: 30px 0;
    text-align: center;
    box-shadow: 0 6px 12px rgba(0,0,0,0.05);
    border-left: 5px solid #28a745;
  }
  
  .user-guide-section h3 {
    color: #155724;
    margin-bottom: 15px;
    font-size: 1.4em;
  }
  
  .user-guide-section p {
    color: #155724;
    font-size: 14px;
    margin-bottom: 20px;
    line-height: 1.5;
  }
  
  @media (max-width: 1200px) {
    .timeline-weeks {flex-wrap: wrap;}
    .timeline-week {flex: 0 0 calc(50% - 20px); margin-bottom: 20px;}
    .timeline-arrow {display: none;}
  }
  @media (max-width: 768px) {
    .timeline-week {flex: 0 0 calc(100% - 20px);}
  } 
  .interpretation-box {
    background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
    border-left: 5px solid #3498db;
    border-radius: 10px;
    padding: 20px;
    margin: 15px 0;
    box-shadow: 0 4px 8px rgba(0,0,0,0.1);
  }
  
  .interpretation-title {
    color: #2c3e50;
    font-size: 1.2em;
    font-weight: 600;
    margin-bottom: 10px;
    display: flex;
    align-items: center;
  }
  
  .interpretation-title i {
    margin-right: 8px;
    color: #3498db;
  }
  
  .interpretation-content {
    color: #34495e;
    line-height: 1.6;
    font-size: 0.95em;
  }
  
  .interpretation-highlight {
    background: linear-gradient(135deg, #fff3cd 0%, #ffeaa7 100%);
    border-left: 4px solid #f39c12;
    padding: 10px 15px;
    border-radius: 5px;
    margin: 10px 0;
  }
  
  .correlation-interpretation {
    background: linear-gradient(135deg, #e8f5e8 0%, #d4edda 100%);
    border-left: 4px solid #28a745;
    padding: 15px;
    border-radius: 8px;
    margin: 15px 0;
  }
  
  .trend-indicator {
    display: inline-block;
    padding: 3px 8px;
    border-radius: 12px;
    font-size: 0.8em;
    font-weight: 600;
    margin: 0 5px;
  }
  
  .trend-increasing {
    background: #ffebee;
    color: #c62828;
  }
  
  .trend-decreasing {
    background: #e8f5e8;
    color: #2e7d32;
  }
  
  .trend-stable {
    background: #fff3e0;
    color: #ef6c00;
  }
"

# =====================================
# USER INTERFACE
# =====================================

# UI Helper Functions ----
# UI Helper Functions ----
create_home_tab <- function() {
  tabItem(tabName = "home",
          div(style = "padding: 20px;",
              h1("Selamat Datang di Dashboard Malaria Indonesia", class = "main-title"),
              
              # User Guide Section
              div(class = "user-guide-section",
                  h3("📚 Panduan Pengguna"),
                  p("Pelajari cara menggunakan dashboard ini dengan mengunduh panduan pengguna lengkap dalam format PDF."),
                  tags$a(href = "userguide.pdf", target = "_blank", 
                         class = "user-guide-btn",
                         tags$i(class = "fa fa-file-pdf-o"),
                         "Download Panduan Pengguna (PDF)"
                  )
              ),
              
              # Summary Statistics
              h2("Ringkasan Statistik Data Malaria", class = "section-title"),
              fluidRow(
                column(3, div(class = "stats-box",
                              h3(format(summary_stats$total_kasus, big.mark = ".")),
                              p("Total Kasus Malaria")
                )),
                column(3, div(class = "stats-box",
                              h3(summary_stats$jumlah_provinsi),
                              p("Provinsi Terpantau")
                )),
                column(3, div(class = "stats-box",
                              h3(format(round(summary_stats$rata_rata), big.mark = ".")),
                              p("Rata-rata Kasus per Provinsi")
                )),
                column(3, div(class = "stats-box",
                              h3(summary_stats$periode_data),
                              p("Periode Data")
                ))
              ),
              
              # Content Section
              fluidRow(
                column(6,
                       box(title = "Top 5 Provinsi dengan Kasus Tertinggi", status = "primary", 
                           solidHeader = TRUE, width = 12, class = "custom-box",
                           DT::dataTableOutput("top_provinsi_table")
                       )
                ),
                column(6,
                       box(title = "Video Tutorial Penggunaan Dashboard", status = "success", 
                           solidHeader = TRUE, width = 12, class = "custom-box",
                           div(class = "video-container",
                               tags$iframe(src = "https://www.youtube.com/embed/dQw4w9WgXcQ",
                                           frameborder = "0", allowfullscreen = TRUE)
                           )
                       )
                )
              ),
              
              # Timeline Section
              div(class = "timeline-section",
                  h2("Linimasa Pengembangan Proyek", class = "section-title"),
                  div(class = "timeline-container",
                      div(class = "timeline-arrow"),
                      div(class = "timeline-weeks",
                          div(class = "timeline-week", h4("Minggu 7"), p("Membuat proposal dashboard")),
                          div(class = "timeline-week", h4("Minggu 8"), p("Mencari data yang diperlukan")),
                          div(class = "timeline-week", h4("Minggu 9"), p("Membuat dashboard menggunakan software shiny")),
                          div(class = "timeline-week", h4("Minggu 10-12"), p("Mengembangkan dashboard sesuai tujuan")),
                          div(class = "timeline-week", h4("Minggu 13"), p("Membuat laporan dan video tutorial")),
                          div(class = "timeline-week", h4("Minggu 14"), p("Editing dan pengumpulan"))
                      )
                  )
              ),
              
              # About Section
              div(class = "about-section",
                  h2("Tentang Dashboard", class = "section-title"),
                  div(class = "about-content",
                      p(style = "font-size: 16px; line-height: 1.6; margin-bottom: 20px;",
                        "Dashboard Malaria Indonesia ini merupakan aplikasi web interaktif yang menampilkan pengaruh perubahan iklim terhadap sebaran kasus malaria 
                         di seluruh provinsi Indonesia dari tahun 2001 hingga 2023. Dashboard ini dikembangkan sebagai 
                         bagian dari tugas akhir mata kuliah Komputasi Statistik dengan menggunakan teknologi R Shiny."
                      ),
                      p(strong("Fitur Utama Dashboard:"), style = "font-size: 16px; margin-bottom: 15px;"),
                      tags$ul(style = "font-size: 15px; line-height: 1.6;",
                              tags$li("Peta interaktif sebaran malaria per provinsi dengan visualisasi warna"),
                              tags$li("Filter dinamis berdasarkan tahun untuk melihat perubahan temporal"),
                              tags$li("Analisis time-series yang menampilkan perubahan dari tiap periode"),
                              tags$li("Pengaruh sebab akibat perubahan iklim dengan sebaran kasus malaria"),
                              tags$li("Fungsi unduh data dalam format Excel untuk analisis lanjutan")
                      ),
                      p(style = "font-size: 15px; line-height: 1.6; margin-top: 20px; font-style: italic;",
                        "Data yang digunakan bersumber dari Kementerian Kesehatan Republik Indonesia dan 
                         telah disesuaikan dengan perubahan wilayah administratif provinsi sesuai dengan 
                         perkembangan otonomi daerah di Indonesia."
                      )
                  )
              )
          )
  )
}
## Bantuan untuk interpretasi time series 
add_interpretation_ui <- function() {
  fluidRow(
    box(
      title = "Interpretasi Analisis Time Series", 
      status = "info", 
      solidHeader = TRUE, 
      width = 12,
      div(class = "interpretation-box",
          div(class = "interpretation-title",
              icon("chart-line"),
              "Analisis Trend dan Pola"
          ),
          div(class = "interpretation-content",
              htmlOutput("trend_interpretation")
          )
      )
    )
  )
}
add_correlation_interpretation_ui <- function() {
  fluidRow(
    box(
      title = "Interpretasi Korelasi", 
      status = "success", 
      solidHeader = TRUE, 
      width = 12,
      div(class = "correlation-interpretation",
          div(class = "interpretation-title",
              icon("network-wired"),
              "Analisis Hubungan Antar Variabel"
          ),
          div(class = "interpretation-content",
              htmlOutput("correlation_interpretation")
          )
      )
    )
  )
}

create_time_series_tab <- function() {
  tabItem(tabName = "time_series",
          fluidRow(
            box(
              title = "Ringkasan Data Time Series", status = "primary", solidHeader = TRUE, width = 12,
              fluidRow(
                valueBoxOutput("total_kasus_ts", width = 3),
                valueBoxOutput("avg_suhu_ts", width = 3),
                valueBoxOutput("avg_hujan_ts", width = 3),
                valueBoxOutput("avg_lembap_ts", width = 3)
              )
            )
          ),
          
          fluidRow(
            box(
              title = "Kontrol Analisis", status = "info", solidHeader = TRUE, width = 12,
              fluidRow(
                column(4,
                       selectInput("provinsi_ts", 
                                   "Pilih Provinsi:",
                                   choices = NULL,
                                   selected = NULL)
                ),
                column(5,
                       sliderInput("tahun_range",
                                   "Rentang Tahun:",
                                   min = 2001,
                                   max = 2023,
                                   value = c(2001, 2023),
                                   step = 1,
                                   sep = "")
                ),
                column(3,
                       radioButtons("plot_type",
                                    "Jenis Plot:",
                                    choices = list("Area Chart" = "area", "Line Chart" = "line"),
                                    selected = "area",
                                    inline = TRUE),
                       checkboxInput("show_trend", "Tampilkan Garis Trend", value = FALSE),
                       # TAMBAHKAN TOMBOL DOWNLOAD DI SINI
                       br(),
                       downloadButton("download_report", "Download word", 
                                      class = "btn-primary", 
                                      icon = icon("download"))
                )
              )
            )
          ),
          
          fluidRow(
            box(title = "Time Series - Kasus Malaria", status = "danger", solidHeader = TRUE, width = 6, 
                plotlyOutput("malaria_ts_plot", height = "300px")),
            box(title = "Time Series - Suhu (°C)", status = "warning", solidHeader = TRUE, width = 6, 
                plotlyOutput("suhu_ts_plot", height = "300px"))
          ),
          
          fluidRow(
            box(title = "Time Series - Curah Hujan (mm)", status = "primary", solidHeader = TRUE, width = 6, 
                plotlyOutput("hujan_ts_plot", height = "300px")),
            box(title = "Time Series - Kelembapan (%)", status = "success", solidHeader = TRUE, width = 6, 
                plotlyOutput("lembap_ts_plot", height = "300px"))
          ),
          
          fluidRow(
            column(6, offset = 3,
                   box(title = "Analisis Korelasi", status = "warning", solidHeader = TRUE, width = 12, 
                       plotlyOutput("correlation_plot", height = "350px"))
            )
          ),
          add_interpretation_ui(),
          add_correlation_interpretation_ui()
  )
}

create_map_tab <- function(tab_name, map_output, year_input, title, legend_text) {
  tabItem(
    tabName = tab_name,
    div(class = "outer",
        leafletOutput(map_output, width = "100%", height = "100%"),
        absolutePanel(
          id = "controls",
          class = "panel panel-default",
          fixed = TRUE,
          draggable = TRUE,
          top = 60,
          left = "auto",
          right = 20,
          bottom = "auto",
          width = 330,
          height = "auto",
          
          h2(title),
          
          # Ganti selectInput jadi selectizeInput + placeholder
          selectizeInput(
            inputId = year_input,
            label = "Pilih Tahun:",
            choices = c("", listtahun),
            selected = "",
            options = list(placeholder = "Pilih Tahun")
          ),
          
          div(
            id = "legend", 
            h4("Legenda"),
            p("Abu-abu: Tidak ada data"),
            p(legend_text)
          )
        )
    )
  )
}

create_regression_tab <- function(id_prefix, title) {
  tabItem(tabName = paste0("regresi_", id_prefix),
          fluidRow(
            column(4,
                   box(
                     title = "Pengaturan Analisis", status = "primary", solidHeader = TRUE, width = 12,
                     selectInput(
                       paste0("prov_regresi_", id_prefix), "Pilih Provinsi:", 
                       choices = c("Pilih Provinsi..." = "")
                     ),
                     radioButtons(
                       paste0("model_select_", id_prefix), "Pilih Model Regresi:",
                       choices = list("Linear" = "Linear", "Polinomial Orde 2" = "Polynomial"),
                       selected = "Linear", inline = TRUE
                     ),
                     
                     # <<< PERUBAHAN DIMULAI DI SINI
                     hr(), # Tambahkan garis pemisah
                     downloadButton(
                       paste0("download_report_", id_prefix), # ID dinamis untuk tombol
                       "Download Laporan (.docx)",
                       icon = icon("file-word"),
                       class = "btn-success" # Warna tombol hijau
                     )
                     # <<< AKHIR PERUBAHAN
                   ),
                   box(
                     title = "Perbandingan Model", status = "warning", solidHeader = TRUE, width = 12,
                     DT::dataTableOutput(paste0("comparison_table_", id_prefix))
                   )
            ),
            column(8,
                   box(
                     title = paste("Grafik Regresi:", title), status = "success", solidHeader = TRUE, width = 12,
                     plotlyOutput(paste0("plot_regresi_", id_prefix), height = "500px")
                   ),
                   # Menggabungkan ringkasan dan interpretasi agar lebih rapi
                   box(
                     title = "Ringkasan & Interpretasi Hasil", status = "info", solidHeader = TRUE, width = 12,
                     # verbatimTextOutput untuk output model summary
                     verbatimTextOutput(paste0("summary_regresi_", id_prefix)),
                     hr(),
                     # htmlOutput untuk interpretasi yang sudah diformat
                     htmlOutput(paste0("interpretasi_", id_prefix))
                   )
            )
          )
  )
}

create_data_tab <- function() {
  tabItem(tabName = "data",
          fluidRow(
            column(3,
                   selectInput("pilih_prov", "Pilih Provinsi:",
                               choices = c("Semua Provinsi" = "", sort(unique(longdata$PROVINSI))),
                               multiple = TRUE)
            ),
            column(3,
                   conditionalPanel("input.pilih_prov",
                                    selectInput("pilih_tahun", "Tahun", 
                                                c("Semua Tahun" = "", listtahun), multiple = TRUE)
                   )
            )
          ),
          fluidRow(
            column(2, numericInput("minScore", "Min Kasus", min = 0, max = 100, value = 0)),
            column(2, numericInput("maxScore", "Max Kasus", min = 0, max = max_kasus, value = max_kasus)),
            column(3, br(), downloadButton("download_excel", "Unduh Data Excel", class = "btn-primary"))
          ),
          hr(),
          DT::dataTableOutput("tabel_malaria")
  )
} 

create_metadata_tab <- function() {
  tabItem(tabName = "metadata",
          div(style = "padding: 20px;",
              h1("Metadata Dashboard Malaria Indonesia", class = "main-title"),
              
              # Informasi Dataset
              h2("Informasi Dataset", class = "section-title"),
              fluidRow(
                column(6,
                       box(title = "Dataset Malaria", status = "danger", solidHeader = TRUE, width = 12, class = "custom-box",
                           div(class = "about-content",
                               tags$ul(style = "font-size: 15px; line-height: 1.6;",
                                       tags$li(strong("Sumber: "), "Kementerian Kesehatan Republik Indonesia"),
                                       tags$li(strong("Periode: "), "2001 - 2023"),
                                       tags$li(strong("Unit: "), "Jumlah kasus per provinsi per tahun"),
                                       tags$li(strong("Cakupan: "), "38 Provinsi Indonesia"),
                                       tags$li(strong("Format: "), "SPSS (.sav)"),
                                       tags$li(strong("Variabel: "), "Kasus malaria tahunan (t2001-t2023)")
                               )
                           )
                       )
                ),
                column(6,
                       box(title = "Dataset Iklim", status = "primary", solidHeader = TRUE, width = 12, class = "custom-box",
                           div(class = "about-content",
                               tags$ul(style = "font-size: 15px; line-height: 1.6;",
                                       tags$li(strong("Sumber: "), "Nasa"),
                                       tags$li(strong("Parameter: "), "Suhu (°C), Curah Hujan (mm), Kelembapan (%)"),
                                       tags$li(strong("Periode: "), "2001 - 2023"),
                                       tags$li(strong("Resolusi: "), "Data tahunan per provinsi"),
                                       tags$li(strong("Format: "), "SPSS (.sav)")
                               )
                           )
                       )
                )
              ),
              
              # Metodologi
              h2("Pengolahan Data Peta", class = "section-title"),
              div(class = "about-content",
                  tags$h4("Penyesuaian Wilayah Administratif", style = "color: #2c3e50; margin-bottom: 15px;"),
                  p(style = "font-size: 15px; line-height: 1.6; margin-bottom: 15px;",
                    "Data telah disesuaikan dengan perubahan wilayah administratif provinsi di Indonesia. Fungsi ", 
                    code("gabung_provinsi_dinamis()"), " digunakan untuk menggabungkan data provinsi yang terbentuk setelah tahun tertentu dengan provinsi induknya:"
                  ),
                  tags$ul(style = "font-size: 14px; line-height: 1.5;",
                          tags$li("Maluku Utara → Maluku (sebelum 2001)"),
                          tags$li("Banten → Jawa Barat (sebelum 2001)"),
                          tags$li("Gorontalo → Sulawesi Utara (sebelum 2001)"),
                          tags$li("Kepulauan Bangka Belitung → Sumatera Selatan (sebelum 2001)"),
                          tags$li("Kepulauan Riau → Riau (sebelum 2003)"),
                          tags$li("Papua Barat → Papua (sebelum 2004)"),
                          tags$li("Sulawesi Barat → Sulawesi Selatan (sebelum 2005)"),
                          tags$li("Kalimantan Utara → Kalimantan Timur (sebelum 2013)"),
                          tags$li("Papua Selatan → Papua (sebelum 2023)"),
                          tags$li("Papua Tengah → Papua (sebelum 2023)"),
                          tags$li("Papua Pegunungan → Papua (sebelum 2023)"),
                          tags$li("Papua Barat Daya → Papua Barat (sebelum 2023)")
                  )
              ),
              
              # Spesifikasi Teknis
              h2("Spesifikasi Teknis", class = "section-title"),
              fluidRow(
                column(4,
                       div(class = "stats-box",
                           h3("R Shiny"),
                           p("Framework Dashboard")
                       )
                ),
                column(4,
                       div(class = "stats-box",
                           h3("Leaflet"),
                           p("Library Peta Interaktif")
                       )
                ),
                column(4,
                       div(class = "stats-box",
                           h3("Plotly"),
                           p("Visualisasi Interaktif")
                       )
                )
              ),
              
              # Struktur Data
              h2("Struktur Data", class = "section-title"),
              div(class = "about-content",
                  tags$h4("Format Data Panjang (Long Format)", style = "color: #2c3e50; margin-bottom: 15px;"),
                  p(style = "font-size: 15px; line-height: 1.6; margin-bottom: 15px;",
                    "Data dikonversi dari format lebar ke format panjang untuk analisis time series dan regresi:"
                  ),
                  tags$ul(style = "font-size: 14px; line-height: 1.5;",
                          tags$li(strong("PROVINSI: "), "Nama provinsi (38 provinsi)"),
                          tags$li(strong("Tahun: "), "Tahun observasi (2001-2023)"),
                          tags$li(strong("Kasus_Malaria: "), "Jumlah kasus malaria"),
                          tags$li(strong("Suhu: "), "Suhu rata-rata tahunan (°C)"),
                          tags$li(strong("Curah_Hujan: "), "Curah hujan tahunan (mm)"),
                          tags$li(strong("Kelembapan: "), "Kelembapan relatif rata-rata (%)")
                  )
              ),
              
              # Analisis Statistik
              h2("Metode Analisis", class = "section-title"),
              fluidRow(
                column(6,
                       box(title = "Analisis Regresi", status = "success", solidHeader = TRUE, width = 12, class = "custom-box",
                           div(class = "about-content",
                               tags$ul(style = "font-size: 15px; line-height: 1.6;",
                                       tags$li(strong("Regresi Linear: "), "Y = β₀ + β₁X + ε"),
                                       tags$li(strong("Regresi Polinomial: "), "Y = β₀ + β₁X + β₂X² + ε"),
                                       tags$li(strong("Metrik Evaluasi: "), "R², Adjusted R², P-value"),
                                       tags$li(strong("Interpretasi: "), "Signifikansi statistik dan praktis")
                               )
                           )
                       )
                ),
                column(6,
                       box(title = "Analisis Time Series", status = "warning", solidHeader = TRUE, width = 12, class = "custom-box",
                           div(class = "about-content",
                               tags$ul(style = "font-size: 15px; line-height: 1.6;",
                                       tags$li(strong("Visualisasi: "), "Area chart dan line chart"),
                                       tags$li(strong("Trend Analysis: "), "Garis trend linear"),
                                       tags$li(strong("Korelasi: "), "Matriks korelasi antar variabel"),
                                       tags$li(strong("Agregasi: "), "Statistik deskriptif per provinsi")
                               )
                           )
                       )
                )
              ),
              
              # Batasan dan Catatan
              h2("Batasan dan Catatan Penting", class = "section-title"),
              div(class = "about-content",
                  tags$ul(style = "font-size: 15px; line-height: 1.6;",
                          tags$li(strong("Kualitas Data: "), "Beberapa provinsi memiliki data yang tidak lengkap untuk periode tertentu"),
                          tags$li(strong("Interpretasi Kausal: "), "Analisis menunjukkan asosiasi, bukan hubungan sebab-akibat"),
                          tags$li(strong("Resolusi Temporal: "), "Data agregat tahunan, tidak menangkap variasi musiman"),
                          tags$li(strong("Faktor Eksternal: "), "Tidak memperhitungkan faktor sosial-ekonomi dan intervensi kesehatan"),
                          tags$li(strong("Pembaruan Data: "), "Dashboard ini menggunakan data hingga tahun 2023")
                  )
              )
          )
  )
}

# Main UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Malaria Indonesia"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("HOME", tabName = "home", icon = icon("home")),
      menuItem("Time Series", tabName = "time_series", icon = icon("chart-line")), 
      menuItem("PETA", tabName = "peta", icon = icon("map"),
               menuSubItem("Peta Kasus Malaria", tabName = "peta_malaria"),
               menuSubItem("Peta Suhu", tabName = "peta_suhu"),
               menuSubItem("Peta Curah Hujan", tabName = "peta_hujan"),
               menuSubItem("Peta Kelembapan", tabName = "peta_lembap")
      ),
      menuItem("ANALISIS REGRESI", tabName = "analisis_regresi", icon = icon("project-diagram"),
               menuSubItem("Malaria vs Suhu", tabName = "regresi_suhu"),
               menuSubItem("Malaria vs Curah Hujan", tabName = "regresi_hujan"), 
               menuSubItem("Malaria vs Kelembapan", tabName = "regresi_lembap")
      ),
      menuItem("METADATA", tabName = "metadata", icon = icon("info-circle")), 
      menuItem("Unduh Data", tabName = "data", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
    
    tabItems(
      create_home_tab(),
      create_time_series_tab(),
      create_map_tab("peta_malaria", "indonesia_malaria_map", "year", "PETA MALARIA", 
                     "Warna menunjukkan tingkat kasus malaria: Kuning: Rendah → Merah: Tinggi"),
      create_map_tab("peta_suhu", "peta_suhu_map", "year_suhu", "PETA SUHU", 
                     "Warna menunjukkan tingkat suhu: putih: Sejuk → Merah: Panas"),
      create_map_tab("peta_hujan", "peta_hujan_map", "year_hujan", "PETA CURAH HUJAN", 
                     "Warna menunjukkan tingkat curah hujan: Putih: Kering → Biru: Basah"),
      create_map_tab("peta_lembap", "peta_lembap_map", "year_lembap", "PETA KELEMBAPAN", 
                     "Warna menunjukkan tingkat kelembapan: Kuning: Kering → Hijau: Lembab"),
      create_regression_tab("suhu", "Malaria vs Suhu"),
      create_regression_tab("hujan", "Malaria vs Curah Hujan"),
      create_regression_tab("lembap", "Malaria vs Kelembapan"),
      create_metadata_tab(), 
      create_data_tab()
    )
  )
)

# =====================================
# SERVER LOGIC
# =====================================

server <- function(input, output, session) {
  
  # =====================================
  # HOME TAB SERVER LOGIC
  # =====================================
  
  output$top_provinsi_table <- DT::renderDataTable({
    DT::datatable(
      top_provinsi %>%
        mutate(total_kasus = format(total_kasus, big.mark = ".")) %>%
        rename("Provinsi" = PROVINSI, "Total Kasus" = total_kasus),
      options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE, 
                     info = FALSE, paging = FALSE),
      rownames = FALSE
    )
  })
  
  # =====================================
  # TIME SERIES SERVER LOGIC
  # =====================================
  
  # Initialize province choices for time series
  observe({
    updateSelectInput(session, "provinsi_ts",
                      choices = sort(unique(longdata$PROVINSI)),
                      selected = "Papua")
  })
  
  # Reactive data for time series
  ts_data <- reactive({
    req(input$provinsi_ts, input$tahun_range)
    
    start_year <- input$tahun_range[1]
    end_year <- input$tahun_range[2]
    
    longdata %>%
      filter(PROVINSI == input$provinsi_ts,
             Tahun >= start_year,
             Tahun <= end_year) %>%
      arrange(Tahun)
  })
  
  # Summary value boxes for time series tab
  output$total_kasus_ts <- renderValueBox({
    data <- ts_data()
    total <- sum(data$Kasus_Malaria, na.rm = TRUE)
    valueBox(value = format(total, big.mark = "."), subtitle = "Total Kasus Malaria", 
             icon = icon("virus"), color = "red")
  })
  
  output$avg_suhu_ts <- renderValueBox({
    data <- ts_data()
    avg_suhu <- round(mean(data$Suhu, na.rm = TRUE), 1)
    valueBox(value = paste0(avg_suhu, "°C"), subtitle = "Rata-rata Suhu", 
             icon = icon("thermometer-half"), color = "orange")
  })
  
  output$avg_hujan_ts <- renderValueBox({
    data <- ts_data()
    avg_hujan <- round(mean(data$Curah_Hujan, na.rm = TRUE), 1)
    valueBox(value = paste0(avg_hujan, " mm"), subtitle = "Rata-rata Curah Hujan", 
             icon = icon("cloud-rain"), color = "blue")
  })
  
  output$avg_lembap_ts <- renderValueBox({
    data <- ts_data()
    avg_lembap <- round(mean(data$Kelembapan, na.rm = TRUE), 1)
    valueBox(value = paste0(avg_lembap, "%"), subtitle = "Rata-rata Kelembapan", 
             icon = icon("tint"), color = "teal")
  })
  
  # Helper function to create time series plots
  create_ts_plot <- function(data, y_var, y_label, color, plot_type, show_trend) {
    if(nrow(data) == 0 || all(is.na(data[[y_var]]))) {
      return(plotly_empty() %>% layout(title = "Tidak ada data untuk rentang yang dipilih"))
    }
    
    p <- plot_ly(data, x = ~Tahun, y = data[[y_var]], name = y_label, type = 'scatter')
    
    if(plot_type == "area") {
      p <- p %>% add_lines(fill = "tozeroy", 
                           fillcolor = paste0("rgba(", paste(col2rgb(color), collapse = ","), ",0.3)"), 
                           line = list(color = color, width = 2))
    } else if(plot_type == "line") {
      p <- p %>% add_lines(line = list(color = color, width = 2))
    }
    
    if(show_trend && nrow(data) > 2) {
      trend_data <- data %>% filter(!is.na(.data[[y_var]]))
      if(nrow(trend_data) > 2) {
        lm_model <- lm(trend_data[[y_var]] ~ trend_data$Tahun)
        p <- p %>% 
          add_lines(x = trend_data$Tahun, 
                    y = predict(lm_model),
                    name = "Trend",
                    line = list(dash = "dash", color = "#1a1a1a", width = 1))
      }
    }
    
    p %>%
      layout(
        xaxis = list(title = "Tahun"),
        yaxis = list(title = y_label),
        showlegend = show_trend,
        hovermode = "x"
      )
  }
  
  # Individual time series plots
  output$malaria_ts_plot <- renderPlotly({
    req(input$provinsi_ts)
    create_ts_plot(ts_data(), "Kasus_Malaria", "Kasus Malaria", "#d73027", input$plot_type, input$show_trend)
  })
  
  output$suhu_ts_plot <- renderPlotly({
    req(input$provinsi_ts)
    create_ts_plot(ts_data(), "Suhu", "Suhu (°C)", "#f46d43", input$plot_type, input$show_trend)
  })
  
  output$hujan_ts_plot <- renderPlotly({
    req(input$provinsi_ts)
    create_ts_plot(ts_data(), "Curah_Hujan", "Curah Hujan (mm)", "#2166ac", input$plot_type, input$show_trend)
  })
  
  output$lembap_ts_plot <- renderPlotly({
    req(input$provinsi_ts)
    create_ts_plot(ts_data(), "Kelembapan", "Kelembapan (%)", "#1b7837", input$plot_type, input$show_trend)
  })
  
  # Correlation plot
  output$correlation_plot <- renderPlotly({
    data <- ts_data()
    
    if(nrow(data) < 3) {
      return(plotly_empty() %>% layout(title = "Data tidak cukup untuk analisis korelasi"))
    }
    
    cor_data <- data %>%
      select(Kasus_Malaria, Suhu, Curah_Hujan, Kelembapan) %>%
      cor(use = "complete.obs")
    
    plot_ly(
      z = cor_data,
      x = colnames(cor_data),
      y = colnames(cor_data),
      type = "heatmap",
      colorscale = "RdBu",
      zmid = 0,
      text = round(cor_data, 2),
      texttemplate = "%{text}",
      textfont = list(size = 12)
    ) %>%
      layout(
        title = "Matriks Korelasi Variabel",
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
  }) 
  
  analyze_trend <- function(data, variable) {
    if(nrow(data) < 3 || all(is.na(data[[variable]]))) {
      return(list(trend = "unknown", slope = 0, r_squared = 0))
    }
    
    clean_data <- data %>% filter(!is.na(.data[[variable]]))
    if(nrow(clean_data) < 3) {
      return(list(trend = "unknown", slope = 0, r_squared = 0))
    }
    
    model <- lm(clean_data[[variable]] ~ clean_data$Tahun)
    slope <- coef(model)[2]
    r_squared <- summary(model)$r.squared
    
    trend <- case_when(
      abs(slope) < 0.1 ~ "stable",
      slope > 0 ~ "increasing",
      slope < 0 ~ "decreasing",
      TRUE ~ "unknown"
    )
    
    return(list(trend = trend, slope = slope, r_squared = r_squared))
  } 
  
  interpret_correlation <- function(cor_value) {
    abs_cor <- abs(cor_value)
    strength <- case_when(
      abs_cor >= 0.7 ~ "kuat",
      abs_cor >= 0.5 ~ "sedang",
      abs_cor >= 0.3 ~ "lemah",
      TRUE ~ "sangat lemah"
    )
    
    direction <- ifelse(cor_value > 0, "positif", "negatif")
    
    return(list(strength = strength, direction = direction))
  } 
  
  # Interpretasi trend time series
  output$trend_interpretation <- renderUI({
    data <- ts_data()
    province <- input$provinsi_ts
    
    if(nrow(data) < 3) {
      return(div("Data tidak cukup untuk analisis trend."))
    }
    
    # Analisis trend untuk setiap variabel
    malaria_trend <- analyze_trend(data, "Kasus_Malaria")
    suhu_trend <- analyze_trend(data, "Suhu")
    hujan_trend <- analyze_trend(data, "Curah_Hujan")
    lembap_trend <- analyze_trend(data, "Kelembapan")
    
    # Fungsi helper untuk trend indicator
    trend_indicator <- function(trend) {
      class_name <- paste0("trend-", trend)
      icon_name <- case_when(
        trend == "increasing" ~ "arrow-up",
        trend == "decreasing" ~ "arrow-down",
        trend == "stable" ~ "minus",
        TRUE ~ "question"
      )
      
      text <- case_when(
        trend == "increasing" ~ "Meningkat",
        trend == "decreasing" ~ "Menurun",
        trend == "stable" ~ "Stabil",
        TRUE ~ "Tidak Jelas"
      )
      
      span(class = paste("trend-indicator", class_name),
           icon(icon_name), " ", text)
    }
    
    # Interpretasi statistik
    year_range <- paste(min(data$Tahun), "-", max(data$Tahun))
    
    div(
      h4(paste("Analisis Trend Provinsi", province, "Periode", year_range)),
      
      div(class = "interpretation-highlight",
          p(strong("Kasus Malaria: "), trend_indicator(malaria_trend$trend)),
          p(paste("Dalam periode", year_range, "kasus malaria di", province, 
                  switch(malaria_trend$trend,
                         "increasing" = "menunjukkan tren peningkatan.",
                         "decreasing" = "menunjukkan tren penurunan.",
                         "stable" = "relatif stabil.",
                         "menunjukkan pola yang tidak jelas.")))
      ),
      
      h5("Faktor Iklim:"),
      tags$ul(
        tags$li(strong("Suhu: "), trend_indicator(suhu_trend$trend)),
        tags$li(strong("Curah Hujan: "), trend_indicator(hujan_trend$trend)),
        tags$li(strong("Kelembapan: "), trend_indicator(lembap_trend$trend))
      ),
      
      if(malaria_trend$r_squared > 0.3) {
        div(
          p(strong("Catatan: "), 
            "Tren kasus malaria menunjukkan pola yang cukup konsisten (R² = ", 
            round(malaria_trend$r_squared, 3), ") yang mengindikasikan adanya faktor sistematis yang mempengaruhi perubahan kasus.")
        )
      }
    )
  })
  
  # Interpretasi korelasi
  output$correlation_interpretation <- renderUI({
    data <- ts_data()
    province <- input$provinsi_ts
    
    if(nrow(data) < 3) {
      return(div("Data tidak cukup untuk analisis korelasi."))
    }
    
    # Validasi dan bersihkan data
    cor_vars <- c("Kasus_Malaria", "Suhu", "Curah_Hujan", "Kelembapan")
    
    # Periksa apakah kolom ada dan numerik
    missing_cols <- setdiff(cor_vars, names(data))
    if(length(missing_cols) > 0) {
      return(div(paste("Kolom tidak ditemukan:", paste(missing_cols, collapse = ", "))))
    }
    
    # Konversi ke numeric dan filter data valid
    clean_data <- data %>%
      select(all_of(cor_vars)) %>%
      mutate(across(everything(), as.numeric)) %>%
      filter(complete.cases(.))
    
    if(nrow(clean_data) < 3) {
      return(div("Data numerik tidak cukup untuk analisis korelasi."))
    }
    
    # Hitung korelasi dengan error handling
    cor_data <- tryCatch({
      cor(clean_data, use = "complete.obs")
    }, error = function(e) {
      return(NULL)
    })
    
    if(is.null(cor_data)) {
      return(div("Gagal menghitung korelasi. Periksa data input."))
    }
    
    # Interpretasi korelasi utama dengan validasi
    suhu_cor_val <- cor_data["Kasus_Malaria", "Suhu"]
    hujan_cor_val <- cor_data["Kasus_Malaria", "Curah_Hujan"]
    lembap_cor_val <- cor_data["Kasus_Malaria", "Kelembapan"]
    suhu_hujan_cor_val <- cor_data["Suhu", "Curah_Hujan"]
    
    # Validasi nilai korelasi
    if(any(is.na(c(suhu_cor_val, hujan_cor_val, lembap_cor_val)))) {
      return(div("Beberapa nilai korelasi tidak dapat dihitung."))
    }
    
    suhu_cor <- interpret_correlation(suhu_cor_val)
    hujan_cor <- interpret_correlation(hujan_cor_val)
    lembap_cor <- interpret_correlation(lembap_cor_val)
    
    div(
      h4(paste("Analisis Korelasi Provinsi", province)),
      
      div(
        h5("Hubungan Kasus Malaria dengan Faktor Iklim:"),
        tags$ul(
          tags$li(
            strong("Suhu: "),
            paste("Korelasi", suhu_cor$strength, suhu_cor$direction, 
                  "(r =", round(suhu_cor_val, 3), ")")
          ),
          tags$li(
            strong("Curah Hujan: "),
            paste("Korelasi", hujan_cor$strength, hujan_cor$direction,
                  "(r =", round(hujan_cor_val, 3), ")")
          ),
          tags$li(
            strong("Kelembapan: "),
            paste("Korelasi", lembap_cor$strength, lembap_cor$direction,
                  "(r =", round(lembap_cor_val, 3), ")")
          )
        )
      ),
      
      div(class = "interpretation-highlight",
          h5("Interpretasi Epidemiologi:"),
          p(
            if(suhu_cor_val > 0.3) {
              "Suhu yang lebih tinggi cenderung meningkatkan kasus malaria, kemungkinan karena mempercepat siklus hidup nyamuk Anopheles."
            } else if(suhu_cor_val < -0.3) {
              "Suhu yang lebih rendah cenderung mengurangi kasus malaria, kemungkinan karena menghambat aktivitas nyamuk."
            } else {
              "Tidak ada hubungan yang kuat antara suhu dan kasus malaria di provinsi ini."
            }
          ),
          p(
            if(hujan_cor_val > 0.3) {
              "Curah hujan yang tinggi cenderung meningkatkan kasus malaria, kemungkinan karena menciptakan tempat perindukan nyamuk."
            } else if(hujan_cor_val < -0.3) {
              "Curah hujan yang rendah cenderung mengurangi kasus malaria, kemungkinan karena mengurangi habitat nyamuk."
            } else {
              "Tidak ada hubungan yang kuat antara curah hujan dan kasus malaria di provinsi ini."
            }
          ), 
          p(
            if(lembap_cor_val > 0.3) {
              "Kelembapan udara yang tinggi cenderung meningkatkan kasus malaria. Ini karena kelembapan mendukung kelangsungan hidup nyamuk dewasa, sehingga mereka punya lebih banyak waktu untuk menularkan parasit."
            } else if(lembap_cor_val < -0.3) {
              "Kelembapan udara yang rendah cenderung menekan kasus malaria. Udara yang kering dapat menyebabkan dehidrasi dan kematian pada nyamuk, sehingga mengurangi populasi dan laju penularan."
            } else {
              "Tidak ditemukan hubungan yang signifikan secara statistik antara tingkat kelembapan udara dan jumlah kasus malaria di provinsi ini."
            }
          )
      ),
      
      if(abs(suhu_hujan_cor_val) > 0.5) {
        div(
          p(strong("Catatan: "), 
            "Terdapat hubungan yang cukup kuat antara suhu dan curah hujan (r = ", 
            round(suhu_hujan_cor_val, 3), 
            "), yang menunjukkan pola iklim yang saling terkait di wilayah ini.")
        )
      }
    )
  }) 
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("Laporan_Malaria_", gsub(" ", "_", input$provinsi_ts), "_", input$tahun_range[1], "-", input$tahun_range[2], ".docx")
    },
    content = function(file) {
      id <- showNotification("Membuat laporan Word...", duration = NULL, closeButton = FALSE)
      on.exit(removeNotification(id))
      
      params <- list(
        province = input$provinsi_ts,
        year_range = paste(input$tahun_range[1], "-", input$tahun_range[2]),
        ts_data = ts_data()
      )
      
      rmarkdown::render(
        "report_template.Rmd",
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  # =====================================
  # MAP SERVER LOGIC
  # =====================================
  
  # Helper function to format value display
  format_value_display <- function(value, unit) {
    if(unit == "") {
      ifelse(is.na(value), "Tidak Ada Data", format(value, big.mark = ".", scientific = FALSE))
    } else if(unit == "°C") {
      ifelse(is.na(value), "Tidak Ada Data", paste0(round(value, 1), "°C"))
    } else if(unit == " mm") {
      ifelse(is.na(value), "Tidak Ada Data", paste0(round(value, 1), " mm"))
    } else if(unit == "%") {
      ifelse(is.na(value), "Tidak Ada Data", paste0(round(value, 1), "%"))
    }
  }
  
  # Helper function to create map observers
  create_map_observer <- function(map_output, year_input, data_source, color_palette, unit = "", legend_title_prefix = "") {
    # Initialize map
    output[[map_output]] <- renderLeaflet({
      leaflet() %>%
        setView(lng = 118, lat = -2, zoom = 5) %>%
        addProviderTiles(providers$CartoDB.Positron)
    })
    
    # Observe year changes
    observe({
      req(input[[year_input]])
      
      year <- input[[year_input]]
      year_column <- paste0("t", year)
      
      if (!year_column %in% colnames(data_source)) return()
      
      peta_tahun_ini <- gabung_provinsi_dinamis(provinsi_indonesia, year)
      
      data_tahun <- data_source %>%
        select(Provinsi = PROVINSI, value = all_of(year_column))
      
      peta_tahun_ini <- peta_tahun_ini %>%
        left_join(data_tahun, by = c("Provinsi_Gabungan" = "Provinsi"))
      
      # Palet warna utama untuk poligon
      color_pal <- colorNumeric(
        palette = color_palette,
        domain = peta_tahun_ini$value,
        na.color = "gray",
        reverse = if(color_palette == "RdYlBu") TRUE else FALSE
      )
      
      value_display <- format_value_display(peta_tahun_ini$value, unit)
      
      popup_text <- paste0(
        "<strong>", peta_tahun_ini$Provinsi_Gabungan, "</strong><br>",
        legend_title_prefix, " (", year, "): ", value_display
      )
      
      # --- Logika Legenda ---
      leg_values <- na.omit(peta_tahun_ini$value)
      legend_title <- paste0("<strong>", paste(legend_title_prefix, year, unit), "</strong>")
      na_html <- "<div class='legend-item'><i style='background:gray'></i> <span>Tidak Ada Data</span></div>"
      
      legend_html <- if (length(leg_values) > 1 && length(unique(leg_values)) > 1) {
        breaks <- pretty(leg_values, n = 5)
        
        bpal <- colorBin(
          palette = color_palette,
          domain = leg_values,
          bins = breaks,
          reverse = if(color_palette == "RdYlBu") TRUE else FALSE,
          pretty = FALSE
        )
        
        if (length(breaks) < 2) {
          paste(legend_title, na_html, sep = "<br>")
        } else {
          
          # --- PERBAIKAN FORMAT ANGKA ---
          if(unit == "") { # Untuk kasus Malaria
            num_format <- function(x) format(round(x, 0), big.mark = ".", scientific = FALSE, trim = TRUE)
          } else { # Untuk Suhu, dll.
            num_format <- function(x) format(round(x, 1), nsmall = 1, big.mark = ".", decimal.mark = ",")
          }
          
          labels <- paste0(num_format(breaks[-length(breaks)]), " – ", num_format(breaks[-1]))
          colors <- bpal(breaks[-length(breaks)])
          
          labels_html <- paste0(
            "<div class='legend-item'><i style='background:", colors, "'></i> <span>", labels, "</span></div>",
            collapse = ""
          )
          paste(legend_title, na_html, labels_html, sep = "<br>")
        }
      } else {
        paste(legend_title, na_html, sep = "<br>")
      }
      
      # --- Memperbarui Peta ---
      leafletProxy(map_output, data = peta_tahun_ini) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(
          fillColor = ~color_pal(value),
          fillOpacity = 0.7,
          color = "black",
          weight = 1,
          popup = popup_text,
          highlightOptions = highlightOptions(
            weight = 4,
            color = "#FF0000",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        ) %>%
        addControl(HTML(legend_html), position = "bottomright", className = "info legend")
    })
  }
  
  # Create all map observers
  create_map_observer("indonesia_malaria_map", "year", data_malaria, "YlOrRd", "", "Kasus Malaria")
  create_map_observer("peta_suhu_map", "year_suhu", data_suhu, "Reds", "°C", "Suhu")
  create_map_observer("peta_hujan_map", "year_hujan", data_hujan, "Blues", " mm", "Curah Hujan")
  create_map_observer("peta_lembap_map", "year_lembap", data_lembap, "YlGn", "%", "Kelembapan")
  
  # =====================================
  # REGRESSION SERVER LOGIC
  # =====================================
  
  # Initialize province choices for regression
  observe({
    provinsi_choices <- c("Pilih Provinsi..." = "", sort(unique(longdata$PROVINSI)))
    
    updateSelectInput(session, "prov_regresi_suhu", choices = provinsi_choices)
    updateSelectInput(session, "prov_regresi_hujan", choices = provinsi_choices)
    updateSelectInput(session, "prov_regresi_lembap", choices = provinsi_choices)
  })
  
  # Helper function to create regression server logic
  create_regression_server <- function(id_prefix, x_var, x_label, plot_color, title) { # <<< TAMBAHKAN argumen 'title'
    
    # Bagian ini tetap sama
    analysis_results <- eventReactive(input[[paste0("prov_regresi_", id_prefix)]], {
      req(input[[paste0("prov_regresi_", id_prefix)]] != "")
      data_prov <- longdata %>% filter(PROVINSI == input[[paste0("prov_regresi_", id_prefix)]])
      analyze_both_regressions(data_prov, x_var, "Kasus_Malaria")
    })
    
    # Semua output untuk tampilan di dasbor tetap sama
    output[[paste0("plot_regresi_", id_prefix)]] <- renderPlotly({
      results <- analysis_results()
      if(!results$sufficient_data) return(plotly_empty() %>% layout(title = results$message))
      
      model_choice <- input[[paste0("model_select_", id_prefix)]]
      selected_model <- if (model_choice == "Linear") results$linear$model else results$polynomial$model
      
      fitted_values <- fitted(selected_model)
      fitted_values[fitted_values < 0] <- 0
      sorted_fitted_values <- fitted_values[order(results$data[[x_var]])]
      
      plot_ly(data = results$data, x = as.formula(paste0("~", x_var)), y = ~Kasus_Malaria, 
              type = 'scatter', mode = 'markers', 
              marker = list(size = 8, color = plot_color, opacity = 0.7), 
              name = "Data Observasi") %>%
        add_lines(x = ~sort(get(x_var)), y = sorted_fitted_values, 
                  name = paste("Regresi", model_choice), 
                  line = list(color = '#c0392b', width = 3)) %>%
        layout(title = paste("Hubungan", x_label, "vs Kasus Malaria"), 
               xaxis = list(title = x_label), 
               yaxis = list(title = "Kasus Malaria", rangemode = 'tozero'), 
               legend = list(orientation = 'h', y = -0.2, x = 0.5, xanchor = 'center'))
    })
    
    output[[paste0("comparison_table_", id_prefix)]] <- DT::renderDataTable({
      results <- analysis_results()
      req(results$sufficient_data)
      
      comparison_df <- data.frame(
        Metrik = c("R-squared", "Adjusted R-squared", "P-value Model"), 
        Linear = c(results$linear$glance$r.squared, results$linear$glance$adj.r.squared, results$linear$glance$p.value), 
        `Polinomial Orde 2` = c(results$polynomial$glance$r.squared, results$polynomial$glance$adj.r.squared, results$polynomial$glance$p.value), 
        check.names = FALSE
      )
      
      DT::datatable(comparison_df, options = list(dom = 't', ordering = FALSE), 
                    rownames = FALSE, caption = "Tabel Perbandingan Metrik Model") %>% 
        formatSignif(columns = c("Linear", "Polinomial Orde 2"), digits = 4)
    })
    
    output[[paste0("interpretasi_", id_prefix)]] <- renderUI({
      results <- analysis_results()
      req(results$sufficient_data)
      model_choice <- input[[paste0("model_select_", id_prefix)]]
      selected_result <- if (model_choice == "Linear") results$linear else results$polynomial
      HTML(create_single_interpretation(selected_result, model_choice))
    })
    
    output[[paste0("summary_regresi_", id_prefix)]] <- renderPrint({
      results <- analysis_results()
      req(results$sufficient_data)
      model_choice <- input[[paste0("model_select_", id_prefix)]]
      print(if (model_choice == "Linear") results$linear$summary else results$polynomial$summary)
    })
    
    ### DOWNLOAD LAPORAN 
    output[[paste0("download_report_", id_prefix)]] <- downloadHandler(
      filename = function() {
        paste0("Laporan_Regresi_", 
               gsub(" ", "_", input[[paste0("prov_regresi_", id_prefix)]]), "_", 
               format(Sys.Date(), "%Y%m%d"), ".docx")
      },
      content = function(file) {
        id <- showNotification("Membuat laporan Word...", duration = NULL, closeButton = FALSE)
        on.exit(removeNotification(id))
        
        # Ambil data yang diperlukan
        results <- analysis_results()
        req(results$sufficient_data)
        
        # Parameter untuk template
        params <- list(
          province = input[[paste0("prov_regresi_", id_prefix)]],
          analysis_title = title,
          model_type = input[[paste0("model_select_", id_prefix)]],
          regression_data = results$data,
          x_var = x_var
        )
        
        # Render ke Word
        rmarkdown::render(
          "regression_report_template.Rmd",
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )
  }
  
  # Create regression servers
  # Create regression servers
  create_regression_server("suhu", "Suhu", "Suhu (°C)", "#3498db", title = "Malaria vs Suhu")
  create_regression_server("hujan", "Curah_Hujan", "Curah Hujan (mm)", "#2ecc71", title = "Malaria vs Curah Hujan")
  create_regression_server("lembap", "Kelembapan", "Kelembapan (%)", "#f39c12", title = "Malaria vs Kelembapan")
  
  # =====================================
  # DATA DOWNLOAD SERVER LOGIC
  # =====================================
  
  # Filtered data reactive
  filtered_data <- reactive({
    df <- longdata
    
    if (length(input$pilih_prov) > 0) { 
      df <- df %>% filter(PROVINSI %in% input$pilih_prov) 
    }
    if (length(input$pilih_tahun) > 0) { 
      df <- df %>% filter(Tahun %in% as.integer(input$pilih_tahun)) 
    }
    df <- df %>% filter(is.na(Kasus_Malaria) | (Kasus_Malaria >= input$minScore & Kasus_Malaria <= input$maxScore))
    
    return(df)
  })
  
  # Data table output
  output$tabel_malaria <- DT::renderDataTable({
    DT::datatable(filtered_data(), rownames = FALSE)
  })
  
  # Download handler
  output$download_excel <- downloadHandler(
    filename = function() paste0("data_malaria_", Sys.Date(), ".xlsx"),
    content = function(file) writexl::write_xlsx(filtered_data(), path = file)
  )
}
# =====================================
# RUN APPLICATION
# =====================================

shinyApp(ui = ui, server = server)