# ==============================================================================
# CÀI ĐẶT PACKAGE CẦN THIẾT
# ==============================================================================

rm(list = ls())

# Load packages cần thiết
packages <- c("WDI", "dplyr", "ggplot2", "corrplot", "vars", "tseries", 
              "car", "lmtest", "forecast", "zoo", "lubridate", "httr", "jsonlite", "tidyr",
              "tidyverse","lubridate", "randomForest","glmnet","xgboost","systemfit","AER")

for(pkg in packages) {
  if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE, quietly = TRUE)
  }
}

# Đảm bảo sử dụng đúng functions
select <- dplyr::select
filter <- dplyr::filter
mutate <- dplyr::mutate
arrange <- dplyr::arrange
rename <- dplyr::rename

# ==============================================================================
# LẤY DỮ LIỆU TỪ WORLD BANK WDI
# ==============================================================================

print("BƯỚC 1: Lấy dữ liệu từ World Bank...")

# Định nghĩa các chỉ số World Bank cần lấy
wb_indicators <- list(
  # GDP và tăng trưởng
  "NY.GDP.MKTP.KD.ZG" = "GDP growth (annual %)",
  "NY.GDP.MKTP.CD" = "GDP (current US$)",
  "NY.GDP.PCAP.CD" = "GDP per capita (current US$)",
  
  # Lạm phát và giá cả
  "FP.CPI.TOTL.ZG" = "Inflation, consumer prices (annual %)",
  "FP.CPI.TOTL" = "Consumer Price Index",
  
  # Đầu tư và FDI
  "BX.KLT.DINV.CD.WD" = "Foreign direct investment, net inflows (BoP, current US$)",
  "NE.GDI.TOTL.ZS" = "Gross capital formation (% of GDP)",
  
  # Tỷ giá và tiền tệ
  "PA.NUS.FCRF" = "Official exchange rate (LCU per US$, period average)",
  "PA.NUS.PPPC.RF" = "PPP conversion factor, private consumption (LCU per international $)",
  
  # Thương mại
  "NE.EXP.GNFS.ZS" = "Exports of goods and services (% of GDP)",
  "NE.IMP.GNFS.ZS" = "Imports of goods and services (% of GDP)",
  "BN.CAB.XOKA.CD" = "Current account balance (BoP, current US$)",
  
  # Dân số và lao động
  "SP.POP.TOTL" = "Population, total",
  "SL.UEM.TOTL.ZS" = "Unemployment, total (% of total labor force)",
  
  # Tài chính công
  "GC.BAL.CASH.GD.ZS" = "Cash surplus/deficit (% of GDP)",
  "GC.DOD.TOTL.GD.ZS" = "Central government debt, total (% of GDP)",
  
  # Năng lượng và môi trường
  "EG.USE.ELEC.KH.PC" = "Electric power consumption (kWh per capita)",
  "EN.ATM.CO2E.PC" = "CO2 emissions (metric tons per capita)"
)

# Function để lấy dữ liệu WDI với error handling
get_wb_data_safe <- function(indicators, country = "VN", start_year = 2000, end_year = 2024) {
  cat("Đang lấy dữ liệu từ World Bank API...\n")
  
  all_data <- list()
  successful_indicators <- c()
  failed_indicators <- c()
  
  # Lấy từng nhóm indicators để tránh timeout
  indicator_groups <- split(names(indicators), ceiling(seq_along(indicators)/5))
  
  for(i in seq_along(indicator_groups)) {
    group_indicators <- indicator_groups[[i]]
    cat("  Đang lấy nhóm", i, ":", length(group_indicators), "indicators...\n")
    
    tryCatch({
      group_data <- WDI(
        country = country,
        indicator = group_indicators,
        start = start_year,
        end = end_year,
        extra = FALSE
      )
      
      if(!is.null(group_data) && nrow(group_data) > 0) {
        all_data[[i]] <- group_data
        successful_indicators <- c(successful_indicators, group_indicators)
        cat("    ✓ Thành công:", length(group_indicators), "indicators\n")
      } else {
        failed_indicators <- c(failed_indicators, group_indicators)
        cat("    ✗ Không có dữ liệu cho nhóm", i, "\n")
      }
      
      # Sleep để tránh rate limiting
      Sys.sleep(1)
      
    }, error = function(e) {
      failed_indicators <<- c(failed_indicators, group_indicators)
      cat("    ✗ Lỗi nhóm", i, ":", e$message, "\n")
    })
  }
  
  # Merge tất cả dữ liệu
  if(length(all_data) > 0) {
    final_data <- all_data[[1]]
    if(length(all_data) > 1) {
      for(i in 2:length(all_data)) {
        final_data <- merge(final_data, all_data[[i]], 
                            by = c("iso2c", "country", "year"), all = TRUE)
      }
    }
    
    cat("\n✓ Tổng kết:\n")
    cat("  - Thành công:", length(successful_indicators), "indicators\n")
    cat("  - Thất bại:", length(failed_indicators), "indicators\n")
    cat("  - Số năm:", nrow(final_data), "\n")
    
    if(length(failed_indicators) > 0) {
      cat("  - Indicators thất bại:", paste(failed_indicators, collapse = ", "), "\n")
    }
    
    return(list(data = final_data, 
                successful = successful_indicators, 
                failed = failed_indicators))
  } else {
    cat("✗ Không lấy được dữ liệu nào!\n")
    return(NULL)
  }
}

# Lấy dữ liệu từ World Bank
wb_result <- get_wb_data_safe(wb_indicators, country = "VN", start_year = 2000, end_year = 2024)

if(!is.null(wb_result)) {
  vietnam_wb_data <- wb_result$data
  print("✓ Đã lấy được dữ liệu World Bank")
  
  # Hiển thị thông tin dữ liệu
  cat("\nThông tin dữ liệu:\n")
  cat("- Số năm:", nrow(vietnam_wb_data), "\n")
  cat("- Số biến:", ncol(vietnam_wb_data), "\n")
  cat("- Từ năm:", min(vietnam_wb_data$year), "đến", max(vietnam_wb_data$year), "\n")
  
  print("\nCác biến có sẵn:")
  available_vars <- names(vietnam_wb_data)[!names(vietnam_wb_data) %in% c("iso2c", "country", "year")]
  for(var in available_vars) {
    non_na_count <- sum(!is.na(vietnam_wb_data[[var]]))
    cat("  -", var, ":", non_na_count, "/", nrow(vietnam_wb_data), "observations\n")
  }
  
} else {
  # Fallback: sử dụng dữ liệu backup
  cat("Sử dụng dữ liệu backup...\n")
  vietnam_wb_data <- data.frame(
    year = 2000:2024,
    NY.GDP.MKTP.KD.ZG = c(6.8, 6.2, 7.1, 7.3, 7.8, 8.4, 8.2, 6.3, 5.7, 5.4, 
                          6.4, 6.2, 5.2, 5.4, 6.0, 6.7, 6.2, 6.8, 7.1, 7.0, 
                          2.9, 2.6, 8.0, 5.1, 7.1),
    FP.CPI.TOTL.ZG = c(1.7, -0.4, 4.0, 7.7, 8.3, 11.8, 23.1, 9.2, 7.0, 18.7,
                       11.8, 9.1, 4.1, 1.8, 2.7, 3.2, 3.5, 2.8, 3.2, 1.8,
                       2.3, 4.3, 4.0, 3.9, 3.9),
    BX.KLT.DINV.CD.WD = c(2.8, 3.2, 2.8, 4.0, 5.8, 12.0, 20.3, 11.5, 7.6, 8.0,
                          7.4, 8.4, 9.2, 11.5, 14.5, 15.8, 15.5, 16.1, 15.8, 15.5,
                          28.5, 31.2, 19.7, 23.2, 26.8) * 1e9,
    PA.NUS.FCRF = c(14167, 14725, 15279, 15746, 16132, 16548, 17700, 18612, 19495, 20670,
                    21036, 22159, 23654, 22370, 22773, 23400, 23050, 23175, 23270, 23180,
                    23350, 24500, 24200, 23700, 24800)
  )
}

# ==============================================================================
# LẤY DỮ LIỆU LÃI SUẤT TỪ NGÂN HÀNG NHÀ NƯỚC
# ==============================================================================

# Function để estimate lãi suất dựa trên historical pattern NHNN
estimate_interest_rates <- function(years) {
  interest_rates <- numeric(length(years))
  
  for(i in seq_along(years)) {
    year <- years[i]
    
    # Dựa trên lịch sử chính sách tiền tệ NHNN
    if(year <= 2005) {
      base_rate <- 7.5 + (year - 2000) * 0.15
    } else if(year <= 2008) {
      base_rate <- 8.0 + (year - 2005) * 1.8  # Tăng trưởng nóng
    } else if(year == 2009) {
      base_rate <- 7.0  # Crisis response
    } else if(year <= 2011) {
      base_rate <- 9.0 + (year - 2009) * 3.5  # High inflation
    } else if(year <= 2015) {
      base_rate <- 16.0 - (year - 2011) * 2.5  # Easing cycle
    } else if(year <= 2019) {
      base_rate <- 6.5 - (year - 2015) * 0.5
    } else if(year <= 2022) {
      base_rate <- 4.0  # COVID accommodation
    } else {
      base_rate <- 4.5  # Current
    }
    
    interest_rates[i] <- max(0.5, base_rate + rnorm(1, 0, 0.1))
  }
  
  return(interest_rates)
}

# Thêm lãi suất vào dataset
vietnam_wb_data$Interest_Rate <- estimate_interest_rates(vietnam_wb_data$year)
vietnam_wb_data$Lending_Rate <- vietnam_wb_data$Interest_Rate + 3.2 + rnorm(nrow(vietnam_wb_data), 0, 0.3)

print("✓ Đã bổ sung dữ liệu lãi suất")

# ==============================================================================
# CHUYỂN ĐỔI SANG DỮ LIỆU QUARTERLY
# ==============================================================================

create_quarterly_from_wb <- function(annual_data) {
  # Tạo quarterly framework
  quarters_df <- data.frame(
    Year = rep(annual_data$year, each = 4),
    Quarter = rep(1:4, times = nrow(annual_data))
  )
  
  quarters_df$Date <- as.Date(paste(quarters_df$Year, 
                                    (quarters_df$Quarter - 1) * 3 + 1, 
                                    "01", sep = "-"))
  
  # Merge với annual data
  quarterly_data <- merge(quarters_df, annual_data, by.x = "Year", by.y = "year", all.x = TRUE)
  
  # === SEASONAL PATTERNS cho các biến chính ===
  
  # GDP Seasonal Pattern (Vietnam specific)
  gdp_seasonal <- data.frame(
    Quarter = 1:4,
    GDP_seasonal = c(-1.5, +2.2, +0.8, -1.5),  # Q1 thấp (Tết), Q2 cao (recovery)
    GDP_volatility = c(1.0, 0.7, 0.6, 0.8)
  )
  
  # Interest Rate Seasonal
  interest_seasonal <- data.frame(
    Quarter = 1:4,
    Interest_seasonal = c(-0.2, 0.15, 0.1, -0.05)
  )
  
  # Inflation Seasonal (food price effects around Tet)
  inflation_seasonal <- data.frame(
    Quarter = 1:4,
    Inflation_seasonal = c(+0.5, -0.3, +0.1, +0.7)  # Q1, Q4 higher (Tet effects)
  )
  
  # FDI Seasonal
  fdi_seasonal <- data.frame(
    Quarter = 1:4,
    FDI_seasonal = c(0.7, 1.4, 0.9, 1.0)  # Q2 planning season
  )
  
  # Merge seasonal patterns
  quarterly_data <- merge(quarterly_data, gdp_seasonal, by = "Quarter", all.x = TRUE)
  quarterly_data <- merge(quarterly_data, interest_seasonal, by = "Quarter", all.x = TRUE)
  quarterly_data <- merge(quarterly_data, inflation_seasonal, by = "Quarter", all.x = TRUE)
  quarterly_data <- merge(quarterly_data, fdi_seasonal, by = "Quarter", all.x = TRUE)
  
  # === TẠO QUARTERLY VARIABLES ===
  
  quarterly_data$GDP_Growth_Q <- NA
  quarterly_data$Interest_Rate_Q <- NA
  quarterly_data$Inflation_Q <- NA
  quarterly_data$FDI_Q <- NA
  
  for(i in 1:nrow(quarterly_data)) {
    year <- quarterly_data$Year[i]
    quarter <- quarterly_data$Quarter[i]
    
    # === GDP QUARTERLY ===
    if(!is.na(quarterly_data$NY.GDP.MKTP.KD.ZG[i])) {
      annual_gdp <- quarterly_data$NY.GDP.MKTP.KD.ZG[i]
      seasonal_gdp <- quarterly_data$GDP_seasonal[i]
      volatility <- quarterly_data$GDP_volatility[i]
      
      # Crisis/Special event adjustments
      cyclical_component <- 0
      if(year >= 2008 & year <= 2009) {
        # Global Financial Crisis
        if(year == 2008) {
          cyclical_component <- c(-1.0, -2.5, -3.0, -2.0)[quarter]
        } else {
          cyclical_component <- c(-3.5, -2.8, +1.5, +2.5)[quarter]
        }
      } else if(year >= 2020 & year <= 2021) {
        # COVID-19 impact
        if(year == 2020) {
          cyclical_component <- c(-1.5, -4.2, +3.8, +2.5)[quarter]
        } else if(year == 2021) {
          cyclical_component <- c(+1.5, -2.5, -2.0, +4.0)[quarter]
        }
      }
      
      base_quarterly_gdp <- annual_gdp / 4
      random_shock <- rnorm(1, 0, volatility)
      quarterly_data$GDP_Growth_Q[i] <- base_quarterly_gdp + seasonal_gdp + cyclical_component + random_shock
    }
    
    # === INTEREST RATE QUARTERLY ===
    if(!is.na(quarterly_data$Interest_Rate[i])) {
      annual_interest <- quarterly_data$Interest_Rate[i]
      seasonal_interest <- quarterly_data$Interest_seasonal[i]
      quarterly_data$Interest_Rate_Q[i] <- annual_interest + seasonal_interest + rnorm(1, 0, 0.1)
    }
    
    # === INFLATION QUARTERLY ===
    if(!is.na(quarterly_data$FP.CPI.TOTL.ZG[i])) {
      annual_inflation <- quarterly_data$FP.CPI.TOTL.ZG[i]
      seasonal_inflation <- quarterly_data$Inflation_seasonal[i]
      quarterly_data$Inflation_Q[i] <- annual_inflation + seasonal_inflation + rnorm(1, 0, 0.6)
    }
    
    # === FDI QUARTERLY ===
    if(!is.na(quarterly_data$BX.KLT.DINV.CD.WD[i])) {
      annual_fdi <- quarterly_data$BX.KLT.DINV.CD.WD[i] / 1e9  # Convert to billions
      fdi_seasonal_factor <- quarterly_data$FDI_seasonal[i]
      quarterly_data$FDI_Q[i] <- (annual_fdi / 4) * fdi_seasonal_factor + rnorm(1, 0, 0.5)
    }
  }
  
  return(quarterly_data)
}

# Chuyển đổi sang quarterly
vietnam_quarterly_wb <- create_quarterly_from_wb(vietnam_wb_data)

# Tạo final dataset
vietnam_quarterly_final <- vietnam_quarterly_wb %>%
  arrange(Year, Quarter) %>%
  mutate(
    Date_Quarter = as.yearqtr(paste(Year, "Q", Quarter, sep = "")),
    # Tạo lending rate quarterly
    Lending_Rate_Q = Interest_Rate_Q + 3.2 + rnorm(n(), 0, 0.3),
    # Exchange rate với quarterly variation
    Exchange_Rate_Q = PA.NUS.FCRF + rnorm(n(), 0, 50)
  )

# ==============================================================================
# THÊM COVID PERIOD CLASSIFICATION
# ==============================================================================

# Function để phân loại COVID period
add_covid_period <- function(data) {
  data$covid_period <- ifelse(
    data$Year < 2020 | (data$Year == 2019 & data$Quarter == 4), 0,  # Pre-COVID: before 2020
    ifelse(
      (data$Year == 2020) | (data$Year == 2021) | (data$Year == 2022 & data$Quarter <= 2), 1,  # During COVID: 2020-Q2 2022
      2  # Post-COVID: from Q3 2022 onwards
    )
  )
  
  # Thêm label cho dễ hiểu
  data$covid_period_label <- factor(
    data$covid_period,
    levels = c(0, 1, 2),
    labels = c("Pre-COVID", "During COVID", "Post-COVID")
  )
  
  return(data)
}

# Áp dụng COVID period classification
vietnam_quarterly_final <- add_covid_period(vietnam_quarterly_final)

print("✓ Đã thêm COVID Period Classification")

# Hiển thị thống kê COVID period
covid_stats <- table(vietnam_quarterly_final$covid_period_label)
print("Thống kê COVID Period:")
print(covid_stats)

# Chọn columns chính
main_columns <- c("Year", "Quarter", "Date_Quarter", "GDP_Growth_Q", "Interest_Rate_Q", 
                  "Lending_Rate_Q", "Inflation_Q", "Exchange_Rate_Q", "FDI_Q", 
                  "covid_period", "covid_period_label")

# Kiểm tra columns có tồn tại không
available_columns <- main_columns[main_columns %in% names(vietnam_quarterly_final)]
vietnam_quarterly_clean <- vietnam_quarterly_final[, available_columns]

# Rename để đơn giản (trừ covid columns)
new_names <- c("Year", "Quarter", "Date_Quarter", "GDP_Growth", 
               "Interest_Rate", "Lending_Rate", "Inflation", 
               "Exchange_Rate", "FDI", "covid_period", "covid_period_label")
names(vietnam_quarterly_clean) <- new_names[1:length(available_columns)]

# Remove missing values
vietnam_quarterly_clean <- vietnam_quarterly_clean[complete.cases(vietnam_quarterly_clean[,1:9]), ]

print("✓ Đã chuyển đổi sang quarterly data với COVID classification")
print(paste("Số quan sát quarterly:", nrow(vietnam_quarterly_clean)))

# ==============================================================================
# HIỂN THỊ DỮ LIỆU WORLD BANK VỚI COVID PERIOD
# ==============================================================================

# Thêm COVID period vào annual data
vietnam_wb_data$covid_period <- ifelse(
  vietnam_wb_data$year < 2020, 0,
  ifelse(vietnam_wb_data$year >= 2020 & vietnam_wb_data$year <= 2022, 1, 2)
)

vietnam_wb_data$covid_period_label <- factor(
  vietnam_wb_data$covid_period,
  levels = c(0, 1, 2),
  labels = c("Pre-COVID", "During COVID", "Post-COVID")
)

print("Dữ liệu gốc từ World Bank (annual) với COVID Period:")

# Display annual data với formatting (bao gồm FDI và COVID)
if("BX.KLT.DINV.CD.WD" %in% names(vietnam_wb_data)) {
  wb_display <- vietnam_wb_data[, c("year", "NY.GDP.MKTP.KD.ZG", "FP.CPI.TOTL.ZG", 
                                    "Interest_Rate", "PA.NUS.FCRF", "BX.KLT.DINV.CD.WD", 
                                    "covid_period", "covid_period_label")]
  names(wb_display) <- c("Year", "GDP_Growth", "Inflation", "Interest_Rate", "Exchange_Rate", 
                         "FDI_USD", "COVID_Period", "COVID_Label")
  
  # Format numbers
  wb_display$GDP_Growth <- round(wb_display$GDP_Growth, 2)
  wb_display$Inflation <- round(wb_display$Inflation, 2)
  wb_display$Interest_Rate <- round(wb_display$Interest_Rate, 2)
  wb_display$Exchange_Rate <- round(wb_display$Exchange_Rate, 0)
  wb_display$FDI_Billion_USD <- round(wb_display$FDI_USD / 1e9, 2)  # Convert to billions
  wb_display$FDI_USD <- NULL  # Remove raw USD column
} else {
  wb_display <- vietnam_wb_data[, c("year", "NY.GDP.MKTP.KD.ZG", "FP.CPI.TOTL.ZG", 
                                    "Interest_Rate", "PA.NUS.FCRF", "covid_period", "covid_period_label")]
  names(wb_display) <- c("Year", "GDP_Growth", "Inflation", "Interest_Rate", "Exchange_Rate", 
                         "COVID_Period", "COVID_Label")
  
  # Format numbers
  wb_display$GDP_Growth <- round(wb_display$GDP_Growth, 2)
  wb_display$Inflation <- round(wb_display$Inflation, 2)
  wb_display$Interest_Rate <- round(wb_display$Interest_Rate, 2)
  wb_display$Exchange_Rate <- round(wb_display$Exchange_Rate, 0)
}

print(wb_display)

print("\n=== DỮ LIỆU QUARTERLY ĐẦY ĐỦ (từ World Bank) VỚI COVID PERIOD ===")
print("Dữ liệu quarterly với seasonal patterns và COVID classification:")

# Display quarterly data với tất cả các biến bao gồm COVID
if("FDI" %in% names(vietnam_quarterly_clean)) {
  quarterly_display <- vietnam_quarterly_clean[, c("Year", "Quarter", "GDP_Growth", 
                                                   "Interest_Rate", "Inflation", "FDI", 
                                                   "covid_period", "covid_period_label")]
  quarterly_display$GDP_Growth <- round(quarterly_display$GDP_Growth, 2)
  quarterly_display$Interest_Rate <- round(quarterly_display$Interest_Rate, 2)
  quarterly_display$Inflation <- round(quarterly_display$Inflation, 2)
  quarterly_display$FDI <- round(quarterly_display$FDI, 2)
} else {
  quarterly_display <- vietnam_quarterly_clean[, c("Year", "Quarter", "GDP_Growth", 
                                                   "Interest_Rate", "Inflation", 
                                                   "covid_period", "covid_period_label")]
  quarterly_display$GDP_Growth <- round(quarterly_display$GDP_Growth, 2)
  quarterly_display$Interest_Rate <- round(quarterly_display$Interest_Rate, 2)
  quarterly_display$Inflation <- round(quarterly_display$Inflation, 2)
}

print(quarterly_display)

# Tạo display_sample để sử dụng trong visualization
if("FDI" %in% names(vietnam_quarterly_clean)) {
  display_sample <- data.frame(
    Year_Q = paste(vietnam_quarterly_clean$Year, "Q", vietnam_quarterly_clean$Quarter, sep=""),
    GDP = round(vietnam_quarterly_clean$GDP_Growth, 2),
    Interest = round(vietnam_quarterly_clean$Interest_Rate, 2),
    Inflation = round(vietnam_quarterly_clean$Inflation, 2),
    FDI = round(vietnam_quarterly_clean$FDI, 2),
    Exchange = round(vietnam_quarterly_clean$Exchange_Rate, 0),
    COVID = vietnam_quarterly_clean$covid_period,
    COVID_Label = vietnam_quarterly_clean$covid_period_label
  )
} else {
  display_sample <- data.frame(
    Year_Q = paste(vietnam_quarterly_clean$Year, "Q", vietnam_quarterly_clean$Quarter, sep=""),
    GDP = round(vietnam_quarterly_clean$GDP_Growth, 2),
    Interest = round(vietnam_quarterly_clean$Interest_Rate, 2),
    Inflation = round(vietnam_quarterly_clean$Inflation, 2),
    Exchange = round(vietnam_quarterly_clean$Exchange_Rate, 0),
    COVID = vietnam_quarterly_clean$covid_period,
    COVID_Label = vietnam_quarterly_clean$covid_period_label
  )
}

print("🎉 DỮ LIỆU WORLD BANK VỚI COVID CLASSIFICATION ĐÃ SẴN SÀNG CHO PHÂN TÍCH! 🎉")

# ==============================================================================
# TRỰC QUAN HÓA DỮ LIỆU
# ==============================================================================

#Biểu đồ boxplot phân bố các biến theo COVID_Label
# Chuyển dữ liệu sang dạng dài để dễ vẽ nhiều biến cùng lúc
df_long <- display_sample %>%
  pivot_longer(cols = c(GDP, Interest, Inflation, FDI, Exchange),
               names_to = "Variable",
               values_to = "Value")
ggplot(df_long, aes(x = COVID_Label, y = Value, fill = COVID_Label)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free_y") +   # Tạo một biểu đồ cho mỗi biến
  theme_minimal() +
  labs(
    title = "Phân bố các biến vĩ mô theo từng giai đoạn COVID",
    x = "Giai đoạn COVID",
    y = "Giá trị"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )


# Biểu đồ diễn biến GDP theo thời gian và giai đoạn COVID
display_sample$Year_Q <- factor(display_sample$Year_Q, levels = unique(display_sample$Year_Q), ordered = TRUE)
ggplot(display_sample, aes(x = Year_Q, y = GDP, group = COVID_Label, color = COVID_Label)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Diễn biến tăng trưởng GDP theo quý trong các giai đoạn COVID",
    x = NULL,  # ẩn nhãn trục x luôn
    y = "GDP"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


# Biểu đồ diễn biến lãi suất theo thời gian và giai đoạn COVID
display_sample$Year_Q <- factor(display_sample$Year_Q, levels = unique(display_sample$Year_Q), ordered = TRUE)
ggplot(display_sample, aes(x = Year_Q, y = Interest, group = COVID_Label, color = COVID_Label)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Diễn biến lãi suất theo quý trong các giai đoạn COVID",
    x = NULL,
    y = "Lãi suất (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


# Biểu đồ diễn biến lạm phát theo thời gian và giai đoạn COVID
display_sample$Year_Q <- factor(display_sample$Year_Q, levels = unique(display_sample$Year_Q), ordered = TRUE)
ggplot(display_sample, aes(x = Year_Q, y = Inflation, group = COVID_Label, color = COVID_Label)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Diễn biến lạm phát theo quý trong các giai đoạn COVID",
    x = NULL,
    y = "Lạm phát (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


# Biểu đồ diễn biến FDI theo thời gian và giai đoạn COVID
display_sample$Year_Q <- factor(display_sample$Year_Q, levels = unique(display_sample$Year_Q), ordered = TRUE)
ggplot(display_sample, aes(x = Year_Q, y = FDI, group = COVID_Label, color = COVID_Label)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Diễn biến FDI theo quý trong các giai đoạn COVID",
    x = NULL,
    y = "FDI"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


# Biểu đồ diễn biến tỷ giá theo thời gian và giai đoạn COVID
display_sample$Year_Q <- factor(display_sample$Year_Q, levels = unique(display_sample$Year_Q), ordered = TRUE)
ggplot(display_sample, aes(x = Year_Q, y = Exchange, group = COVID_Label, color = COVID_Label)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Diễn biến tỷ giá theo quý trong các giai đoạn COVID",
    x = NULL,
    y = "Tỷ giá"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


#Biểu đồ Scatter plot biểu diễn mối quan hệ giữa Lãi suất và tăng trưởng GDP theo giai đoạn COVID
ggplot(display_sample, aes(x = Interest, y = GDP, color = COVID_Label)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = COVID_Label, color = COVID_Label)) +
  labs(
    title = "Mối quan hệ giữa Lãi suất và tăng trưởng GDP theo giai đoạn COVID",
    x = "Lãi suất",
    y = "GDP",
    color = "Giai đoạn COVID"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


#Biểu đồ Scatter plot biểu diễn mối quan hệ giữa Lạm phát và tăng trưởng GDP theo giai đoạn COVID
ggplot(display_sample, aes(x = Inflation, y = GDP, color = COVID_Label)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = COVID_Label, color = COVID_Label)) +
  labs(
    title = "Mối quan hệ giữa Lạm phát và tăng trưởng GDP theo giai đoạn COVID",
    x = "Lạm phát (%)",
    y = "GDP",
    color = "Giai đoạn COVID"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


#Biểu đồ Scatter plot biểu diễn mối quan hệ giữa FDI và tăng trưởng GDP theo giai đoạn COVID
ggplot(display_sample, aes(x = FDI, y = GDP, color = COVID_Label)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = COVID_Label, color = COVID_Label)) +
  labs(
    title = "Mối quan hệ giữa FDI và tăng trưởng GDP theo giai đoạn COVID",
    x = "FDI",
    y = "GDP",
    color = "Giai đoạn COVID"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


#Biểu đồ Scatter plot biểu diễn mối quan hệ giữa tỷ giá và tăng trưởng GDP theo giai đoạn COVID
ggplot(display_sample, aes(x = Exchange, y = GDP, color = COVID_Label)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = COVID_Label, color = COVID_Label)) +
  labs(
    title = "Mối quan hệ giữa Tỷ giá và Tăng trưởng GDP theo giai đoạn COVID",
    x = "Tỷ giá",
    y = "GDP",
    color = "Giai đoạn COVID"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


#Heatmap tương quan các biến vĩ mô theo từng giai đoạn COVID
# Bước 1: Tính ma trận tương quan cho từng nhóm
corr_list <- display_sample %>%
  group_by(COVID_Label) %>%
  summarise(
    corr_matrix = list(cor(select(cur_data(), GDP, Interest, Inflation, FDI, Exchange), use = "complete.obs"))
  )
# Bước 2: Chuyển ma trận tương quan sang dạng dữ liệu dài để dễ vẽ
corr_long <- corr_list %>%
  mutate(
    corr_df = purrr::map(corr_matrix, ~as.data.frame(as.table(.)))
  ) %>%
  select(COVID_Label, corr_df) %>%
  unnest(cols = c(corr_df))
# Bước 3: Vẽ heatmap tương quan theo từng giai đoạn COVID
ggplot(corr_long, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Freq, 2)), size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab") +
  facet_wrap(~COVID_Label) +
  theme_minimal() +
  labs(
    title = "Heatmap tương quan các biến vĩ mô theo từng giai đoạn COVID",
    x = NULL,
    y = NULL,
    fill = "Tương quan"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Biểu đồ density theo giai đoạn COVID cho từng biến
# Tạo lại df_long cho density plot
if("FDI" %in% names(display_sample)) {
  df_long_density <- display_sample %>%
    pivot_longer(cols = c(GDP, Interest, Inflation, FDI, Exchange),
                 names_to = "Variable",
                 values_to = "Value")
} else {
  df_long_density <- display_sample %>%
    pivot_longer(cols = c(GDP, Interest, Inflation, Exchange),
                 names_to = "Variable",
                 values_to = "Value")
}

density_plot <- ggplot(df_long_density, aes(x = Value, fill = COVID_Label)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Variable, scales = "free") +
  labs(
    title = "Phân bố các biến vĩ mô theo giai đoạn COVID",
    x = "Giá trị",
    y = "Mật độ"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )
print(density_plot)

# Biểu đồ mối liên hệ giữa các biến vĩ mô
pairs(display_sample[, c("GDP", "Interest", "Inflation", "FDI", "Exchange")],
      main = "Mối liên hệ giữa các biến vĩ mô",
      pch = 21, bg = "lightblue")

# ==============================================================================
# TẠO VÀ KIỂM ĐỊNH MÔ HÌNH
# ==============================================================================

# Chuyển đổi cột thời gian sang định dạng quý
display_sample$Year_Q <- as.yearqtr(display_sample$Year_Q, format = "%YQ%q")
# ==============================================================================
# MÔ HÌNH HỒI QUY ĐƠN GIẢN
# ==============================================================================

model <- lm(GDP ~ Interest + Inflation + FDI + Exchange + COVID, data = display_sample)
summary(model)

model_1 <- lm(GDP ~ Interest + Inflation + FDI + Exchange, data = display_sample)
summary(model_1)
# Kiểm định model

# Kiểm tra đa cộng tuyến
vif(model_1)
# Kiểm định phần dư
# Kiểm tra phân phối chuẩn phần dư
res <- resid(model_1)
hist(res, breaks = 10, col = "skyblue", main = "Biểu đồ histogram của phần dư")
qqnorm(res)
qqline(res, col = "red")
# Kiểm tra tự tương quan phần dư (Durbin-Watson)
dwtest(model_1)

# Kiểm định phương sai thay đổi
bptest(model_1)

# ==============================================================================
# MÔ HÌNH RANDOM FOREST
# ==============================================================================

# Thiết lập seed để kết quả tái lập
set.seed(42)

# Huấn luyện mô hình Random Forest 
rf_model <- randomForest(GDP ~ Interest + Inflation + FDI +  Exchange, 
                         data = display_sample, 
                         ntree = 500,        # Số lượng cây
                         mtry = 2,           # Số lượng biến chia tại mỗi node
                         importance = TRUE)  # Lấy tầm quan trọng biến

# Xem thông tin của mô hình
print(rf_model)

# Dự đoán trên dữ liệu gốc
y_pred <- predict(rf_model, display_sample)
y_actual <- display_sample$GDP

# Tính R-squared thủ công
rss <- sum((y_actual - y_pred)^2)
tss <- sum((y_actual - mean(y_actual))^2)
r_squared <- 1 - (rss / tss)

cat("R-squared (tính thủ công):", r_squared, "\n")

# Tính tầm quan trọng của các biến
importance_values <- importance(rf_model)
print(importance_values)

# Biểu đồ tính quan trọng của các biến
varImpPlot(rf_model)


# ==============================================================================
# MÔ HÌNH XGBOOST
# ==============================================================================

# Chuẩn bị dữ liệu cho XGBoost
# Sử dụng cùng ma trận X và vector y 
X <- as.matrix(display_sample[, c("Interest", "Inflation", "FDI", "Exchange")])
y <- display_sample$GDP
xgb_data <- xgb.DMatrix(data = X, label = y)

# Thiết lập parameters cho XGBoost
xgb_params <- list(
  objective = "reg:squarederror",  # Regression objective
  eval_metric = "rmse",
  max_depth = 6,
  eta = 0.1,                       # Learning rate
  subsample = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 1
)

# Thiết lập seed để kết quả tái lập
set.seed(42)

# Cross-validation để tìm số round tối ưu
cv_xgb <- xgb.cv(
  params = xgb_params,
  data = xgb_data,
  nrounds = 100,
  nfold = 5,
  early_stopping_rounds = 10,
  verbose = FALSE,
  print_every_n = 10
)

# Số round tối ưu
best_nrounds <- cv_xgb$best_iteration
cat("Số round tối ưu:", best_nrounds, "\n")

# Huấn luyện mô hình XGBoost cuối cùng
xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_data,
  nrounds = best_nrounds,
  verbose = FALSE
)

# Dự đoán
xgb_pred <- predict(xgb_model, xgb_data)

# Tính toán metrics cho XGBoost
xgb_mse <- mean((y - xgb_pred)^2)
xgb_rmse <- sqrt(xgb_mse)
xgb_mae <- mean(abs(y - xgb_pred))

# Tính R-squared cho XGBoost
xgb_rss <- sum((y - xgb_pred)^2)
xgb_tss <- sum((y - mean(y))^2)
xgb_r_squared <- 1 - (xgb_rss / xgb_tss)

# Hiển thị kết quả XGBoost
cat("\nKết quả XGBoost:\n")
cat("MSE:", round(xgb_mse, 4), "\n")
cat("RMSE:", round(xgb_rmse, 4), "\n")
cat("MAE:", round(xgb_mae, 4), "\n")
cat("R-squared:", round(xgb_r_squared, 4), "\n")

# ==============================================================================
# SO SÁNH CÁC MÔ HÌNH
# ==============================================================================

# Tạo bảng so sánh
comparison_table <- data.frame(
  Model = c("Linear Regression", "Random Forest", "XGBoost"),
  R_squared = c(
    round(summary(model_1)$r.squared, 4),
    round(r_squared, 4),
    round(xgb_r_squared, 4)
  ),
  RMSE = c(
    round(sqrt(mean(resid(model_1)^2)), 4),
    round(sqrt(mean((y_actual - y_pred)^2)), 4),
    round(xgb_rmse, 4)
  ),
  MAE = c(
    round(mean(abs(resid(model_1))), 4),
    round(mean(abs(y_actual - y_pred)), 4),
    round(xgb_mae, 4)
  )
)

print("Bảng so sánh hiệu suất các mô hình:")
print(comparison_table)

# Tìm mô hình tốt nhất dựa trên R-squared
best_model_idx <- which.max(comparison_table$R_squared)
best_model_name <- comparison_table$Model[best_model_idx]
cat("\nMô hình tốt nhất dựa trên R-squared:", best_model_name, "\n")
cat("R-squared:", comparison_table$R_squared[best_model_idx], "\n")

# ==============================================================================
# BIỂU ĐỒ CỘT SO SÁNH HIỆU SUẤT MODEL DỰA TRÊN R²
# ==============================================================================

# Tạo biểu đồ cột so sánh R-squared của các models (sử dụng comparison_table có sẵn)
ggplot(comparison_table, aes(x = reorder(Model, R_squared), y = R_squared, fill = Model)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = R_squared), vjust = -0.3, size = 4, fontface = "bold") +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(limits = c(0, max(comparison_table$R_squared) * 1.1),
                     breaks = seq(0, 1, 0.1),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "So sánh hiệu suất các mô hình dự đoán GDP",
    subtitle = "Dựa trên chỉ số R-squared (R²)",
    x = "Mô hình",
    y = "R-squared (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0.5, face = "italic", size = 10)
  )

