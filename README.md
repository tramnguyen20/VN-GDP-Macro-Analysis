# Impact of Macroeconomic Factors on Vietnam's GDP Growth

## Project Overview
Dự án phân tích và định lượng các nhân tố vĩ mô tác động đến sự tăng trưởng GDP của Việt Nam từ năm 1990 đến nay. Dự án kết hợp giữa các phương pháp Kinh tế lượng truyền thống (Econometrics) và các thuật toán Học máy (Machine Learning) để so sánh khả năng giải thích và dự báo của mô hình.

## Data Source
Dữ liệu được trích xuất trực tiếp từ API của **World Bank (WDI)** với các chỉ số chính:
- **GDP Growth**: Tỷ lệ tăng trưởng GDP hàng năm.
- **FDI**: Đầu tư trực tiếp nước ngoài (% GDP).
- **Trade**: Tổng kim ngạch xuất nhập khẩu (% GDP).
- **Inflation**: Tỷ lệ lạm phát (CPI).
- **Money Supply (M2)**: Cung tiền mở rộng.

## Methodology & Models

### 1. Econometrics Approach
- **OLS Regression**: Thiết lập mô hình cơ sở để đánh giá mức độ tác động của từng biến.
- **Instrumental Variables (IV) / 2SLS**: Xử lý hiện tượng nội sinh (Endogeneity) bằng phương pháp biến công cụ thông qua thư viện `AER`, đảm bảo các ước lượng không bị chệch.
- **Diagnostic Tests**: Kiểm định đa cộng tuyến (VIF), tự tương quan (Durbin-Watson) và phương sai thay đổi (Breusch-Pagan).

### 2. Machine Learning Approach
- **Random Forest & XGBoost**: Đánh giá tầm quan trọng của các biến (Feature Importance) và xử lý các mối quan hệ phi tuyến tính.
- **Regularized Regression**: Áp dụng Lasso và Ridge để tối ưu hóa việc chọn lọc đặc trưng và kiểm soát quá mức (Overfitting).



## Key Technical Insights
- So sánh hiệu suất giữa các mô hình dựa trên chỉ số **R-squared**, **RMSE** và **MAE**.
- Trực quan hóa ma trận tương quan (Correlation Matrix) giữa các biến vĩ mô.
- Xác định biến số có tác động mạnh nhất đến sự ổn định và tăng trưởng của nền kinh tế.



## Technical Stack
- **Language**: R
- **Libraries**: 
    - *Data:* `WDI`, `tidyverse`, `dplyr`
    - *Econometrics:* `AER`, `systemfit`, `car`, `lmtest`
    - *Machine Learning:* `randomForest`, `xgboost`, `glmnet`
    - *Visualization:* `ggplot2`, `corrplot`

## Project Structure
- `impact_macro_gdp_vn.R`: Mã nguồn xử lý dữ liệu và xây dựng mô hình.
- `README.md`: Tài liệu hướng dẫn và tóm tắt kết quả dự án.

---
