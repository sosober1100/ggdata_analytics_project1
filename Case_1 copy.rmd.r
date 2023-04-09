# ======================================================
# STEP 1: SETUP ENVIRONMENT
# ======================================================

install.packages("tidyverse")
install.packages("Tmisc")
install.packages("pivottabler")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("geosphere")


library(Tmisc)
library(tidyverse)
library(pivottabler)
library(ggplot2)
library(tidyr)
library(geosphere)


load("df3_casual_number_durat.RData")
load("df3_member_number_durat.RData")
load("df2.RData")
load("station_distance.RData")



# ======================================================
# STEP 2: READ CSV
# ======================================================


df1 <- list.files(path = "/Users/quocbaonguyen/Downloads/Raw data/untitled folder", pattern = "*.csv", full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows()

# ======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
# ======================================================

### Kiểm tra bộ dữ liệu, có thể thấy không có vấn đề gì, các trường thời gian đúng kiểu dữ liệu, các trường số đúng kiểu dữ liệu luôn

glimpse(df1)

### Kiểm tra các giá trị bị trống ở từng cặp trường, có thể thấy

empty_cols <- colSums(is.na(df1))
empty_cols[empty_cols > 0]

### các cột có giá trị NA là: start_station_name, start_station_id, end_station_name,end_station_id, end_lat, end_lng, k có ảnh hưởng gì nhiều

### Tạo ra một cột thể hiện thời gian di chuyển theo seconds và chuyển nó về dạng số
df1$durat <- df1$ended_at - df1$started_at

df1$durat <- as.numeric(df1$durat)



### Kiểm tra giá trị của cột đó có cái nào <0 không, vì <0 là vô lý
View(df1 %>%
  filter(durat < 0))

### Loại bỏ các giá trị có durat < 0 vô lí này đi

df2 <- df1[!(df1$durat < 0), ] # ok rồi


### Tạo ra 2 cột thứ theo start và end date

df2$swday <- weekdays(
  df2$started_at,
  abbreviate = TRUE
)
df2$ewday <- weekdays(
  df2$ended_at,
  abbreviate = TRUE
)


### ------ Thêm một số cột: Ngày, Tháng, Năm, Thời gian đạp xe, Thứ trong tuần đó theo start time

df2$days <- format(
  as.Date(
    df2$started_at,
    format = "%d/%m/%Y"
  ), "%d"
) # đây là dd

system.time(df2$fulldate <- as.Date(df2$started_at)) # đây là yyyy-mm-dd

df2$month <- format(
  as.Date(
    df2$started_at,
    format = "%d/%m/%Y"
  ), "%m"
) # đây là mm

df2$year <- format(
  as.Date(
    df2$started_at,
    format = "%d/%m/%Y"
  ), "%Y"
) # đây là yyyy

system.time(df2$ddmmyy <- as.Date(df2$started_at)) # đây là yyyy-mm-dd

### ----- Tính khoảng cách
dist_haversine <- function(lat1, lon1, lat2, lon2) {
  distHaversine(c(lon1, lat1), c(lon2, lat2)) / 1000 # Tạo ra hàm để đổi đơn vị sang kilômét
}

df2$distance <- mapply(dist_haversine, df2$start_lat, df2$start_lng, df2$end_lat, df2$end_lng)
# mapply() là một hàm trong R cho phép bạn áp dụng một hàm lên nhiều đối số cùng một lúc và trả về một vector,
# trong trường hợp này thì mapply() được sử dụng để áp dụng hàm distHaversine() lên các cặp tọa độ bắt đầu và kết thúc của mỗi chuyến xe trong df2


### ----- Trích xuất ra bảng distance
station_distance <- df2 %>%
  select(start_station_name, end_station_name, distance) %>%
  filter(!is.na(start_station_name) & !is.na(end_station_name) & !is.na(distance)) %>%
  filter(!(start_station_name == end_station_name)) %>%
  group_by(start_station_name, end_station_name) %>%
  summarize(mean_distance = mean(distance))

# ======================================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS AND VISUALIZE
# ======================================================


### ------ Xem các loại member thì có các giá trị mean, max, min, sd ... để coi mô tả


df2_stat <- df2 %>%
  group_by(member_casual) %>%
  summarize(
    n(),
    mean = mean(durat),
    max = max(durat),
    min = min(durat),
    sd = sd(durat),
    med = median(durat)
  )
# casual có số lần đi ít hơn, tg trung bình cao hơn, max cao hơn, min = 0

### ------ Xem mô tả theo thứ ngày trong tuần, thấy theo ngày thì khá tương đồng, không có gì đặc biệt

df2_stat_bydatswday <- df2 %>%
  group_by(swday, member_casual) %>%
  summarize(
    n = n(),
    mean = mean(durat),
    max = max(durat),
    min = min(durat),
    sd = sd(durat),
    med = median(durat)
  ) %>%
  arrange(match(swday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))

### Xem theo thời gian trọng bình thì casual luôn lái nhiều hơn, hợp lý so với lập luận ban đầu, tuy nhiên thì t7 cn Casual lại có lái lâu hơn các ngày khác

ggplot(df2_stat_bydatswday) +
  geom_col(aes(x = swday, y = mean, fill = member_casual), position = "dodge") +
  labs(
    title = "Average duration by day of week and member type",
    x = "Day of week",
    y = "Average duration"
  ) +
  scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  theme_minimal()

### Xem số lượng obs theo các ngày trong tuần thì thấy Casual đi nhiều ở 2 ngày cuối tuần

ggplot(df2_stat_bydatswday) +
  geom_col(aes(x = swday, y = n, fill = member_casual), position = "dodge") +
  labs(
    title = "Number of observations by day of week and member type",
    x = "Day of week",
    y = "Observation"
  ) +
  scale_x_discrete(limits = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  theme_minimal()

#-------------------------------------------------------------------------------------
#### --------> Tập trung campaing vào 2 ngày cuối tuần này để casual đổi thành member|
#-------------------------------------------------------------------------------------

### ------ Xem theo cung đường về số lần ride, tổng số thời gian thực hiện, load lại biến df3 là được, không cần chạy cái này

df2_member_station_rideid <- df2 %>%
  select(member_casual, start_station_name, ride_id, end_station_name) %>%
  filter(!is.na(start_station_name) & !is.na(end_station_name)) %>%
  pivot_longer(cols = -c(start_station_name, member_casual, end_station_name), names_to = "member_casuals", values_to = "descr") %>%
  group_by(start_station_name, member_casual, end_station_name) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = member_casual, values_from = n, values_fill = 0)

### ------ Xem theo station và membercasual, cung đường có thời gian duration nhiều nhất

df2_member_station_duration <- df2 %>%
  select(member_casual, start_station_name, end_station_name, durat) %>%
  filter(!is.na(start_station_name) & !is.na(end_station_name)) %>%
  pivot_longer(cols = c(-start_station_name, -end_station_name, -member_casual), names_to = "variable", values_to = "sum") %>%
  group_by(start_station_name, member_casual, end_station_name) %>%
  summarise(sum = sum(sum), .groups = "drop") %>%
  pivot_wider(names_from = member_casual, values_from = sum, values_fill = as.duration(0))


### ------ Ghép 2 bảng duration và number lại vói nhau

merged_df_member_station_full <- left_join(df2_member_station_rideid, df2_member_station_duration, by = c("start_station_name", "end_station_name"))
colnames(merged_df_member_station_full) <- c("start", "end", "num_c_rideid", "num_m_rideid", "sum_c_durat", "sum_m_durat")


### ----- Tách ra thành casual và member riêng

df3_casual_number_durat <- select(merged_df_member_station_full, -c("num_m_rideid", "sum_m_durat"))

# Tìm ra tính cột multiple của casual
df3_casual_number_durat <- df3_casual_number_durat %>%
  mutate(multiple = num_c_rideid * sum_c_durat)

# Tìm ra 20% cung đường được sử dụng nhiều nhất đối với casual, từ đó xem thử xem là có thể có chương trình khuyến mãi với cung đường này được hay không? vì tần suất sử dụng của những người ngày khác cao

df3_casual_number_durat_80 <- df3_casual_number_durat %>%
  filter(multiple >= quantile(df3_casual_number_durat$multiple, 0.8)) %>%
  arrange(desc(multiple))

# Tìm ra tính cột multiple của member

df3_member_number_durat <- df3_member_number_durat %>%
  mutate(multiple = num_m_rideid * sum_m_durat)

# Tìm ra 20% cung đường được sử dụng nhiều nhất đối với member
df3_member_number_durat_80 <- df3_member_number_durat %>%
  filter(multiple >= quantile(df3_member_number_durat$multiple, 0.8)) %>%
  arrange(desc(multiple))

# tìm ra các cung đường mà casual thường thuê xe để đi quanh lại trong năm và chia thành 2 nhóm, nhóm có cung đường khứ hồi và nhóm không
df3_casual_number_durat_startisend <- df3_casual_number_durat_80 %>%
  filter(start == end) %>%
  arrange(desc(multiple)) # -> có thể tập trung vào nhóm này để tạo ra các chương trình khuyến mãi


df3_startisend_ridetype_distance <- df2 %>%
  select(start_station_name, end_station_name, rideable_type, ride_id) %>%
  filter(start_station_name == end_station_name) %>%
  pivot_longer(cols = -c(start_station_name, end_station_name, rideable_type), names_to = "member_casuals", values_to = "descr") %>%
  group_by(start_station_name, end_station_name, rideable_type) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = rideable_type, values_from = n, values_fill = 0)

colnames(df3_startisend_ridetype_distance) <- c("start", "end", "electric_bike", "classic_bike", "docked_bike")

df3_final1_distance <- station_distance

colnames(df3_final1_distance) <- c("start", "end", "distance")

df3_final_startisend <- left_join(df3_casual_number_durat_startisend, df3_startisend_ridetype_distance, by = c("start", "end"))

summary(df3_final_startisend)
#----------------------------------------------------------------------------------------------
# kq cho thấy là số lượng ng đi classic là cao nhất, rồi đến electric, rồi dến docked ->
# -> theo thứ tự này để làm chương trình khuyến mãi để thu hút họ, phần lớn họ sẽ trả lại xe
# tại ga đó nên vận dùng đặc điểm này để thu hút người sử dụng, ví dụ như là làm chương trình khuyến
# mãi tại cung đường này để thu hút những người casual này
#----------------------------------------------------------------------------------------------

df3_casual_number_durat_start_isnot_tend <- df3_casual_number_durat_80 %>% # nhóm này sẽ phân tích sâu hơn về khoảng cách để xem sự khác biệt sau
  filter(!(start == end)) %>%
  arrange(desc(multiple))

df3_startnotend_ridetype_distance <- df2 %>%
  select(start_station_name, end_station_name, rideable_type, ride_id) %>%
  filter(!(start_station_name == end_station_name)) %>%
  pivot_longer(cols = -c(start_station_name, end_station_name, rideable_type), names_to = "member_casuals", values_to = "descr") %>%
  group_by(start_station_name, end_station_name, rideable_type) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = rideable_type, values_from = n, values_fill = 0)

colnames(df3_startnotend_ridetype_distance) <- c("start", "end", "electric_bike", "classic_bike", "docked_bike")


df3_final_startnotend <- left_join(df3_startnotend_ridetype_distance, df3_final1_distance, by = c("start", "end"))

summary(df3_final_startnotend)
#-----------------------------------------------------------------------------------------------------------------------------------
# kq cho thấy là số lượng ng đi classic là cao nhất, rồi đến electric, số lượng người đi docked rất thấp, có thể bỏ qua nhóm này
# Quãng đường trung bình mà những người đi trên tuyến này là 4.7 km
# -> từ đây có chương trình khuyến mãi nào để họ có thể đăng ký sử dụng member hay không? những người sử dụng dịch vụ này thế nào?
#-----------------------------------------------------------------------------------------------------------------------------------

# ----- Phân tích theo từng loại ridetype

df4_stat_byridetype <- df2 %>%
  group_by(rideable_type, member_casual) %>%
  summarize(
    n = n(),
    mean = mean(durat),
    max = max(durat),
    min = min(durat),
    sd = sd(durat),
    med = median(durat)
  )

ggplot(df4_stat_byridetype) +
  geom_col(aes(x = rideable_type, y = n, fill = member_casual), position = "dodge") +
  labs(
    title = "Number of observation by ride type and member type",
    x = "Day of week",
    y = "Average duration"
  ) +
  theme_minimal()

df4_stat_byridetype_distance <- df2 %>%
  filter(!is.na(df2$distance)) %>%
  group_by(rideable_type, member_casual) %>%
  summarize(
    n = n(),
    mean = mean(distance),
    max = max(distance),
    min = min(distance),
    sd = sd(distance),
    med = median(distance)
  )

ggplot(df4_stat_byridetype_distance) +
  geom_col(aes(x = rideable_type, y = mean, fill = member_casual), position = "dodge") +
  labs(
    title = "Average distance by ridetype and membercasual",
    x = "Ride type",
    y = "Average distance"
  )


#----------------------------------------------------------------------------------------
# Khoảng cách là gần như bằng nhau giữa các ridetype và member_casual
# vậy mà casual có thời gian sử dụng lâu hơn, đặc biệt docked bike chỉ có casual sử dụng
# -> Chủ yếu mọi người sử dụng xe đạp để đi những địa điểm không xác định, tiêu biểu là đi ngắm cảnh là nhiều (rõ ràng nhất đối với trường họp sử dụng dockedbike)
# -> kết hợp với các bên du lịch để cung cấp tuyến đường du lịch dành cho những người sử dụng docked bike, họ sẽ nhận được lịch trình tham khảo nếu đăng ký làm thành viên
#----------------------------------------------------------------------------------------



df4_stat_bymonth <- df2 %>%
  group_by(month, member_casual) %>%
  summarize(
    n = n(),
    mean = mean(durat),
    max = max(durat),
    min = min(durat),
    sd = sd(durat),
    med = median(durat)
  )

ggplot(df4_stat_bymonth) +
  geom_col(aes(x = month, y = n, fill = member_casual), position = "dodge") +
  labs(
    title = "Numerber of ride by ridetype and membercasual",
    x = "Ride type",
    y = "Number of observations"
  )

ggplot(df4_stat_bymonth) +
  geom_col(aes(x = month, y = mean, fill = member_casual), position = "dodge") +
  labs(
    title = "Average distance by ridetype and membercasual",
    x = "Ride type",
    y = "Average distance"
  )
#----------------------------------------------------------------------------------
## phân tích theo chuỗi thời gian cho thấy nhu cầu mỗi lần thuê tăng đỉnh điểm ở các thngs 6,7,8 và giảm dần đến hết năm, 
#có thể tìm thêm thông tin để làm chương trình khuyến mãi trong khoảng thời gian cao điểm này, đặc biệt là vào các ngày cuối tuần khi lượn thuê tăng cao
#----------------------------------------------------------------------------------
