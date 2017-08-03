library(tidyverse)
library(stringr)
library(lubridate)


# 読み込むファイル名の一覧のオブジェクト作成 -------
files <- list.files(path = "accelerometer",  full.names = TRUE)
file_name <- str_replace(files, ".csv", "")
file_name <- str_replace(file_name, "accelerometer/", "")


# csvファイルを一括で読み込む -------
ldata <- lapply(files, read_csv, locale = locale(encoding="cp932"), 
                skip = 3, col_types = cols("時刻" = col_character()))


# ファイル名から日付とIDを取得 -------
for(i in 1:length(file_name)) {
  ldata[[i]]$year_month_date = str_sub(file_name[i],-8,-1) #日付
  ldata[[i]]$id = str_sub(file_name[i],1,5) #id
}


# リストの各要素を1つのデータフレームに統合 -------
alldata <- do.call(rbind, ldata)
alldata


# 時間の変数を作成 -------
alldata <- alldata %>%
  rename(hour_min_sec = 時刻, mets = 活動強度, activity = 運動種別) %>% 
  unite(year_month_date, hour_min_sec, col = "time",  
        sep = " ", remove = FALSE) %>%  
  mutate(time = ymd_hms(time, tz =  "Asia/Tokyo"), 
         day = mday(time),
         hour = hour(time),
         id = factor(id),
     actlv4 = cut(mets, breaks = c(-1, 0.9, 1.5, 2.9, Inf), 
             labels = c("none", "sedentary", "light", "mvpa")))


# データの確認 -------
alldata %>% select(id,time,day,hour,mets,actlv4)


# 指定した1日の活動量履歴を視覚化(12月1日　平日・勤務日) -------
alldata %>% filter(day == 1) %>% 
  ggplot() +
  geom_col(aes(x = time, y = mets, fill = actlv4)) +
  coord_cartesian(xlim = c(ymd_hms("2014-12-01  7:00:00", tz = "Asia/Tokyo"),
                           ymd_hms("2014-12-01 19:30:00", tz = "Asia/Tokyo")),
                  ylim = c(0,10)) + 
  scale_x_datetime(date_breaks = "1 hours", date_labels = "%H:%M") +
  geom_hline(yintercept = 1.5, color = "yellow")


# 指定した1日の活動量履歴を視覚化(12月7日　休日・自宅) -------
alldata %>% filter(day == 7) %>% 
  ggplot() +
  geom_col(aes(x = time, y = mets, fill = actlv4)) +
  coord_cartesian(xlim = c(ymd_hms("2014-12-07  9:00:00", tz = "Asia/Tokyo"),
                           ymd_hms("2014-12-07 19:30:00", tz = "Asia/Tokyo")),
                  ylim = c(0,10)) + 
  scale_x_datetime(date_breaks = "1 hours", date_labels = "%H:%M") +
  geom_hline(yintercept = 1.5, color = "yellow") 


# 指定した1日の活動量履歴を視覚化(12月30日　休日・外出) -------
alldata %>% filter(day == 30) %>% 
  ggplot() +
  geom_col(aes(x = time, y = mets, fill = actlv4)) +
  coord_cartesian(xlim = c(ymd_hms("2014-12-30  9:00:00", tz = "Asia/Tokyo"),
                           ymd_hms("2014-12-30 19:30:00", tz = "Asia/Tokyo")),
                  ylim = c(0,10)) + 
  scale_x_datetime(date_breaks = "1 hours", date_labels = "%H:%M") +
  geom_hline(yintercept = 1.5, color = "yellow") 


# 各日の1時間ごとのメッツの平均値を可視化 -------
alldata %>% 
  group_by(day,hour) %>% 
  summarise(mean_mets = mean(mets, rm.na = TRUE)) %>% 
  ggplot() + 
  geom_col(aes(x = hour, y = mean_mets, fill = factor(day))) +
  theme(strip.text=element_text(size = 12)) +
  facet_grid(day ~ .)
