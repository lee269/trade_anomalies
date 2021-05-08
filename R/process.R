library("dplyr")
library("readxl")
library("ggplot2")
library("stats")
library("tseries")
library("forecast")
library("zoo")
library("urca")
library("scales")
library("here")
library("timetk")

trade <- read.csv(here("data", "raw", "beef.csv"),
                  col.names = c("year", "month", "product", "value", "tonnes"))

trade <- trade %>% 
  mutate(date = as.Date(paste(year, month, "1", sep = "-"), format = "%Y-%m-%d"))

head(trade)

ggplot(trade, aes(x = date, y = value)) + geom_line()

forecast::ndiffs(ts(trade$value, frequency = 12, start = 2018))

trade_diff <- diff(ts(trade$value, frequency = 12, start = 2018))

acf(trade_diff)
boxplot(trade_diff)

trade_diff_q3 <- quantile(trade_diff, 0.75, na.rm = TRUE)
trade_diff_q1 <- quantile(trade_diff, 0.25, na.rm = TRUE)
trade_diff_iqr <- trade_diff_q3 - trade_diff_q1


trade_diff_ul <- trade_diff_q3 + 1.5*trade_diff_iqr
trade_diff_ullb <- trade_diff_ul * 0.95
trade_diff_ulub <- trade_diff_ul * 1.05


trade_diff_ll <- trade_diff_q1 - 1.5*trade_diff_iqr
trade_diff_lllb <- trade_diff_ll * 0.95
trade_diff_llub <- trade_diff_ll * 1.05



trade_diff_df <- cbind(trade$date[-1], as.data.frame(trade_diff))
colnames(trade_diff_df) <- c("date", "value")

trade_diff_df <- tk_tbl(trade_diff_df)

ggplot(trade_diff_df, aes(x = date, y = value)) +
  geom_line() +
  geom_rect(xmin = as.Date("2018-02-01"),
            xmax = Inf, 
            ymin = trade_diff_lllb,
            ymax = trade_diff_llub, 
            alpha = 0.005, 
            colour = NA, 
            fill = "Blue") +
  geom_rect(xmin = as.Date("2018-02-01"),
            xmax = Inf, 
            ymin = trade_diff_ullb,
            ymax = trade_diff_ulub, 
            alpha = 0.005, 
            colour = NA, 
            fill = "Blue") +
  scale_y_continuous(limits = c(-22000, 20000))
