library("here")
library("dplyr")
library("ggplot2")
library("purrr")
library("tidyr")

# source(here("R", "thresholds.R"))
# https://data-se.netlify.app/2018/12/05/plot-many-ggplot-diagrams-using-nest-and-map/

# Read in trade data
ffd <- read.csv(here("data", "raw", "ffd2.csv"),
                col.names = c("year", "month", "flow", "product", "value", "tonnes"))

# Create thresholds
trade <- ffd %>% 
  mutate(date = as.Date(paste(year, month, "1", sep = "-"), format = "%Y-%m-%d")) %>% 
  group_by(product, flow) %>% 
  mutate(diff = value - lag(value),
         limupper = quantile(diff, 0.75, na.rm = TRUE) + (quantile(diff, 0.75, na.rm = TRUE) - quantile(diff, 0.25, na.rm = TRUE)) * 1.5,
         limupperub = limupper * 1.05,
         limupperlb = limupper * 0.95,
         limlower = quantile(diff, 0.25, na.rm = TRUE) - (quantile(diff, 0.75, na.rm = TRUE) - quantile(diff, 0.25, na.rm = TRUE)) * 1.5,
         limlowerub = limlower * 1.05,
         limlowerlb = limlower * 0.95)

# Nested for charting
trade_nest <- trade %>% 
              ungroup() %>% 
              # filter(flow == "Total Exports") %>%
              group_by(flow, product) %>%
              nest() %>%
              mutate(cht = pmap(list(data = data, title = product, subtitle = flow), prod))            
  
              




test <- trade %>% 
        filter(flow == "Total Exports", product == "Lettuce and chicory, fresh or chilled")


prod <- function(data, title, subtitle){
  data %>% 
    ggplot() +
    geom_line(aes(x = date, y = diff), colour = "blue") +
    geom_point(aes(x = date, y = diff), colour = "blue") +
    geom_line(aes(x = date, y = limupper), colour = "red", size = 3, alpha = 0.25) +
    geom_line(aes(x = date, y = limlower), colour = "red", size = 3, alpha = 0.25) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(title = title,
         subtitle = subtitle) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())
    # facet_wrap(~ product, scales = "free")
  
}

prod(test, "product", "flow")

forecast::ndiffs(ts(test$value, frequency = 12, start = 2009))
