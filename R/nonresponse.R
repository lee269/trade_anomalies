library(here)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# url for non response data
# https://www.uktradeinfo.com/trade-data/ots-custom-table/ 
# ?id=35222291-0b7b-4309-af60-7bd2df3417e5

# Read in data
nonresponse <- read_csv(here("data", "raw", "nonresponse2.csv"),
                        col_names = c("sitc1",
                                      "sitc2",
                                      "sitc3",
                                      "sitc4",
                                      "sitc5",
                                      "flow",
                                      "year",
                                      "month",
                                      "include",
                                      "value_sterling"),
                        skip = 1) %>% 
                  select(!(sitc3:sitc5)) %>% 
                  mutate(date = ymd(paste(year, month, "1", sep = "-")))

# Total nonresponse
totals <- nonresponse %>% 
                group_by(flow, date) %>% 
                summarise(total = sum(value_sterling))

totals %>% 
            ggplot(aes(x = date, y = total)) +
            geom_line(aes(colour = flow)) +
            theme_minimal()

# Products nonresponse
products <- nonresponse %>% 
              group_by(sitc2, flow, date) %>% 
              summarise(total = sum(value_sterling))

products %>% 
          ggplot(aes(x = date, y = total)) +
          geom_line(aes(colour = sitc2)) +
          facet_grid(cols = vars(flow)) +
          guides(fill = guide_legend(direction = "horizontal"))
