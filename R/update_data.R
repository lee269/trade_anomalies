library(here)
library(readr)
library(dplyr)

# filenames
ffd_archive_file <- "ffd2009-2019.csv"
ffd_latest_file <- "ffd202104.csv"
ffd_country_archive_file <- "ffdcountry2015-2019.csv"
ffd_country_latest_file <- "ffdcountry202104.csv"
nr_latest_file <- "nonresponse202104.csv"

# Process FFD totals -----------------------------------------------------------

# read in the archive csv
ffdarchivecols <- cols(
  year = col_integer(),
  month = col_integer(),
  flow = col_character(),
  divcode = col_integer(),
  divdesc = col_character(),
  product = col_character(),
  value = col_double(),
  tonnes = col_double(),
  date = col_date(format = "")
)

ffdarchive <- read_csv(here("data", "tidy", ffd_archive_file),
                       col_types = ffd_latest_file) 


# read in the latest file
ffdlatestcols <- cols(
  year = col_integer(),
  month = col_integer(),
  flow = col_character(),
  divcode = col_integer(),
  divdesc = col_character(),
  product = col_character(),
  value = col_double(),
  tonnes = col_double()
)


ffdlatest <- read_csv(here("data", "raw", ffd_latest_file),
                      col_types = ffdlatestcols, 
                      col_names = c("year", "month", "flow", "divcode", "divdesc", "product", "value", "tonnes"),
                      skip = 1) %>% 
  mutate(date = as.Date(paste(year, month, "1", sep = "-"), format = "%Y-%m-%d"),
         flow = case_when(flow == "Total Exports" ~ "Exports",
                          flow == "Total Imports" ~ "Imports"),
         value = value * 1000) 



ffd <- ffdarchive %>% 
  bind_rows(ffdlatest)

# save latest data
write_rds(ffd, here("data", "tidy", "ffd.rds"))

# Process FFD Country data -----------------------------------------------------

ffdcountryarchivecols <- cols(
  year = col_integer(),
  month = col_integer(),
  flow = col_character(),
  eunoneu = col_character(),
  country = col_character(),
  divcode = col_integer(),
  divdesc = col_character(),
  value = col_double(),
  tonnes = col_double(),
  date = col_date(format = "")
)

ffdcountryarchive <- read_csv(here("data", "tidy", ffd_country_archive_file),
                              col_types = ffdcountryarchivecols)



ffdcountrylatestcols <- cols(
  year = col_integer(),
  month = col_integer(),
  flow = col_character(),
  eunoneu = col_character(),
  country = col_character(),
  divcode = col_integer(),
  divdesc = col_character(),
  value = col_double(),
  tonnes = col_double()
)


ffdcountrylatest <- read_csv(here("data", "raw", ffd_country_latest_file),
                             col_names = c("year", "month", "flow", "eunoneu", "country", "divcode", "divdesc", "value", "tonnes"),
                             col_types = ffdcountrylatestcols,
                             skip = 1) %>% 
  mutate(date = as.Date(paste(year, month, "1", sep = "-"), format = "%Y-%m-%d"), 
         flow = case_when(flow == "Total Exports" ~ "Exports",
                          flow == "Total Imports" ~ "Imports"),
         value = value * 1000)


ffdcountry <- ffdcountryarchive %>% 
  bind_rows(ffdcountrylatest)

# save latest data
write_rds(ffdcountry, here("data", "tidy", "ffdcountry.rds"))

# Update non-response data -----------------------------------------------------

# https://www.uktradeinfo.com/trade-data/ots-custom-table/?id=cdee0450-14b3-4e26-bed5-16e2e23b105a
# 
# add EU flag
# tidy divcodes to numeric and filter out div 0 (could do this and get new url)

nonresponse <- read_csv(here("data", "raw", nr_latest_file),
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
  mutate(date = ymd(paste(year, month, "1", sep = "-")),
         flow = case_when(flow == "EU - Exports" ~ "Exports",
                          flow == "EU - Imports" ~ "Imports"),
         eunoneu = "EU",
         divcode = case_when(str_starts(sitc2, "00") ~ 0,
                             str_starts(sitc2, "01") ~ 1,
                             str_starts(sitc2, "02") ~ 2,
                             str_starts(sitc2, "03") ~ 3,
                             str_starts(sitc2, "04") ~ 4,
                             str_starts(sitc2, "05") ~ 5,
                             str_starts(sitc2, "06") ~ 6,
                             str_starts(sitc2, "07") ~ 7,
                             str_starts(sitc2, "08") ~ 8,
                             str_starts(sitc2, "09") ~ 9,
                             str_starts(sitc2, "11") ~ 11,
                             str_starts(sitc2, "22") ~ 22,
                             str_starts(sitc2, "41") ~ 41,
                             str_starts(sitc2, "42") ~ 42,
                             str_starts(sitc2, "43") ~ 43),
         valuenr = value_sterling) %>% 
  select(date, eunoneu, flow, divcode, valuenr) %>% 
  filter(divcode != 0)

# Save latest data
write_rds(nonresponse, here("data", "tidy", "nonresponse.rds"))
