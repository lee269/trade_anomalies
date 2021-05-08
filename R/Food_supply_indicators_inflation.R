#### HEAD ####

## Food supply indicators escalation ranges - inflation
## This script empirically derives escalation ranges for inflation indicators
## 
## Author: Rachel Dunne
## Date: May 2020


#### SET DIRECTORY TO WORK FROM AND ENABLE R PACKAGES REQUIRED ####
setwd("//defra.sharepoint.com@SSL/DavWWWRoot/teams/Team514/Crosscutting Evidence/OR_Hub/01_Consultancy_Projects/Covid19/Food_supply_indicators")

# usePackage - a function that will install a package if it's not already installed, and then load it if it's not already loaded
usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage("dplyr")
usePackage("readxl")
usePackage("ggplot2")
usePackage("stats")
usePackage("tseries")
usePackage("forecast")
usePackage("zoo")
usePackage("urca")
usePackage("scales")


#### READ IN AND CLEAN DATA ####

food_nonalcbev_inflation <- read_xls("Data/food_non_alcoholic_beverages_inflation.xls", skip = 7)
colnames(food_nonalcbev_inflation) <- c("Date", "Inflation_rate")

#reduce to just monthly data
food_nonalcbev_inflation_monthly <- food_nonalcbev_inflation[c(which(food_nonalcbev_inflation$Date == "1989 JAN"):nrow(food_nonalcbev_inflation)),]

# look at all items inflation for context
all_items_inflation <- read_xls("Data/all_items_inflation.xls", skip = 7)
colnames(all_items_inflation) <- c("Date", "Inflation_rate")
all_items_inflation_monthly <- all_items_inflation[c(which(all_items_inflation$Date == "1989 JAN"):nrow(all_items_inflation)),]


#create lookup of months to numbers
months <- data.frame( "character" = c("JAN", "FEB" ,"MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"), 
                      "number" = c("01","02","03","04","05","06","07","08","09","10","11","12"))

# match months in dataframes to months lookup
idx_months_food <- match(substring(food_nonalcbev_inflation_monthly$Date,6,8), months$character, incomparables = c(NA,""))
idx_months_all <- match(substring(all_items_inflation_monthly$Date,6,8), months$character, incomparables = c(NA,""))

# make dates in standard format
food_nonalcbev_inflation_monthly$Date <- paste0(substring(food_nonalcbev_inflation_monthly$Date,1,4), "-", months$number[idx_months_food], "-01")
all_items_inflation_monthly$Date <- paste0(substring(all_items_inflation_monthly$Date,1,4), "-", months$number[idx_months_all], "-01")

#convert dates column to type date
food_nonalcbev_inflation_monthly$Date <- as.Date(food_nonalcbev_inflation_monthly$Date, format = "%Y-%m-%d")
all_items_inflation_monthly$Date <- as.Date(all_items_inflation_monthly$Date, format = "%Y-%m-%d")

# add column giving group to each dataframe then combine
food_nonalcbev_inflation_monthly$Group <- "Food and non-alcoholic beverages"
all_items_inflation_monthly$Group <- "All items"

inflation_monthly <- rbind(food_nonalcbev_inflation_monthly, all_items_inflation_monthly)

#### PLOT DATA ####

inflation_monthly_plot <- ggplot(inflation_monthly, aes(x = Date, y = Inflation_rate, group = Group, colour = Group)) + geom_line() +
  theme_minimal() + ylab("Inflation rate (%)")

inflation_monthly_plot


#### DERIVE ESCALATION RANGE FOR FOOD INFLATION ####



#check nsdiffs() and ndiffs() - number of first and seasonal differences needed to make stationary
ndiffs(ts(food_nonalcbev_inflation_monthly$Inflation_rate, frequency = 12, start = 1989))
nsdiffs(ts(food_nonalcbev_inflation_monthly$Inflation_rate, frequency = 12, start = 1989))
#ndiffs() is 1 therefore find diff of time series to make stationary

# take first difference to make stationary
food_nonalcbev_inflation_monthly_diff <- diff(ts(food_nonalcbev_inflation_monthly$Inflation_rate, frequency = 12, start = 1989))

#check autocorrelation function tends to 0
acf(food_nonalcbev_inflation_monthly_diff)


# look at boxplot
boxplot(food_nonalcbev_inflation_monthly_diff)


# worried about at especially high levels of inflation so looking for values above the threshold of 1.5*IQR above UQ/Q3


#find quartiles and IQR
food_nonalcbev_inflation_diff_Q3 <- quantile(food_nonalcbev_inflation_monthly_diff, 0.75, na.rm = TRUE)
food_nonalcbev_inflation_diff_Q1 <- quantile(food_nonalcbev_inflation_monthly_diff, 0.25, na.rm = TRUE)
food_nonalcbev_inflation_diff_IQR <- food_nonalcbev_inflation_diff_Q3 - food_nonalcbev_inflation_diff_Q1


# find threshold for outliers (1.5*IQR + Q3)
food_nonalcbev_inflation_diff_upperlimit <- food_nonalcbev_inflation_diff_Q3 + 1.5*food_nonalcbev_inflation_diff_IQR

# add uncertainty range of +/-5% around upper limit
food_nonalcbev_inflation_diff_esc_range_upperlimit_LB <- food_nonalcbev_inflation_diff_upperlimit*0.95
food_nonalcbev_inflation_diff_esc_range_upperlimit_UB <- food_nonalcbev_inflation_diff_upperlimit*1.05

#make data dataframe so can plot
food_nonalcbev_inflation_diff_df <- cbind(food_nonalcbev_inflation_monthly$Date[-1], data.frame(food_nonalcbev_inflation_monthly_diff))
colnames(food_nonalcbev_inflation_diff_df) <- c("Date", "Inflation_rate")

#find intervention dates (within or above escalation range)
food_nonalcbev_inflation_diff_interventiondates <- food_nonalcbev_inflation_diff_df$Date[which(food_nonalcbev_inflation_diff_df$Inflation_rate > food_nonalcbev_inflation_diff_esc_range_upperlimit_LB) ]

#plot time series and escalation range
food_nonalcbev_inflation_diff_plot <- ggplot(food_nonalcbev_inflation_diff_df, aes(x = Date, y = Inflation_rate)) + geom_line() + 
  geom_rect(xmin = as.Date("1989-01-01"), xmax = Inf, ymin = food_nonalcbev_inflation_diff_esc_range_upperlimit_LB, ymax = food_nonalcbev_inflation_diff_esc_range_upperlimit_UB, alpha = 0.005, colour = NA, fill = "Blue") + 
  theme_minimal() + ylab("Change in inflation (%)") + scale_x_date(date_breaks = "1 year", labels = date_format("%Y"))+ theme(axis.text.x = element_text(size = 7, angle = 60))


food_nonalcbev_inflation_diff_plot

ggsave("Outputs/food_nonalcbev_inflation_diff_plot.png", food_nonalcbev_inflation_diff_plot, width = 15, height = 12, units = "cm")



