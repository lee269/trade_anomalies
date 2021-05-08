

thresholds <- function(data){
  require("ggplot2")
  
  data_diff <- diff(ts(data$value, frequency = 12, start = 2009))

  data_diff_q3 <- quantile(data_diff, 0.75, na.rm = TRUE)
  data_diff_q1 <- quantile(data_diff, 0.25, na.rm = TRUE)
  data_diff_iqr <- data_diff_q3 - data_diff_q1
  
  
  data_diff_ul <- data_diff_q3 + 1.5*data_diff_iqr
  data_diff_ullb <- data_diff_ul * 0.95
  data_diff_ulub <- data_diff_ul * 1.05
  
  
  data_diff_ll <- data_diff_q1 - 1.5*data_diff_iqr
  data_diff_lllb <- data_diff_ll * 0.95
  data_diff_llub <- data_diff_ll * 1.05
  
  
  
  data_diff_df <- cbind(data$date[-1], as.data.frame(data_diff), data_diff_ll, data_diff_ul)
  colnames(data_diff_df) <- c("date", "change", "limlower", "limupper")

  data_diff_df <- dplyr::as_tibble(data_diff_df)
  return(data_diff_df)
  # ggplot(data_diff_df, aes(x = date, y = value)) +
  #   geom_line() +
  #   geom_rect(xmin = as.Date("2009-02-01"),
  #             xmax = Inf, 
  #             ymin = data_diff_lllb,
  #             ymax = data_diff_llub, 
  #             alpha = 0.005, 
  #             colour = NA, 
  #             fill = "Blue") +
  #   geom_rect(xmin = as.Date("2009-02-01"),
  #             xmax = Inf, 
  #             ymin = data_diff_ullb,
  #             ymax = data_diff_ulub, 
  #             alpha = 0.005, 
  #             colour = NA, 
  #             fill = "Blue")  
    
}


tidy_thresholds <- function(data){
  
  data <- data %>% 
          mutate(diff = value - lag(value),
                 limupper = quantile(diff, 0.75, na.rm = TRUE) + (quantile(diff, 0.75, na.rm = TRUE) - quantile(diff, 0.25, na.rm = TRUE)) * 1.5,
                 limlower = quantile(diff, 0.25, na.rm = TRUE) - (quantile(diff, 0.75, na.rm = TRUE) - quantile(diff, 0.25, na.rm = TRUE)) * 1.5) %>% 
          select(date, diff, limlower, limupper) %>% 
          as_tibble()
  
  
  
  return(data)
}
