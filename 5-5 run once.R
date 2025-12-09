#جهت اجرای محاسبات سنگین

library(dplyr)
library(lubridate)
library(hms)
library(ggplot2)
library(optimx)

#دیتا
file_path <- "D:/Rego/ibm.txt" 
ibm_data <- read.table(file_path, header = FALSE)
colnames(ibm_data) <- c("Date_Time", "Volume", "bid_quote", "ask_quote", "transaction_price")

ibm_clean <- ibm_data %>%
  mutate(
    Date_Time_str = as.character(Date_Time),
    date_str = substr(Date_Time_str, 1, 6),
    time_sec_str = substr(Date_Time_str, 7, nchar(Date_Time_str)),
    date = ymd(paste0("19", date_str)),
    time = as_hms(as.numeric(time_sec_str)),
    datetime = date + seconds(as.numeric(time_sec_str))
  ) %>%
  dplyr::select(datetime, date, time, transaction_price)

#محاسبه و تعئیل دیرش ها 
durations_raw_df <- ibm_clean %>%
  filter(time >= hms(hours=9, minutes=30) & time <= hms(hours=16)) %>%
  arrange(datetime) %>% group_by(date) %>%
  mutate(duration = as.numeric(datetime - lag(datetime), units = "secs")) %>%
  ungroup() %>% filter(!is.na(duration) & duration > 0)

durations_raw_df <- durations_raw_df %>% mutate(time_interval = floor_date(datetime, "5 minutes"))
diurnal_factor <- durations_raw_df %>% group_by(time_of_day = hms::as_hms(time_interval)) %>% summarise(avg_duration = mean(duration))
durations_adjusted_df <- durations_raw_df %>% mutate(time_of_day = hms::as_hms(time_interval)) %>%
  left_join(diurnal_factor, by = "time_of_day") %>%
  mutate(adjusted_duration = duration / avg_duration) %>%
  filter(!is.na(adjusted_duration) & adjusted_duration > 0)

adjusted_durations <- durations_adjusted_df$adjusted_duration

# تعریف تابع درستنمایی
log_likelihood_wacd_robust <- function(params, data) {
  omega <- params[1]; gamma1 <- params[2]; omega1 <- params[3]; alpha <- params[4]
  n <- length(data); psi <- numeric(n); loglik <- 0
  psi[1] <- mean(data)
  if (alpha <= 0.1) return(1e10)
  scale_factor <- 1 / gamma(1 + 1/alpha)
  for (i in 2:n) {
    psi[i] <- omega + gamma1 * data[i-1] + omega1 * psi[i-1]
    if (psi[i] <= 1e-9 || is.na(psi[i])) return(1e10)
    log_density <- dweibull(data[i], shape = alpha, scale = psi[i] * scale_factor, log = TRUE)
    if(is.na(log_density) || is.infinite(log_density)) return(1e10)
    loglik <- loglik + log_density
  }
  return(-loglik)
}

# اجرای بهینه سازی
start_params <- c(omega=0.1, gamma1=0.1, omega1=0.8, alpha=0.9)
lower_bounds <- c(1e-6, 1e-6, 1e-6, 1e-6)
upper_bounds <- c(Inf, 0.999, 0.999, Inf)

print("شروع فرآیند بهینه‌سازی... این ممکن است زمان‌بر باشد.")
fit_wacd_ibm <- suppressWarnings({
  optimx(start_params, log_likelihood_wacd_robust, data = adjusted_durations, 
         method = "L-BFGS-B", lower = lower_bounds, upper = upper_bounds)
})

saveRDS(fit_wacd_ibm, file = "fit_wacd_model.rds")
print("فرآیند تمام شد. نتیجه مدل در فایل 'fit_wacd_model.rds' در پوشه پروژه شما ذخیره شد.")

