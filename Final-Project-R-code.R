
library(dplyr)
library(lubridate)
library(ggplot2)
library(hms)

#Main Data for IBM since 11/1/90-1/31/91 4 Month
file_path <- "D:/Rego/ibm.txt"
ibm_data <- read.table(file_path, header = FALSE)
colnames(ibm_data) <- c("Date_Time", "Volume", "bid_quote", "ask_quote", "transaction_price")

head(ibm_data)


#Cleaning Data - (Date&Time Format)
ibm_clean <- ibm_data %>%
  mutate(
    Date_Time_str = as.character(Date_Time),
    date_str = substr(Date_Time_str, 1, 6),
    time_sec_str = substr(Date_Time_str, 7, nchar(Date_Time_str)),
    date = ymd(paste0("19", date_str)),
    time = as_hms(as.numeric(time_sec_str)),
    datetime = date + seconds(as.numeric(time_sec_str))
  ) %>%
  dplyr::select(
    datetime, 
    date, 
    time, 
    Volume, 
    bid_quote, 
    ask_quote, 
    transaction_price
  )
head(ibm_clean)

#Missing values
missing_per_column <- colSums(is.na(ibm_clean))
cat("تعداد مقادیر گمشده در هر ستون:\n")
print(missing_per_column)



# محاسبه متغیرهای تحلیلی
data_analytical <- ibm_clean %>%
  arrange(datetime) %>%
  group_by(date) %>%
  mutate(
    duration = as.numeric(datetime - lag(datetime), units = "secs"),
    price_change = transaction_price - lag(transaction_price)
  ) %>%
  ungroup() %>%
  filter(!is.na(price_change))

print("داده‌های تحلیلی نهایی:")
print(head(dplyr::select(data_analytical, datetime, duration, price_change)))

tick_size <- 0.125
data_analytical$price_change_ticks <- data_analytical$price_change / tick_size

print("درصد فراوانی تغییرات قیمت (تیک):")
print(prop.table(table(data_analytical$price_change_ticks)) * 100)


#بازده لگاریتمی
#بررسی میکنیم قیمت صفر یا منفی در داده ها نباشد
if (any(ibm_clean$transaction_price <= 0)) {
  stop("خطا: قیمت‌های غیرمثبت یافت شد که محاسبه لگاریتم را غیرممکن می‌کند.")
}
# محاسبه بازده لگاریتمی برای هر معامله
data_log_return <- ibm_clean %>%
  arrange(datetime) %>%
  group_by(date) %>% # محاسبات برای هر روز به صورت جداگانه انجام می‌شود
  mutate(
    log_return = log(transaction_price) - lag(log(transaction_price))
  ) %>%
  ungroup() %>%
  filter(!is.na(log_return)) # حذف اولین مشاهده هر روز که مقدار NA دارد
print("داده‌های تحلیلی جدید با بازده لگاریتمی:")
print(head(dplyr::select(data_log_return, datetime, log_return)))


#############################################################################
#simple plot
ibm_aggregated <- ibm_clean %>%
  mutate(
    time_interval = floor_date(datetime, "5 minutes")
  )

price_trend_data <- ibm_aggregated %>%
  group_by(time_interval) %>%
  summarise(
    
    interval_price = last(transaction_price) 
  ) %>%
  ungroup()

print("داده‌های تجمیع شده در بازه‌های ۵ دقیقه‌ای:")
head(price_trend_data)

ggplot(data = price_trend_data, aes(x = time_interval, y = interval_price)) +
  geom_line(color = "blue") + # رسم نمودار خطی
  labs(
    title = "نمودار روند کلی قیمت سهام IBM (بازه های ۵ دقیقه ای)",
    subtitle = "دوره زمانی: ۱ نوامبر ۱۹۹۰ تا ۳۱ ژانویه ۱۹۹۱",
    x = "تاریخ و زمان",
    y = "قیمت پایانی در بازه ۵ دقیقه‌ای"
  ) +
  theme_minimal() 

#############################################################################
#5-1 


set.seed(123)
n <- 1000       
mu <- 0.01      
sigma <- 0.02   
pi <- 0.3      

true_returns <- rnorm(n, mean = mu, sd = sigma)
traded <- rbinom(n, size = 1, prob = 1 - pi)

observed_returns <- numeric(n)
carry <- 0  

for (t in 1:n) {
  if (traded[t] == 1) {
    observed_returns[t] <- true_returns[t] + carry
    carry <- 0
  } else {
    carry <- carry + true_returns[t]
    observed_returns[t] <- 0  # 
  }
}

par(mfrow = c(2, 1))
plot(true_returns, type = "l", col = "blue", main = "بازده واقعی r_t")
plot(observed_returns, type = "l", col = "darkred", main = "بازده مشاهده‌شده r^o_t")

acf_real <- acf(true_returns, plot = FALSE)
acf_observed <- acf(observed_returns, plot = FALSE)

rho1_real <- acf_real$acf[2]
rho1_obs  <- acf_observed$acf[2]

cat("خودهمبستگی وقفه اول:\n")
cat("بازده واقعی (r_t):", round(rho1_real, 4), "\n")
cat("بازده مشاهده‌شده (r^o_t):", round(rho1_obs, 4), "\n")
#----------------------------------------------------------------------------

library(tidyr)
one_day_data <- ibm_clean %>%
  filter(date == min(date))

start_time <- floor_date(min(one_day_data$datetime), "minute")
end_time <- ceiling_date(max(one_day_data$datetime), "minute")
regular_time_grid <- seq(from = start_time, to = end_time, by = "1 min")
regular_time_df <- data.frame(datetime_grid = regular_time_grid)

sampled_prices <- regular_time_df %>%
  left_join(
    one_day_data %>% dplyr::select(datetime, transaction_price),
    by = join_by(datetime_grid >= datetime)
  ) %>%
  group_by(datetime_grid) %>%
  summarise(
    sampled_price = last(transaction_price)
  ) %>%
  ungroup()

sampled_prices <- sampled_prices %>%
  fill(sampled_price, .direction = "down")

sampled_prices <- sampled_prices %>%
  mutate(
    sampled_return = sampled_price - lag(sampled_price)
  ) %>%
  filter(!is.na(sampled_return))

print("--- تحلیل ACF برای سری زمانی نمونه‌برداری شده منظم (هر ۱ دقیقه) ---")
acf(sampled_prices$sampled_return, main = "ACF بازده‌های نمونه‌برداری شده منظم")

#############################################################################
#5-2

#شبیه سازی مثال کتاب
set.seed(456)
n_trades <- 2000      
initial_price <- 100  
spread <- 0.10        

price_innovations <- rnorm(n_trades, mean = 0, sd = 0.05)
P_star <- cumsum(c(initial_price, price_innovations))
P_bid <- P_star - spread / 2
P_ask <- P_star + spread / 2
trade_direction <- sample(c(-1, 1), n_trades + 1, replace = TRUE)
P_obs <- P_star + trade_direction * (spread / 2)

price_changes_obs <- diff(P_obs)
par(mfrow = c(1,1))

acf(price_changes_obs, main = "ACF تغییرات قیمت: اثر Bid-Ask Bounce")

#----------------------------------------------------------------------------

#شبیه سازی روی دیتای ibm 
library(dplyr)
library(lubridate)

data_analytical <- ibm_clean %>%
  arrange(datetime) %>%
  group_by(date) %>%
  mutate(
    duration = as.numeric(datetime - lag(datetime), units = "secs"),
    price_change = transaction_price - lag(transaction_price)
  ) %>%
  ungroup() %>%
  filter(!is.na(price_change))

print("--- تحلیل ACF برای بررسی Bid-Ask Bounce روی داده‌های واقعی IBM ---")
## [1] "--- تحلیل ACF برای بررسی Bid-Ask Bounce روی داده‌های واقعی IBM ---"
acf(data_analytical$price_change, 
    main="ACF تغییرات قیمت معاملات IBM (تأیید Bid-Ask Bounce)",
    ylab="خودهمبستگی",
    xlab="وقفه (Lag)")

#############################################################################
#5-3


library(highfrequency)
library(data.table)
library(ggplot2)
library(lubridate)

data("sampleTDataRaw")
dt_trades <- as.data.table(sampleTDataRaw)

dt_trades[, DATETIME := ymd_hms(DT)]
dt_trades[, DATE := as.Date(DATETIME)]
dt_trades[, TIME_OF_DAY := format(DATETIME, format = "%H:%M:%S")]

dt_trades[, TIME_5MIN := floor_date(DATETIME, unit = "5 minutes")]

trades_per_interval <- dt_trades[, .N, by = .(DATE, TIME_5MIN)]

trades_per_interval[, TIME_OF_DAY := format(TIME_5MIN, format = "%H:%M")]

diurnal_pattern <- trades_per_interval[, .(AVG_TRADES = mean(N)), by = TIME_OF_DAY]

diurnal_pattern[, TIME_SORT := hms::as_hms(paste0(TIME_OF_DAY, ":00"))]
setorder(diurnal_pattern, TIME_SORT)

ggplot(diurnal_pattern, aes(x = TIME_SORT, y = AVG_TRADES)) +
  geom_line(color = "darkblue") +
  labs(title = "الگوی روزانه (Diurnal Pattern) در فعالیت معاملاتی",
       x = "زمان روز", y = "میانگین تعداد معاملات در بازه ۵ دقیقه‌ای") +
  theme_minimal()

#----------------------------------------------------------------------------

#انجام مراحل با داده های IBM
library(data.table)
library(lubridate)
library(ggplot2)
library(hms)

ibm <- fread("D:/Rego/ibm.txt")
colnames(ibm) <- c("Date_Time" , "Volume" , " bid quote", "ask quote", "transaction price")
head(ibm)


ibm[, DATETIME_STR := sprintf("%06d%06d",
                              as.integer(substr(`Date_Time`, 1, 6)),  # تاریخ
                              as.integer(substr(`Date_Time`, 7, 12)))]  # ساعت

ibm[, DATETIME := ymd_hms(DATETIME_STR, tz = "UTC")]
ibm[, DATE := as.Date(DATETIME)]
ibm[, TIME_5MIN := floor_date(DATETIME, unit = "5 minutes")]
ibm[, TIME_OF_DAY := format(TIME_5MIN, format = "%H:%M")]
trades_per_interval <- ibm[, .N, by = .(DATE, TIME_5MIN)]
trades_per_interval[, TIME_OF_DAY := format(TIME_5MIN, format = "%H:%M")]

diurnal_pattern <- trades_per_interval[, .(AVG_TRADES = mean(N)), by = TIME_OF_DAY]

unique(diurnal_pattern$TIME_OF_DAY)

diurnal_pattern <- diurnal_pattern[!is.na(TIME_OF_DAY) & nchar(TIME_OF_DAY) == 5]
diurnal_pattern[, TIME_SORT := hms::as_hms(paste0(TIME_OF_DAY, ":00"))]

setorder(diurnal_pattern, TIME_SORT)
ggplot(diurnal_pattern, aes(x = TIME_SORT, y = AVG_TRADES)) +
  geom_line(color = "steelblue", size = 1) +
  labs(title = "الگوی روزانه معاملات برای IBM",
       x = "زمان روز", y = "میانگین تعداد معاملات در بازه ۵ دقیقه‌ای") +
  theme_minimal()

#############################################################################
#5-4


#Probit

library(MASS)
set.seed(789)
n_points <- 2500

lag_pchg <- rnorm(n_points)
duration <- runif(n_points)
y_latent <- -0.5 * lag_pchg + 0.2 * duration + rnorm(n_points)

thresholds <- c(-1.5, -0.5, 0.5, 1.5) 
y_observed <- cut(y_latent, breaks = c(-Inf, thresholds, Inf), labels = c("-2", "-1", "0", "1", "2"))

model_data <- data.frame(y = factor(y_observed, ordered = TRUE), lag_pchg, duration)
ordered_probit_model <- polr(y ~ lag_pchg + duration, data = model_data, method = "probit", Hess = TRUE)

summary(ordered_probit_model)

#                            ------------------------------------


library(dplyr)
library(MASS)

tick_size <- 0.125
model_data_probit <- data_analytical %>%
  
  mutate(
    price_change_ticks = price_change / tick_size,
    price_change_limited = case_when(
      price_change_ticks >= 2 ~ "2",
      price_change_ticks == 1 ~ "1",
      price_change_ticks == 0 ~ "0",
      price_change_ticks == -1 ~ "-1",
      price_change_ticks <= -2 ~ "-2"
    ),
    
    y_ordered = factor(price_change_limited, levels=c("-2", "-1", "0", "1", "2"), ordered = TRUE),
    lag_price_change = lag(price_change_ticks)
  ) %>%
  
  filter(!is.na(lag_price_change) & !is.na(y_ordered))

ordered_probit_model_ibm <- polr(y_ordered ~ lag_price_change, 
                                 data = model_data_probit, 
                                 method = "probit", 
                                 Hess = TRUE)
print("--- نتایج مدل پروبیت مرتب (ساده‌شده) برای داده‌های IBM ---")
summary(ordered_probit_model_ibm)


#----------------------------------------------------------------------------
#تجزیه ای
set.seed(101)
n_points <- 3000

spread <- runif(n_points, 0.1, 0.2)
imbalance <- rnorm(n_points)
volume <- rpois(n_points, lambda = 5)

prob_occurrence <- plogis(10 * (spread - 0.15))
change_occurred <- rbinom(n_points, 1, prob_occurrence)

prob_direction_up <- plogis(1.5 * imbalance)
change_direction <- ifelse(rbinom(n_points, 1, prob_direction_up) == 1, 1, -1)

lambda_size <- exp(0.1 * volume)
change_size <- rpois(n_points, lambda = lambda_size) + 1 

pchg <- change_occurred * change_direction * change_size
pchg[change_occurred == 0] <- 0 

model_data <- data.frame(pchg, spread, imbalance, volume, change_occurred)
price_change_data <- subset(model_data, change_occurred == 1)
price_change_data$direction_up <- ifelse(price_change_data$pchg > 0, 1, 0)
price_change_data$size <- abs(price_change_data$pchg)

model_occurrence <- glm(change_occurred ~ spread, data = model_data, family = binomial(link = "logit"))
model_direction <- glm(direction_up ~ imbalance, data = price_change_data, family = binomial(link = "logit"))
model_size <- glm(size ~ volume, data = price_change_data, family = poisson())

summary(model_occurrence)

summary(model_direction)

summary(model_size)



tick_size <- 0.125
model_data_decomp <- data_analytical %>%
  mutate(
    
    price_change_ticks = price_change / tick_size,
    change_occurred = ifelse(price_change_ticks != 0, 1, 0),
    direction_up = ifelse(price_change_ticks > 0, 1, 0),
    change_size = round(abs(price_change_ticks)),
    lag_direction = lag(sign(price_change_ticks)),
    lag_size = lag(change_size)
  ) %>%
  filter(!is.na(lag_direction) & !is.na(lag_size))


model_occurrence <- glm(change_occurred ~ lag_size, 
                        data = model_data_decomp, 
                        family = binomial(link = "logit"))

data_with_change <- filter(model_data_decomp, change_size > 0)

model_direction <- glm(direction_up ~ lag_direction, 
                       data = data_with_change, 
                       family = binomial(link = "logit"))

model_size <- glm(I(change_size - 1) ~ lag_size, 
                  data = data_with_change, 
                  family = poisson(link = "log"))

print("--- نتایج مدل وقوع (Action Model) ---")
summary(model_occurrence)

print("--- نتایج مدل جهت (Direction Model) ---")
summary(model_direction)

print("--- نتایج مدل اندازه (Size Model) ---")
summary(model_size)





# ===================================================================
# بخش تحلیل باقیمانده‌های مدل تجزیه‌ای (ADS)
# ===================================================================

# برازش مدل‌ها بر روی کل داده‌ها
model_occurrence_full <- glm(change_occurred ~ lag_size, data = model_data_full, family = binomial(link = "logit"))
# کد اصلی شما که ممکن است منجر به خطا شود
# data_with_change_full <- filter(model_data_full, change_occurred == 1)

# کد اصلاح شده و صحیح
# کد اصلاح شده و صحیح
data_with_change_full <- filter(model_data_full, change_size > 0)

model_direction_full <- glm(direction_up ~ lag_direction, data = data_with_change_full, family = binomial(link = "logit"))model_size_full <- glm(I(change_size - 1) ~ lag_size, data = data_with_change_full, family = poisson(link = "log"))

# استخراج باقیمانده‌های پیرسون
res_occurrence <- residuals(model_occurrence_full, type = "pearson")
res_direction <- residuals(model_direction_full, type = "pearson")
res_size <- residuals(model_size_full, type = "pearson")

# رسم نمودارهای ACF
par(mfrow=c(3,1))
acf(res_occurrence, main="ACF باقیمانده‌های مدل وقوع")
acf(res_direction, main="ACF باقیمانده‌های مدل جهت")
acf(res_size, main="ACF باقیمانده‌های مدل اندازه")
par(mfrow=c(1,1))
































# ===================================================================
# بخش ارزیابی برون‌نمونه‌ای با روش Train/Test
# مقایسه مدل پروبیت مرتب و مدل تجزیه‌ای (ADS)
# ===================================================================

library(dplyr)
library(MASS)

# --- 1. آماده‌سازی کامل داده‌ها برای هر دو مدل ---
tick_size <- 0.125
model_data_full <- data_analytical %>%
  mutate(
    price_change_ticks = price_change / tick_size,
    price_change_limited = case_when(
      price_change_ticks >= 2 ~ "2",
      price_change_ticks == 1 ~ "1",
      price_change_ticks == 0 ~ "0",
      price_change_ticks == -1 ~ "-1",
      price_change_ticks <= -2 ~ "-2"
    ),
    y_ordered = factor(price_change_limited, levels=c("-2", "-1", "0", "1", "2"), ordered = TRUE),
    lag_price_change = lag(price_change_ticks),
    change_occurred = ifelse(price_change_ticks!= 0, 1, 0),
    direction_up = ifelse(price_change_ticks > 0, 1, 0),
    change_size = round(abs(price_change_ticks)),
    lag_direction = lag(sign(price_change_ticks)),
    lag_size = lag(change_size)
  ) %>%
  filter(!is.na(lag_price_change) &!is.na(lag_direction) &!is.na(lag_size))

# --- 2. تقسیم داده‌ها به بخش آموزش و آزمون (80% - 20%) ---
set.seed(123)
split_ratio <- 0.8
split_index <- floor(nrow(model_data_full) * split_ratio)
train_data <- model_data_full[1:split_index, ]
test_data <- model_data_full[(split_index + 1):nrow(model_data_full), ]

# --- 3. مدل پروبیت مرتب: آموزش و ارزیابی ---
model_probit_train <- polr(y_ordered ~ lag_price_change, data = train_data, method = "probit", Hess = TRUE)
pred_probit_test <- predict(model_probit_train, newdata = test_data)
accuracy_probit <- mean(as.character(pred_probit_test) == as.character(test_data$y_ordered))

# --- 4. مدل تجزیه‌ای (ADS): آموزش و ارزیابی ---
model_occurrence_train <- glm(change_occurred ~ lag_size, data = train_data, family = binomial(link = "logit"))
train_data_with_change <- filter(train_data, change_occurred == 1)
model_direction_train <- glm(direction_up ~ lag_direction, data = train_data_with_change, family = binomial(link = "logit"))
model_size_train <- glm(I(change_size - 1) ~ lag_size, data = train_data_with_change, family = poisson(link = "log"))

prob_occurrence_pred <- predict(model_occurrence_train, newdata = test_data, type = "response")
pred_occurrence <- ifelse(prob_occurrence_pred > 0.5, 1, 0)
prob_direction_pred <- predict(model_direction_train, newdata = test_data, type = "response")
pred_direction <- ifelse(prob_direction_pred > 0.5, 1, -1)
pred_size_lambda <- predict(model_size_train, newdata = test_data, type = "response")
pred_size <- round(pred_size_lambda + 1)
pred_ads_combined <- ifelse(pred_occurrence == 0, 0, pred_direction * pred_size)
pred_ads_categorized <- factor(case_when(
  pred_ads_combined >= 2 ~ "2",
  pred_ads_combined == 1 ~ "1",
  pred_ads_combined == 0 ~ "0",
  pred_ads_combined == -1 ~ "-1",
  pred_ads_combined <= -2 ~ "-2"
), levels = c("-2", "-1", "0", "1", "2"), ordered = TRUE)
accuracy_ads <- mean(as.character(pred_ads_categorized) == as.character(test_data$y_ordered), na.rm = TRUE)

# --- 5. مقایسه نهایی ---
cat("--- مقایسه نهایی عملکرد پیش‌بینی ---\n")
cat(sprintf("دقت مدل پروبیت مرتب: %.2f%%\n", accuracy_probit * 100))
cat(sprintf("دقت مدل تجزیه‌ای (ADS): %.2f%%\n", accuracy_ads * 100))
#############################################################################
#5-5

library(optimx)
set.seed(123)
n_obs <- 1000

omega_true <- 0.2
gamma1_true <- 0.15
omega1_true <- 0.8
alpha_true <- 1.2 

durations <- numeric(n_obs)
psi <- numeric(n_obs)
psi[1] <- 1 

scale_factor <- 1 / gamma(1 + 1/alpha_true)
epsilon <- rweibull(n_obs, shape = alpha_true, scale = scale_factor)

durations[1] <- psi[1] * epsilon[1]

for (i in 2:n_obs) {
  psi[i] <- omega_true + gamma1_true * durations[i-1] + omega1_true * psi[i-1]
  durations[i] <- psi[i] * epsilon[i]
}

par(mfrow=c(2,1))
plot(durations, type='l', main="سری دیرش شبیه‌سازی شده WACD(1,1)", ylab="دیرش")
acf(durations, main="ACF سری دیرش شبیه‌سازی شده")

par(mfrow=c(1,1))

log_likelihood_wacd <- function(params, data) {
  omega <- params[1]
  gamma1 <- params[2]
  omega1 <- params[3]
  alpha <- params[4]
  
  n <- length(data)
  psi <- numeric(n)
  loglik <- 0
  
  psi[1] <- mean(data)
  
  scale_factor <- 1 / gamma(1 + 1/alpha)
  
  for (i in 2:n) {
    psi[i] <- omega + gamma1 * data[i-1] + omega1 * psi[i-1]
    if (psi[i] <= 0 || alpha <= 0) return(1e10)
    
    loglik <- loglik + dweibull(data[i], shape = alpha, scale = psi[i] * scale_factor, log = TRUE)
  }
  
  return(-loglik)
}

start_params <- c(0.1, 0.1, 0.7, 1.0)
lower_bounds <- c(1e-6, 1e-6, 1e-6, 1e-6)
upper_bounds <- c(Inf, 1, 1, Inf)

fit <- optimx(start_params, log_likelihood_wacd, data = durations, 
              method = "L-BFGS-B", lower = lower_bounds, upper = upper_bounds)

print("پارامترهای واقعی:")
print(c(omega=omega_true, gamma1=gamma1_true, omega1=omega1_true, alpha=alpha_true))

print("پارامترهای برآورد شده:")
print(fit[1, 1:4])

#--------------------------------------------------------------------------------


library(dplyr)
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
fit_wacd_ibm <- readRDS("fit_wacd_model.rds")
print("--- پارامترهای برآورد شده برای مدل WACD(1,1) ---")
print(fit_wacd_ibm[1, 1:4])

params_est <- as.numeric(fit_wacd_ibm[1, 1:4])
omega_est <- params_est[1]; gamma1_est <- params_est[2]; omega1_est <- params_est[3]

adjusted_durations <- durations_adjusted_df$adjusted_duration

psi_hat <- numeric(length(adjusted_durations))
psi_hat[1] <- mean(adjusted_durations)
for (i in 2:length(adjusted_durations)) {
  psi_hat[i] <- omega_est + gamma1_est * adjusted_durations[i-1] + omega1_est * psi_hat[i-1]
}
residuals_std <- adjusted_durations / psi_hat

print("--- بررسی خودهمبستگی در باقی‌مانده‌های استاندارد شده ---")
acf(residuals_std, main="ACF باقی‌مانده‌های استاندارد شده مدل WACD")

persistence <- gamma1_est + omega1_est
cat(sprintf("\nپارامتر ماندگاری (Persistence) مدل: %.4f\n", persistence))

#############################################################################
#5-6
fit_tacd_ibm <- readRDS("fit_tacd_model.rds")

results_df <- data.frame(
  Regime = c("Regime 1 (Active)", "Regime 2 (Calm)"),
  Omega = c(fit_tacd_ibm$p1, fit_tacd_ibm$p5),
  Gamma1 = c(fit_tacd_ibm$p2, fit_tacd_ibm$p6),
  Omega1 = c(fit_tacd_ibm$p3, fit_tacd_ibm$p7),
  Alpha = c(fit_tacd_ibm$p4, fit_tacd_ibm$p8)
)

print("--- پارامترهای برآورد شده برای مدل TACD(1,1) ---")
print(results_df)

persistence_r1 <- results_df$Gamma1[1] + results_df$Omega1[1]
persistence_r2 <- results_df$Gamma1[2] + results_df$Omega1[2]

cat(sprintf("\nماندگاری در رژیم ۱ (فعال): %.3f\n", persistence_r1))
cat(sprintf("ماندگاری در رژیم ۲ (آرام): %.3f\n", persistence_r2))


#############################################################################
# بخش جدید: اعتبارسنجی مدل TACD(1,1) با تحلیل باقیمانده‌ها
#############################################################################

# --- 1. استخراج پارامترهای تخمین زده شده از مدل TACD ---
# (این بخش در کد اصلی شما وجود دارد و ما از همان پارامترها استفاده می‌کنیم)

# پارامترهای رژیم ۱ (فعال)
omega_r1 <- fit_tacd_ibm$p1
gamma1_r1 <- fit_tacd_ibm$p2
omega1_r1 <- fit_tacd_ibm$p3

# پارامترهای رژیم ۲ (آرام)
omega_r2 <- fit_tacd_ibm$p5
gamma1_r2 <- fit_tacd_ibm$p6
omega1_r2 <- fit_tacd_ibm$p7

# نکته مهم: مقدار آستانه باید از مدل تخمین زده شده استخراج شود.
# در اینجا ما از یک مقدار فرضی استفاده می‌کنیم. شما باید این مقدار را با مقدار واقعی جایگزین کنید.
threshold <- 3.79 # این مقدار باید از نتایج برازش مدل TACD شما به دست آید

# --- 2. محاسبه میانگین شرطی دیرش (psi_hat) بر اساس دو رژیم ---

# از همان سری دیرش‌های تعدیل‌شده که برای مدل WACD استفاده کردیم، بهره می‌بریم
adjusted_durations <- durations_adjusted_df$adjusted_duration

psi_hat_tacd <- numeric(length(adjusted_durations))
psi_hat_tacd[1] <- mean(adjusted_durations) # مقداردهی اولیه

# حلقه برای محاسبه بازگشتی psi بر اساس رژیم‌ها
for (i in 2:length(adjusted_durations)) {
  
  # بررسی اینکه دیرش قبلی در کدام رژیم قرار می‌گیرد
  if (adjusted_durations[i-1] <= threshold) { 
    # اگر دیرش قبلی کوتاه بود -> استفاده از پارامترهای رژیم ۱ (فعال)
    psi_hat_tacd[i] <- omega_r1 + gamma1_r1 * adjusted_durations[i-1] + omega1_r1 * psi_hat_tacd[i-1]
  } else { 
    # اگر دیرش قبلی بلند بود -> استفاده از پارامترهای رژیم ۲ (آرام)
    psi_hat_tacd[i] <- omega_r2 + gamma1_r2 * adjusted_durations[i-1] + omega1_r2 * psi_hat_tacd[i-1]
  }
}

# --- 3. محاسبه باقیمانده‌های استاندارد شده ---
residuals_std_tacd <- adjusted_durations / psi_hat_tacd

# --- 4. رسم نمودار ACF برای اعتبارسنجی مدل ---
print("--- بررسی خودهمبستگی در باقی‌مانده‌های استاندارد شده مدل TACD ---")
acf(residuals_std_tacd, main="ACF باقی‌مانده‌های استاندارد شده مدل TACD")

















#############################################################################
# بخش جدید: ارزیابی برون‌نمونه‌ای با روش Train/Test
# مقایسه مدل WACD و مدل TACD
#############################################################################

# --- 1. آماده‌سازی و تقسیم داده‌های دیرش خام ---

# محاسبه دیرش‌های خام از کل مجموعه داده (این کد در بخش 5-5 شما وجود دارد)
durations_raw_df <- ibm_clean %>%
  filter(time >= hms(hours=9, minutes=30) & time <= hms(hours=16)) %>%
  arrange(datetime) %>% 
  group_by(date) %>%
  mutate(duration = as.numeric(datetime - lag(datetime), units = "secs")) %>%
  ungroup() %>% 
  filter(!is.na(duration) & duration > 0)

# تقسیم داده‌ها به بخش آموزش و آزمون (80% - 20%)
set.seed(123) # برای تکرارپذیری
split_ratio <- 0.8
split_index <- floor(nrow(durations_raw_df) * split_ratio)

train_raw_df <- durations_raw_df[1:split_index, ]
test_raw_df <- durations_raw_df[(split_index + 1):nrow(durations_raw_df), ]

cat("تعداد مشاهدات دیرش در بخش آموزش:", nrow(train_raw_df), "\n")
cat("تعداد مشاهدات دیرش در بخش آزمون:", nrow(test_raw_df), "\n\n")

# --- 2. تعدیل الگوی روزانه (فقط با استفاده از داده‌های آموزش) ---

# محاسبه فاکتور تعدیل روزانه فقط از داده‌های آموزش
diurnal_factor_train <- train_raw_df %>%
  mutate(time_interval = floor_date(datetime, "5 minutes")) %>%
  group_by(time_of_day = hms::as_hms(time_interval)) %>%
  summarise(avg_duration = mean(duration))

# اعمال فاکتور تعدیل بر هر دو بخش
train_adjusted_df <- train_raw_df %>%
  mutate(time_interval = floor_date(datetime, "5 minutes"), time_of_day = hms::as_hms(time_interval)) %>%
  left_join(diurnal_factor_train, by = "time_of_day") %>%
  mutate(adjusted_duration = duration / avg_duration) %>%
  filter(!is.na(adjusted_duration) & adjusted_duration > 0)

test_adjusted_df <- test_raw_df %>%
  mutate(time_interval = floor_date(datetime, "5 minutes"), time_of_day = hms::as_hms(time_interval)) %>%
  left_join(diurnal_factor_train, by = "time_of_day") %>%
  mutate(adjusted_duration = duration / avg_duration) %>%
  filter(!is.na(adjusted_duration) & adjusted_duration > 0)

train_durations <- train_adjusted_df$adjusted_duration
test_durations <- test_adjusted_df$adjusted_duration

# --- 3. برازش مدل WACD(1,1) فقط بر روی داده‌های آموزش ---
# (از تابع log-likelihood که قبلاً نوشتید استفاده می‌کنیم)
start_params <- c(0.1, 0.1, 0.7, 1.0)
lower_bounds <- c(1e-6, 1e-6, 1e-6, 1e-6)
upper_bounds <- c(Inf, 1, 1, Inf)

fit_wacd_train <- optimx(start_params, log_likelihood_wacd, data = train_durations, 
                         method = "L-BFGS-B", lower = lower_bounds, upper = upper_bounds)

params_est_wacd_train <- as.numeric(fit_wacd_train[1, 1:4])
omega_wacd <- params_est_wacd_train[1]
gamma1_wacd <- params_est_wacd_train[2]
omega1_wacd <- params_est_wacd_train[3]

cat("--- پارامترهای برآورد شده WACD بر روی داده‌های آموزش ---\n")
print(params_est_wacd_train)

# --- 4. پیش‌بینی یک-گام-به-جلو با مدل WACD ---
full_durations <- c(train_durations, test_durations)
psi_hat_wacd_full <- numeric(length(full_durations))
psi_hat_wacd_full[1] <- mean(train_durations) # شروع با میانگین داده‌های آموزش

for (i in 2:length(full_durations)) {
  psi_hat_wacd_full[i] <- omega_wacd + gamma1_wacd * full_durations[i-1] + omega1_wacd * psi_hat_wacd_full[i-1]
}

# جدا کردن پیش‌بینی‌های مربوط به بخش آزمون
predictions_test_wacd <- psi_hat_wacd_full[(length(train_durations) + 1):length(full_durations)]

# محاسبه خطای پیش‌بینی (MSE) برای مدل WACD
mse_wacd <- mean((test_durations - predictions_test_wacd)^2)
cat(sprintf("\nمیانگین مربعات خطای پیش‌بینی (MSE) برای مدل WACD: %.6f\n", mse_wacd))


# --- 5. برازش و ارزیابی مدل TACD(1,1) ---
# نکته: برای این بخش، شما به یک تابع برای برازش مدل TACD نیاز دارید.
# در اینجا ما از نتایج ذخیره شده شما به عنوان یک مثال فرضی استفاده می‌کنیم.
# در پروژه واقعی، باید مدل TACD را نیز بر روی train_durations برازش دهید.

# فرض می‌کنیم پارامترهای زیر از برازش مدل TACD بر روی داده آموزش به دست آمده‌اند:
# (این مقادیر فرضی هستند و باید با نتایج واقعی جایگزین شوند)
fit_tacd_train_params <- readRDS("fit_tacd_model.rds") # به عنوان مثال از فایل کلی استفاده می‌کنیم
threshold <- 3.79 # مقدار آستانه باید از داده آموزش تخمین زده شود

# پارامترهای رژیم ۱ (فعال)
omega_r1 <- fit_tacd_train_params$p1
gamma1_r1 <- fit_tacd_train_params$p2
omega1_r1 <- fit_tacd_train_params$p3

# پارامترهای رژیم ۲ (آرام)
omega_r2 <- fit_tacd_train_params$p5
gamma1_r2 <- fit_tacd_train_params$p6
omega1_r2 <- fit_tacd_train_params$p7

# پیش‌بینی یک-گام-به-جلو با مدل TACD
psi_hat_tacd_full <- numeric(length(full_durations))
psi_hat_tacd_full[1] <- mean(train_durations)

for (i in 2:length(full_durations)) {
  if (full_durations[i-1] <= threshold) { # رژیم ۱ (فعال)
    psi_hat_tacd_full[i] <- omega_r1 + gamma1_r1 * full_durations[i-1] + omega1_r1 * psi_hat_tacd_full[i-1]
  } else { # رژیم ۲ (آرام)
    psi_hat_tacd_full[i] <- omega_r2 + gamma1_r2 * full_durations[i-1] + omega1_r2 * psi_hat_tacd_full[i-1]
  }
}

# جدا کردن پیش‌بینی‌های مربوط به بخش آزمون
predictions_test_tacd <- psi_hat_tacd_full[(length(train_durations) + 1):length(full_durations)]

# محاسبه خطای پیش‌بینی (MSE) برای مدل TACD
mse_tacd <- mean((test_durations - predictions_test_tacd)^2)
cat(sprintf("\nمیانگین مربعات خطای پیش‌بینی (MSE) برای مدل TACD: %.6f\n", mse_tacd))


# --- 6. مقایسه نهایی ---
cat("\n--- مقایسه نهایی عملکرد پیش‌بینی ---\n")
cat(sprintf("MSE برای مدل WACD: %.6f\n", mse_wacd))
cat(sprintf("MSE برای مدل TACD: %.6f\n", mse_tacd))

if (mse_tacd < mse_wacd) {
  cat("نتیجه: مدل TACD عملکرد پیش‌بینی بهتری دارد.\n")
} else if (mse_wacd < mse_tacd) {
  cat("نتیجه: مدل WACD عملکرد پیش‌بینی بهتری دارد.\n")
} else {
  cat("نتیجه: هر دو مدل عملکرد پیش‌بینی یکسانی دارند.\n")
}
#############################################################################
#5-7


library(dplyr)

set.seed(777)
n_price_changes <- 500

last_size <- sample(1:3, n_price_changes, replace=TRUE)
last_direction <- sample(c(-1, 1), n_price_changes, replace=TRUE)

log_duration <- 2 + 0.3 * last_size + rnorm(n_price_changes, 0, 0.5)
duration <- exp(log_duration)

lambda_N <- exp(-0.5 + 0.5 * log_duration)
num_no_change <- rpois(n_price_changes, lambda_N)

prob_up <- 1 / (1 + exp(-(0 - 2.0 * last_direction)))
direction <- ifelse(rbinom(n_price_changes, 1, prob_up) == 1, 1, -1)

lambda_S <- exp(0.2 - 0.1 * num_no_change)
size <- rpois(n_price_changes, lambda_S) + 1 

pcd_data <- data.frame(duration, num_no_change, direction, size, last_size, last_direction)

model_duration <- lm(log(duration) ~ last_size, data = pcd_data)
print("--- مدل برای لگاریتم دیرش ---")
summary(model_duration)

model_num_no_change <- glm(num_no_change ~ log(duration), data = pcd_data, family = "poisson")
print("--- مدل برای تعداد معاملات بدون تغییر ---")
summary(model_num_no_change)

pcd_data$direction_binary <- ifelse(pcd_data$direction == 1, 1, 0)
model_direction <- glm(direction_binary ~ last_direction, data = pcd_data, family = "binomial")
print("--- مدل برای جهت تغییر ---")
summary(model_direction)

model_size <- glm(I(size-1) ~ num_no_change, data = pcd_data, family = "poisson")
print("--- مدل برای اندازه تغییر ---")
summary(model_size)

#-------------------------------------------------------------------------------

library(dplyr)
library(lubridate)
library(hms)

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

tick_size <- 0.125

pcd_data <- ibm_clean %>%
  filter(time >= hms(hours=9, minutes=30) & time <= hms(hours=16)) %>%
  arrange(datetime) %>%
  mutate(price_change = transaction_price - lag(transaction_price)) %>%
  filter(!is.na(price_change)) %>%
  mutate(price_change_ticks = price_change / tick_size) %>%
  filter(price_change_ticks != 0) %>%
  
  group_by(date) %>%
  mutate(
    duration_pc = as.numeric(datetime - lag(datetime), units = "secs"),
    direction_pc = sign(price_change_ticks),
    size_pc = round(abs(price_change_ticks))
  ) %>%
  ungroup() %>%
  filter(!is.na(duration_pc))

pcd_data_filtered <- pcd_data %>%
  filter(duration_pc > 0)
pcd_model_data <- pcd_data %>%
  filter(duration_pc > 0) %>% 
  mutate(
    lag_duration_pc = lag(duration_pc),
    lag_direction_pc = lag(direction_pc),
    lag_size_pc = lag(size_pc)
  ) %>%
  filter(!is.na(lag_duration_pc) & !is.na(lag_direction_pc) & !is.na(lag_size_pc))


model_duration_pcd <- lm(log(duration_pc) ~ lag_size_pc, data = pcd_model_data)
print("--- نتایج مدل برای دیرش بین تغییرات قیمت ---")
summary(model_duration_pcd)

cat("\n\n")
pcd_model_data$direction_up <- ifelse(pcd_model_data$direction_pc > 0, 1, 0)
model_direction_pcd <- glm(direction_up ~ lag_direction_pc, 
                           data = pcd_model_data, 
                           family = binomial(link="logit"))
print("--- نتایج مدل برای جهت تغییر قیمت ---")

summary(model_direction_pcd)
