#5-6
threshold_val <- median(adjusted_durations)
cat(sprintf("آستانه انتخاب شده برای دیرش: %.4f\n", threshold_val))

# تعریف تابع درستنمایی برای مدل TACD
log_likelihood_tacd <- function(params, data, threshold) {
  p1 <- params[1:4]; p2 <- params[5:8]
  n <- length(data); psi <- numeric(n); loglik <- 0
  psi[1] <- mean(data)
  for (i in 2:n) {
    if (data[i-1] <= threshold) {
      psi[i] <- p1[1] + p1[2] * data[i-1] + p1[3] * psi[i-1]
      alpha <- p1[4]
    } else {
      psi[i] <- p2[1] + p2[2] * data[i-1] + p2[3] * psi[i-1]
      alpha <- p2[4]
    }
    if (psi[i] <= 1e-9 || alpha <= 0.1) return(1e10)
    scale_factor <- 1 / gamma(1 + 1/alpha)
    log_density <- dweibull(data[i], shape = alpha, scale = psi[i] * scale_factor, log = TRUE)
    if(is.na(log_density) || is.infinite(log_density)) return(1e10)
    loglik <- loglik + log_density
  }
  return(-loglik)
}

#اجرای بهینه سازی
start_params_tacd <- c(0.1, 0.1, 0.8, 0.9, 0.2, 0.05, 0.9, 0.8)
lower_bounds_tacd <- rep(1e-6, 8)
upper_bounds_tacd <- c(Inf, 0.999, 0.999, Inf, Inf, 0.999, 0.999, Inf)

print("شروع فرآیند بهینه‌سازی مدل TACD... این فرآیند بسیار زمان‌بر است.")
start_time <- Sys.time()

fit_tacd_ibm <- suppressWarnings({
  optimx(start_params_tacd, log_likelihood_tacd, data = adjusted_durations, 
         threshold = threshold_val, method = "L-BFGS-B",
         lower = lower_bounds_tacd, upper = upper_bounds_tacd,
         control = list(maxit=1000))
})

end_time <- Sys.time()
print("بهینه‌سازی تمام شد!")
cat("زمان صرف شده:", format(end_time - start_time), "\n")

#ذخیره کردن نتیجه نهایی
saveRDS(fit_tacd_ibm, file = "fit_tacd_model.rds")
print("نتیجه مدل در فایل 'fit_tacd_model.rds' در پوشه پروژه شما ذخیره شد.")

