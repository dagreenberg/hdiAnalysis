# Simulate some data from a lognormal distribution to use in tests (and maybe a
#  vignette).

set.seed(42)
mean_log <- 4
sd_log <- 0.5
n <- 8000

sim <- rlnorm(n,
              mean_log,
              sd_log)

h <- hist(sim,
          breaks = 50)

plot(h,
     main = "Histogram of simulated values",
     xlab = "Simulated value")

x <- seq(0,
         ceiling(max(sim)/10) * 10,
         by = 0.1)

lines(x,
      dlnorm(x,
             mean_log,
             sd_log) * n * diff(h$mids)[1],
      col = "red")

sim <- list(values = sim,
            mean_log = mean_log,
            sd_log = sd_log)

usethis::use_data(sim,
                  overwrite = TRUE)
