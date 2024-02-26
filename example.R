library(cmdstanr)
library(retrospectr)

file <- file.path(cmdstan_path(), "examples", "bernoulli", "bernoulli.stan")
mod <- cmdstan_model(file)

old_data <- list(N = 10, y = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 1))
new_data <- list(N= 20, y = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 1))

fit <- mod$sample(
  data = old_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 2000,
  refresh = 500 # print update every 500 iters
)

fit2 <- mod$sample(
  data = new_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 2000,
  refresh = 500 # print update every 500 iters
)

log_weights <- calculate_log_weights(model=file, fit, old_data, new_data)
samples <- fit$draws()
new_samples <- fit2$draws()
resampled_samples <- resample(fit, log_weights)
resampled_samples

posterior_1 <- posterior::as_draws_df(samples) |> dplyr::mutate(model = "Original")
posterior_2 <- posterior::as_draws_df(resampled_samples) |> dplyr::mutate(model = "Resampled")
posterior_3 <- posterior::as_draws_df(new_samples) |> dplyr::mutate(model = "New")
posteriors <- rbind(posterior_1, posterior_2, posterior_3)


g <- posteriors |> ggplot2::ggplot(ggplot2::aes(x = theta, colour = model)) +
  ggplot2::geom_density() +
  ggplot2::theme_bw() +
  ggplot2::ylab("Density")
g
ggplot2::ggsave("example.png", plot = g)
