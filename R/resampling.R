#' Resampling
#'
#' Resample posterior samples using log-importance weights
#' @param fit `CmdStanMCMC` fit object or array of posterior samples
#' @param log_weights Log-importance weights
#' @return Resampled posterior samples
#' @examples
#' resampled_samples <- resample(fit, log_weights)
#' @export
resample <- function(fit, log_weights){
  resampled_samples <- posterior::resample_draws(fit$draws(), weights = exp(log_weights))
  return(resampled_samples)
}
