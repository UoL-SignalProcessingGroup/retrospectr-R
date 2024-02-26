#' Importance weight calculation
#'
#' Calculate log importance weights
#' @param model Path to .stan file containing model
#' @param fit `CmdStanMCMC` fit object or array of posterior samples
#' @param old_data Corresponding data list used to generate `fit`
#' @param new_data Data list for which to calculate importance weights
#' @return log-importance weights for the posterior samples
#' @examples
#' logweights <- calculate_log_weights(model, fit, old_data, new_data)
#' @export
calculate_log_weights <- function(model, fit, old_data, new_data){

  old_data_file <- tempfile(fileext = ".data.json")
  cmdstanr::write_stan_json(old_data, old_data_file)

  new_data_file <- tempfile(fileext = ".data.json")
  cmdstanr::write_stan_json(new_data, new_data_file)

  old_model <- bridgestan::StanModel$new(model, old_data_file, seed = 1)

  if (is(fit, "CmdStanMCMC")){
    samples <- extract_samples(fit, old_model)
  } else {
    samples <- fit
  }

  #Check samples match model+old_data
  check_sample_dim(old_model, samples)

  new_model <- bridgestan::StanModel$new(model, new_data_file, seed = 1)
  check_models(old_model, new_model)

  logProb_old <- evaluate_logProb(old_model, samples)
  logProb_new <- evaluate_logProb(new_model, samples)

  logWeights <- logProb_new - logProb_old
  logWeights <- logWeights - matrixStats::logSumExp(logWeights)

  return(logWeights)


}

evaluate_logProb <- function(model, samples){
  if (dim(samples)[3] > 1){
    unc_samples <- aperm(apply(samples, c(1,2), model$param_unconstrain), c(2,3,1))
  } else {
    unc_samples <- apply(samples, c(1,2), model$param_unconstrain)
  }
  logProb <- apply(unc_samples, c(1,2), model$log_density)
  return(logProb)
}
