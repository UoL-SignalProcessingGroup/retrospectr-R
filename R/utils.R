check_sample_dim <- function(model, samples){
  d <- model$param_num()
  sample_d <- dim(samples)[3]
  check_result <- d == sample_d
  if (!check_result){
    stop(paste0("Sample dimensions do not match model dimensions! (" , d, " != ", sample_d, ")"))
  }
  return(check_result)
}

check_models <- function(model, new_model){
  old_names <- model$param_names()
  new_names <- new_model$param_names()
  check_result <- all(old_names == new_names)
  if (!check_result){
    stop(paste0("Model dimensions do not match!"))
  }
  return(check_result)
}

clean_param_name <- function(param_name){
  rmatch <- regexpr("\\.", param_name)
  if (rmatch[1] == -1){
    return(param_name)
  }
  tmp <- stringi::stri_split_fixed(param_name, ".", 2, simplify = TRUE)
  tmp[,2] <- stringr::str_replace(tmp[,2], "\\.", ",")
  return(paste0(tmp[,1], "[", tmp[,2], "]"))
}

clean_param_names <- function(param_names){
  return(sapply(param_names, clean_param_name))
}

extract_samples <- function(fit, model){
  samples <- fit$draws()[,,clean_param_names(model$param_names())]
  return(samples)
}

