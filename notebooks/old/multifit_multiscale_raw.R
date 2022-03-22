multiscale_fits$model_comparison_df

# take variables and their combination
mod_comparison_df <- multifits$model_comparison_all
covs <- unique(mod_comparison_df$covariate)[c(1,2)]
corr_threshold <- 0.6

# check number of multivariate models to be run
n_per_cov <- purrr::map_dbl(covs, function(i)
  nrow(mod_comparison_df[mod_comparison_df$covariate == i,]))
n_models <- prod(n_per_cov)

cov_list <- purrr::map(covs, function(i)
  mod_comparison_df %>%
    dplyr::filter(covariate == i) %>%
    pull(multief) %>%
    as.character())
names(cov_list) <- covs

# list of parms
out_df <- expand.grid(cov_list) %>%
  tibble::as_tibble() %>%
  dplyr::mutate_all(as.character) %>%
  dplyr::mutate(rank = as.numeric(NA),
                AIC = as.numeric(NA),
                dAIC = as.numeric(NA))
# list of models
models <- list()
excluded_vars <- list()
warns <- list()
formulas <- list()

form2 <- update(form, ~ . - multief)

pb <- txtProgressBar(min = 0, max = nrow(out_df), style = 3)
for (i in 1:nrow(out_df)) {
  
  # get variables
  vars <- unname(unlist(out_df[i,1:length(covs)]))
  
  # keep uncorrelated variables
  exclude <- usdm::vifcor(dat_sc[vars], th = corr_threshold)@excluded
  
  # update variables
  vars <- vars[!(vars %in% exclude)]
  
  # update formula
  # add transformation to the parameters!!! e.g. scale
  ff <- as.character(form2)[c(2,1,3)]
  for(f in length(vars):1)
    ff[3] <- paste0(vars[f], " + ", ff[3])
  # update(ff, ~ . + scale(eval(parse(vars[[f]]))))
  ff <- formula(paste(ff, collapse = " "))
  
  # run model
  running <- function(expr) {
    warns <- mess <- NULL
    warn_handler <- function(w) {
      warns <<- c(warns, list(w))
      invokeRestart("muffleWarning")
    }
    mess_handler <- function(m) {
      mess <<- c(mess, list(m))
      NULL
    }
    val <- suppressMessages(tryCatch(withCallingHandlers(expr, warning = warn_handler, message = mess_handler), error = function(e) e))
    out <- list(value = val, warnings = warns, messages = mess)
    return(out)
  }
  
  model <- running(coxph(ff, data = dat_sc))
  
  # get warnings and all that
  models[[i]] <- model$value
  excluded_vars[[i]] <- exclude
  warns[[i]] <- model$warnings
  formulas[[i]] <- try(model$value$formula)
  # get fit details
  out_df$AIC[i] <- try(AIC(model$value))
  
  setTxtProgressBar(pb, i)
}

# errors in fitting  
which_error <- grep("Error", out_df$AIC)

if(length(which_error) > 0) {
  out_df <- out_df %>%
    dplyr::slice(-which_error)
}

# organize results
out_df <- out_df %>%
  tibble::rowid_to_column() %>%
  dplyr::arrange(AIC) %>%
  dplyr::mutate(rank = 1:nrow(.),
                dAIC = AIC - AIC[1],
                relLL = exp(-0.5 * dAIC),
                wAIC = relLL/sum(relLL))