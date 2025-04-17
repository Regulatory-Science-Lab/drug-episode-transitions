# Fit model 
crwei <- flexsurvreg(Surv(Tstart, Tstop, status) ~ trans + shape(trans), data = transitions_all, dist = "weibull")

# Number of transitions in your data
transitions <- sort(unique(transitions_all$trans))  # e.g., 1 to 6
n_trans <- length(transitions)


# Get baseline estimates (for trans = 1)
base_shape  <- crwei$res["shape","est"]
base_scale  <- crwei$res["scale","est"]

# Try to get coefficients for additional transitions (trans = 2, 3, ..., n) ########## NOT WORKING ##########################
trans_coef  <- crwei$res[grep("^trans", rownames(crwei$res)), "est"]
shape_coef  <- crwei$res[grep("^shape\\(trans\\)", rownames(crwei$res)), "est"]

# Combine into a data frame
params <- tibble(
  trans = 1:n_trans,
  logscale_offset = c(0, trans_coef),         # 0 offset for baseline
  logshape_offset = c(0, shape_coef),         # 0 offset for baseline
  scale = exp(log(base_scale) + logscale_offset),
  shape = exp(log(base_shape) + logshape_offset)
)

params


fit_weibull_by_state <- function(transitions_all) {
  unique_states <- unique(state_data$state)
  
  results <- lapply(unique_states, function(st) {
    df_state <- transitions_all %>% filter(trans == st)
    
    # Fit Weibull model for time spent in this state
    # But this is not a competing risks model
    fit <- tryCatch({
      flexsurvreg(Surv(start_time, end_time, event) ~ 1,
                  data = df_state, dist = "weibull")
    }, error = function(e) NULL)
    
    if (!is.null(fit)) {
      tibble(
        state = st,
        shape = fit$res["shape", "est"],
        scale = fit$res["scale", "est"]
      )
    } else {
      tibble(state = st, shape = NA_real_, scale = NA_real_)
    }
  })
  
  bind_rows(results)
}

