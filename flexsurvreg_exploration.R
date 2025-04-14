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


