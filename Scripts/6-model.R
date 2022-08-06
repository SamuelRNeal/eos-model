# Optimal prediction model
# Run prediction model on the data

# Load data --------------------
# Only necessary if not running after "5-impute.R"
# But no harm in reassigning "imp" object here
imp <- readRDS("Data/imputed_dfs.rds")
imp <- complete(imp, action = "long", include = T) %>%
  mutate(
    et_bw = et_bw / 1000,
    et_rr = et_rr / 5,
    et_hr = et_hr / 5,
    oe_activity = fct_collapse(
      oe_activity,
      alert = "alert",
      lethargic = "lethargic",
      other = c("irritable", "seizures", "coma")
    )
  ) %>%
  as.mids()


# Get model --------------------
# a. Develop in a single imputed dataset
set.seed(37)
rand <- floor(runif(1, min = 1, max = 30))
si <- as_tibble(complete(imp, rand))

model <- glm(sepsis ~ et_temp + et_rr + oh_matfever + oh_offliquor + co_prom +
  oe_activity + oe_retractions + oe_grunt,
data = si,
family = "binomial"
)

# b. Estimate regression coefficients across all imputed datasets
imp_fit <- with(
  data = imp,
  exp = glm(
    sepsis ~ et_temp + et_rr + oh_matfever + oh_offliquor +
      co_prom + oe_activity + oe_retractions + oe_grunt,
    family = "binomial"
  )
)

# c. Tidy output
pooled_summary <- summary(pool(imp_fit))

pooled_OR <- exp(cbind(
  "OR" = pooled_summary[, 2],
  "lcl" = (pooled_summary[, 2] - 1.96 * (pooled_summary[, 3])),
  "ucl" = (pooled_summary[, 2] + 1.96 * (pooled_summary[, 3]))
)) %>%
  as.data.frame()

pooled_output <- cbind(
  "predictor" = as.character(pooled_summary[, 1]),
  "beta" = pooled_summary[, 2],
  "SE" = pooled_summary[, 3],
  pooled_OR,
  "p.value" = pooled_summary[, 6]
)

# Round values
pooled_output %>%
  mutate(
    beta = round(beta, 3), SE = round(SE, 3), OR = round(OR, 2),
    lcl = round(lcl, 2), ucl = round(ucl, 2), p.value = round(p.value, 4)
  )

