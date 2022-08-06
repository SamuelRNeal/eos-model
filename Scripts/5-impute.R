# Prepare data for model development
# Multiple imputation of missing data

# Load data --------------------
# Only necessary if not running after "4-prepare2.R"
# But no harm in reassigning "dat" object here
dat <- readRDS("Data/prepare2_dat.rds")

# Define objects for imputation model --------------------

# Final set of candidate predictors
predictors <- c(
  "pi_gest",
  "et_bw",
  "oh_matfever",
  "oh_offliquor",
  "co_prom",
  "et_grunt",
  "et_rr",
  "et_hr",
  "et_temp",
  "oe_activity",
  "oe_nasalflare",
  "oe_retractions",
  "oe_grunt",
  "oe_wob"
)

# Excluded predictors
excluded <- c(
  "et_cyanosis",
  "et_seizures",
  "oe_fontanelle",
  "oe_colour",
  "oe_abdodist",
  "oe_omphalitis",
  "oe_abskin",
  "hx_vomit"
)

# Final candidate predictors with missing values (i.e., to impute)
impute_vars <- c(
  "et_temp",
  "et_bw",
  "oe_wob",
  "et_rr",
  "et_hr",
  "et_grunt",
  "oe_nasalflare",
  "oe_retractions",
  "oe_grunt",
  "oh_matfever",
  "oh_offliquor"
)

# All variables (i.e., to inform the imputation model)
inform_vars <- c(
  predictors,
  excluded,
  "sepsis",
  "time",
  "pi_sex",
  "pi_age",
  "outcome"
)

ivars_df <- dat %>% select(all_of(inform_vars))

# Set MICE parameters --------------------

# Dry run to get defaults
set.seed(123)
dry_imp <- mice(
  ivars_df,
  m = 1,
  maxit = 0,
  defaultMethod = c("pmm", "logreg", "polyreg", "polr")
)

# Get predictor matrix
pm <- dry_imp$predictorMatrix
pm

# Get methods
method <- dry_imp$method
method

# Impute --------------------
# m = 40, iterations = 20
set.seed(123)
imp <- mice(
  ivars_df,
  m = 40,
  maxit = 20,
  predictorMatrix = pm,
  method = method
)

saveRDS(imp, "Data/imputed_dfs.rds")

# Tidy up global environment --------------------
rm(list = setdiff(ls(), c("imp", "predictors", "dat")))
