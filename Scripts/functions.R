# Custom functions

# Read in JSON files exported from Neotree app --------------------
importNeoTree <- function(paths, type) {
  print("Reading files...")
  files <- vector("list", length(paths))
  corrupt <- vector("list", 0)
  pb <- progress::progress_bar$new(total = length(paths))
  for (i in 1:length(paths)) {
    f <- paths[i]
    # Check for and ignore corrupt files
    a <- readChar(f, nchars = file.info(f)$size)
    if (str_sub(a, start = -1) == "}") {
      pb$tick()
      files[[i]] <- do.call(rbind, fromJSON(file = f))
    } else {
      pb$tick()
      corrupt <- c(corrupt, f)
    }
  }

  print("Combining files...")
  data <- files[[1]]
  pb <- progress::progress_bar$new(total = length(paths) - 1)
  if (length(paths) > 1) {
    for (i in 2:length(paths)) {
      pb$tick()
      data <- cbind(data, files[[i]])
    }
  }

  # Extract entries for each "session"
  # One "session" = one admission or outcome form
  print("Extracting entries...")
  entries <- map(data, "entries")
  rm(data) # conserve memory
  pb <- progress::progress_bar$new(total = length(entries) * 2)

  session_id <- paste("session", seq_along(entries))
  keys <- entries %>%
    map(function(x) {
      pb$tick()
      map(x, function(y) y$key)
    })
  values <- entries %>%
    map(function(x) {
      pb$tick()
      map(x, function(y) {
        paste(unlist(map(y$values, function(z) unlist(z$value))),
          collapse = ","
        )
      })
    })

  # Combine into list with one matrix per session
  print("Combining entries...")
  pb <- progress::progress_bar$new(total = length(entries))
  session_list <- seq_along(entries) %>%
    map(function(x) {
      pb$tick()
      cbind(
        session = session_id[x],
        key = unlist(keys[[x]]),
        value = unlist(values[[x]])
      )
    })
  rm(session_id, keys, values) # conserve memory
  # Convert to a single "long" data frame
  data <- do.call(rbind, session_list) %>% as_tibble()

  # Remove duplicated keys in a session
  # Sometimes duplicated key-value pairs
  # Sometimes duplicated key with one real value and one empty string
  # To date, no instances with duplicated keys but unique non-empty values
  print("Removing duplicate keys...")
  if (data %>% select(session, key) %>% duplicated() %>% sum() > 0) {
    data <- data %>%
      group_by(session, key) %>%
      slice(if (all(value == "")) 1 else min(which(value != ""))) %>%
      ungroup()
  }

  print("Finalising...")
  data <- data %>% pivot_wider(names_from = key, values_from = value)

  # Add script type to column names
  data <- data %>% rename_with(~ paste(type, ., sep = "."))

  # Print corrupt file names to console
  if (length(corrupt > 0)) {
    cat(
      "These files could not be processed:\n",
      unlist(corrupt),
      sep = "\n"
    )
  }

  return(data)
}

# Recode strings signifying missingness to NA --------------------
# Where x is a vector, not a data frame
# i.e. designed to work inside dplyr::mutate
recodeMissing <- function(x) {
  strings <- c("", "na", "n/a", "N/A", "NA", "Nil", "nil", "-")
  x[x %in% strings] <- NA
  x
}

# Yates' discrimination slope bootstrap --------------------
yatesBootstrap <- function(model, data, indices) {
  d <- data[indices, ]
  m <- glm(
    model$formula,
    data = d,
    family = "binomial"
  )
  mean(plogis(m$linear.predictors[d$sepsis == "yes"])) - mean(plogis(m$linear.predictors[d$sepsis == "no"]))
}

# Performance bootstrap ---------------------
likelihoodBootstrap <- function(data, indices, model, return = c("PLR", "NLR")) {
  d <- data[indices, ]
  p <- predict(model, newdata = d, type = "response")
  
  roc <- pROC::roc(
    sepsis ~ p,
    data = d
  )
  
  # Get thresholds and corresponding sensitivities from ROC curve
  thres <- coords(
    roc,
    x = "all",
    ret = c("threshold", "sens"),
    transpose = FALSE,
    as.matrix = FALSE
  )
  
  # Get 'optimal' threshold (Youden's index)
  thres_best <- coords(
    roc,
    x = "best",
    ret = c("threshold"),
    best.method = "youden",
    transpose = FALSE,
    as.matrix = FALSE
  )
  
  # For some resamples, multiple thresholds may maximise distance to ROC identity line
  # If so, randomly select one value
   if (nrow(thres_best) > 1){
    thres_best <- thres_best[sample(nrow(thres_best), 1), ]
   
  }
  
  # Define selected sensitivities
  sens_list <- c(0.8, 0.85, 0.9, 0.95)

  selected_thres <- tibble(
    sens = sens_list,
    thres = rep(NA_real_, length(sens_list))
  )
  for (i in 1:length(sens_list)) {
      x <- thres[which.min(abs(thres$sensitivity - sens_list[i])), 1]
      x <- max(x) # take largest value if multiple
      x <- sample(x, 1) # take only one if multiple identical maxima
    selected_thres[i, 2] <- x
  }
  selected_thres <- c(thres_best[[1]], selected_thres$thres)
  
  # Get xtabs
  xtabs_list <- list(NA)
  for (i in 1:length(selected_thres)) {
    obs <- factor(d$sepsis, levels = c("yes", "no"))
    pred <- factor(if_else(p > selected_thres[i], "yes", "no"), levels = c("yes", "no"))
    xtabs_list[[i]] <- table(pred, obs)
  }
  names(xtabs_list) <- c("best", sens_list)
  xtabs_list
  
  # Get performance from xtabs at each threshold
  performance <- tibble(
    sens = NA_real_,
    spec = NA_real_,
    PPV = NA_real_,
    NPV = NA_real_,
    PLR = NA_real_,
    NLR = NA_real_,
  )
  for (i in 1:length(xtabs_list)) {
    TP <- xtabs_list[[i]][1, 1]
    FP <- xtabs_list[[i]][1, 2]
    FN <- xtabs_list[[i]][2, 1]
    TN <- xtabs_list[[i]][2, 2]
    # Sens
    performance[i, "sens"] <- TP / (TP + FN)
    # Spec
    performance[i, "spec"] <- TN / (TN + FP)
    # PPV
    performance[i, "PPV"] <- TP / (TP + FP)
    # NPV
    performance[i, "NPV"] <- TN / (TN + FN)
    # PLR (sens / 1 - spec)
    performance[i, "PLR"] <- performance[i, "sens"] / (1 - performance[i, "spec"])
    # NLR (1 - sens / spec)
    performance[i, "NLR"] <- (1 - performance[i, "sens"]) / performance[i, "spec"]
  }
  
  # Tidy output
  performance <- performance %>%
    mutate(thres = selected_thres) %>%
    select(thres, everything()) %>%
    mutate(across(everything(), round, 3)) %>%
    mutate(across(-c(thres, PLR, NLR), ~.*100))
  
  likelihood <- list(
    PLR = performance$PLR,
    NLR = performance$NLR
    )

  if (return == "PLR") {
    likelihood$PLR
  } else { 
    likelihood$NLR
  }

}
