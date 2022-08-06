# Prepare data for model development
# Record linkage

# Load data --------------------
# Only necessary if not running after "2-prepare1.R"
# But no harm in reassigning "adm/dis" objects here
adm <- readRDS("Data/prepare1_adm.rds")
dis <- readRDS("Data/prepare1_dis.rds")

# Create data frames with linkage variables --------------------
# Admission data
adm_link <- adm %>%
  select(
    uid = Admission.UID_alphanum,
    bw = Admission.BW,
        gest = Admission.Gestation,
         ofc = Admission.OFC,
         length = Admission.Length,
         mode = Admission.ModeDelivery,
         sex = Admission.Gender,
         session = Admission.session
         # keep session to uniquely identify each entry
    ) %>%
  mutate(
    first = substr(.$uid, 1, 3),
    last = substr(.$uid, nchar(.$uid)-2, nchar(.$uid)),
      uidsub = tolower(paste0(first, last))
    ) %>%
  select(-c(uid, first, last))

# Discharge data
dis_link <- dis %>%
  select(
    uid = Discharge.NeoTreeID_alphanum,
         bw = Discharge.BWTDis,
         gest = Discharge.GestBirth,
         ofc = Discharge.OFCDis,
         length = Discharge.LengthDis,
         mode = Discharge.Delivery,
         sex = Discharge.SexDis,
         session = Discharge.session
    ) %>%
  mutate(
    first = substr(.$uid, 1, 3),
    last = if_else(nchar(.$uid) < 6,
         substr(.$uid, 4, nchar(.$uid)),
          substr(.$uid, nchar(.$uid)-2, nchar(.$uid))),
         uidsub = tolower(paste0(first, last))
    ) %>%
  select(-c(uid, first, last))

# Perform linkage --------------------
set.seed(123)

matches_out <- fastLink(
  dfA = adm_link,
  dfB = dis_link,
  varnames = c("uidsub", "bw", "gest", "ofc", "length", "mode", "sex"),
  stringdist.match = "uidsub",
  stringdist.method = "jw",
  jw.weight = .10,
  partial.match = "uidsub",
  cut.a = 0.96,
  cut.p = 0.88,
  dedupe.matches = TRUE,
  cond.indep = TRUE,
  return.all = TRUE
)

# Get matches --------------------
matches_list <- vector("list")

# With zeta >0.98 (matches)
matches_list$low <- getMatches(
  dfA = adm_link,
  dfB = dis_link,
  fl.out = matches_out,
  threshold.match = 0.98,
  combine.dfs = FALSE
)

# With zeta >0.10 (matches + potential matches)
matches_list$high <- getMatches(
  dfA = adm_link,
  dfB = dis_link,
  fl.out = matches_out,
  threshold.match = 0.10,
  combine.dfs = FALSE
)

# Session IDs for potential matches
matches_list$potential_adm <-
  matches_list$high$dfA.match$session[!matches_list$high$dfA.match$session
    %in% matches_list$low$dfA.match$session]

matches_list$potential_dis <-
  matches_list$high$dfB.match$session[!matches_list$high$dfB.match$session
                                      %in% matches_list$low$dfB.match$session]

# Build into data frames --------------------
# Designated matches from fastLink
matches_list$matches <- tibble(
  Admission.session = matches_list$low$dfA.match$session,
  Discharge.session = matches_list$low$dfB.match$session
) %>%
  merge(adm, by = "Admission.session") %>%
  merge(dis, by = "Discharge.session")

# Potential matches from fastLink
matches_list$potentials <- tibble(
  Admission.session = matches_list$potential_adm,
  Discharge.session = matches_list$potential_dis
) %>%
  merge(adm, by = "Admission.session") %>%
  merge(dis, by = "Discharge.session")

# Exclude non-matches from potentials (based on manual review)
matches_list$potentials <- matches_list$potentials %>%
  filter(!Admission.session %in% readLines("Scripts/not_potentials.txt"))

# Final set of matches
dat <- rbind(matches_list$matches, matches_list$potentials) %>%
  # Remove 4 anomalous cases noted during quality checks
  filter(
    Admission.session != "session 70565",
    Admission.session != "session 21083",
    Admission.session != "session 10707"
  ) %>%
  as_tibble()

saveRDS(dat, "Data/linked_df.rds")

# Tidy up global environment --------------------
rm(list=setdiff(ls(), "dat"))
