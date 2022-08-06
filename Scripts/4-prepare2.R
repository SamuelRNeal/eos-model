# Prepare data for model development
# Creating/extracting relevant variables
# Applying inclusion/exclusion criteria

# Load data --------------------
# Only necessary if not running after "3-link.R"
# But no harm in reassigning "dat" object here
dat <- readRDS("Data/linked_df.rds")

# Prepare admission variables --------------------

### Emergency triage & vital signs ###

# Define sessions where et_bw takes value of admission weight
take_aw <- c("\\s98096$", "\\s10931$", "\\s43069$", "\\s28066$")

dat <- dat %>%
  mutate(
    # Categorical variables:
    et_grunt = factor(
      case_when(
        is.na(Admission.DangerSigns) ~ NA_character_,
        grepl("Grun", Admission.DangerSigns) ~ "yes",
        TRUE ~ "no"
      )
    ),
    et_cyanosis = factor(
      case_when(
        is.na(Admission.DangerSigns) ~ NA_character_,
        grepl("Cyan", Admission.DangerSigns) ~ "yes",
        TRUE ~ "no"
      )
    ),
    et_seizures = factor(
      case_when(
        is.na(Admission.DangerSigns) ~ NA_character_,
        grepl("Conv", Admission.DangerSigns) ~ "yes",
        TRUE ~ "no"
      )
    ),
    # Continuous variables:
    et_rr = as.numeric(Admission.RR),
    et_rr = if_else(
      (et_rr < 10 & Discharge.NeoTreeOutcome == "DC"),
      NA_real_,
      et_rr
    ),
    et_rr = if_else(
      et_rr > quantile(et_rr, .995, na.rm = T),
      NA_real_,
      et_rr
    ),
    et_hr = as.numeric(Admission.HR),
    et_hr = if_else(
      (et_hr < 20 & Discharge.NeoTreeOutcome == "DC"),
      NA_real_,
      et_hr
    ),
    et_temp = as.numeric(Admission.Temperature),
    et_bw = as.numeric(
      case_when(
        grepl(paste(take_aw, collapse = "|"), Admission.session) ~ Admission.AW,
        TRUE ~ Admission.BW
      )
    )
  )

### Patient information

dat <- dat %>%
  mutate(
    pi_admreason = factor(Admission.AdmReason),
    adm_datetime = ymd_hms(Admission.DateTimeAdmission, tz = "Africa/Harare"),
    pi_sex = factor(
      case_when(
        is.na(Admission.Gender) ~ NA_character_,
        Admission.Gender == "M" ~ "m",
        Admission.Gender == "F" ~ "f",
        Admission.Gender == "U" ~ "u"
      )
    ),
    pi_type = factor(Admission.TypeBirth),
    pi_type = fct_recode(
      pi_type,
      singleton = "S",
      twin1 = "Tw1",
      twin2 = "Tw2",
      triplet1 = "Tr1",
      triplet2 = "Tr2",
      triplet3 = "Tr3"
    ),
    pi_type = fct_relevel(
      pi_type,
      "singleton",
      "twin1",
      "twin2",
      "triplet1",
      "triplet2",
      "triplet3"
    ),
    pi_gest = as.numeric(Admission.Gestation)
  ) %>%
  rename(
    adm_uid = "Admission.UID",
    adm_session = "Admission.session"
  ) %>%
  # Age:
  # Remove invalid values
  mutate(
    ageb = if_else(
      grepl("month|-", Admission.AgeB),
      NA_character_,
      Admission.AgeB
    )
  ) %>%
  # Create new variable of age in hours
  mutate(
    aged = as.numeric(str_extract(ageb, "\\d+\\b(?=\\sday)")),
    ageh = as.numeric(str_extract(ageb, "\\d+\\b(?=\\shour)"))
  ) %>%
  mutate(
    aged = case_when(
      !is.na(aged) ~ aged,
      is.na(ageb) ~ NA_real_,
      TRUE ~ 0
    ),
    ageh = case_when(
      !is.na(ageh) ~ ageh,
      is.na(ageb) ~ NA_real_,
      TRUE ~ 0
    )
  ) %>%
  mutate(age_hours = aged * 24 + ageh) %>%
  select(-c(ageb, aged, ageh)) %>%
  # Create new age category variable
  mutate(
    agecat = Admission.AgeCat, # category selected by healthcare workers
    agecat_new = cut(
      age_hours,
      breaks = c(0, 2, 24, 48, 72, Inf),
      right = FALSE, # open on the right
      labels = c("FNB", "NB24", "NB48", "INF72", "INF"),
      ordered_result = FALSE
    ) # category based on age_hours
  ) %>%
  mutate(
    pi_age = if_else(
      is.na(agecat_new),
      agecat,
      as.character(agecat_new)
    )
  ) %>%
  mutate(
    pi_age = if_else(
      is.na(pi_age) & Admission.AgeA == "N",
      "INF",
      pi_age
    )
  ) %>%
  mutate(
    pi_age = as.factor(pi_age),
    pi_age = fct_relevel(
      pi_age,
      "FNB",
      "NB24",
      "NB48",
      "INF72",
      "INF"
    ),
    pi_age = fct_recode(
      pi_age,
      fnb = "FNB",
      dol1 = "NB24",
      dol2 = "NB48",
      dol3 = "INF72",
      older = "INF"
    )
  )

### Examination ###

dat <- dat %>%
  mutate(
    oe_fontanelle = factor(Admission.Fontanelle),
    oe_fontanelle = fct_recode(
      oe_fontanelle,
      bulging = "Bulg",
      flat = "Flat",
      sunken = "Sunk"
    ),
    oe_fontanelle = fct_relevel(
      oe_fontanelle,
      "flat",
      "sunken",
      "bulging"
    ),
    oe_activity = factor(Admission.Activity),
    oe_activity = fct_recode(
      oe_activity,
      alert = "Alert",
      coma = "Coma",
      seizures = "Conv",
      irritable = "Irrit",
      lethargic = "Leth"
    ),
    oe_activity = fct_relevel(
      oe_activity,
      "alert",
      "lethargic",
      "irritable",
      "seizures",
      "coma"
    ),
    oe_nasalflare = factor(
      case_when(
        is.na(Admission.SignsRD) ~ NA_character_,
        grepl("NFL", Admission.SignsRD) ~ "yes",
        TRUE ~ "no"
      )
    ),
    oe_retractions = factor(
      case_when(
        is.na(Admission.SignsRD) ~ NA_character_,
        grepl("CHI", Admission.SignsRD) ~ "yes",
        TRUE ~ "no"
      )
    ),
    oe_grunt = factor(
      case_when(
        is.na(Admission.SignsRD) ~ NA_character_,
        grepl("GR", Admission.SignsRD) ~ "yes",
        TRUE ~ "no"
      )
    ),
    oe_wob = factor(
      case_when(
        is.na(Admission.SignsRD) ~ NA_character_,
        Admission.SignsRD == "None" ~ "normal",
        TRUE ~ Admission.WOB
      )
    ),
    oe_wob = fct_recode(
      oe_wob,
      mild = "Mild",
      moderate = "Mod",
      severe = "Sev"
    ),
    oe_wob = fct_relevel(
      oe_wob,
      "normal",
      "mild",
      "moderate",
      "severe"
    ),
    oe_colour = factor(Admission.Colour),
    oe_colour = fct_recode(
      oe_colour,
      blue = "Blue",
      pink = "Pink",
      pale = "White",
      yellow = "Yell"
    ),
    oe_colour = fct_relevel(
      oe_colour,
      "pink",
      "pale",
      "blue",
      "yellow"
    ),
    oe_abdodist = factor(
      case_when(
        is.na(Admission.Abdomen) ~ NA_character_,
        grepl("Dist", Admission.Abdomen) ~ "yes",
        TRUE ~ "no"
      )
    ),
    oe_omphalitis = factor(
      case_when(
        is.na(Admission.Umbilicus) ~ NA_character_,
        grepl("Inf", Admission.Umbilicus) ~ "yes",
        TRUE ~ "no"
      )
    ),
    oe_abskin = factor(
      case_when(
        is.na(Admission.Skin) ~ NA_character_,
        grepl("None", Admission.Skin) ~ "no",
        TRUE ~ "yes"
      )
    ),
  )

### Symptom review ###

dat <- dat %>%
  mutate(
    hx_vomit = factor(
      case_when(
        is.na(Admission.Vomiting) ~ NA_character_,
        (grepl("Yes", Admission.Vomiting) &
          !grepl("YesBl|YesGr", Admission.Vomiting)) ~ "yellow",
        grepl("YesGr", Admission.Vomiting) ~ "bilious",
        grepl("YesBl", Admission.Vomiting) ~ "bloody",
        TRUE ~ "no"
      )
    ),
    hx_vomit = fct_relevel(
      hx_vomit,
      "no",
      "yellow",
      "bilious",
      "bloody"
    )
  )

### Maternal History ###

dat <- dat %>%
  mutate(
    oh_prom2 = factor(
      case_when(
        is.na(Admission.ROMlength) ~ NA_character_,
        Admission.ROMlength == "PROM" ~ "yes",
        TRUE ~ "no"
      )
    ),
    oh_prom = factor(
      case_when(
        is.na(Admission.RFSepsis) ~ NA_character_,
        grepl("PROM", Admission.RFSepsis) ~ "yes",
        TRUE ~ "no"
      )
    ),
    oh_matfever = factor(
      case_when(
        is.na(Admission.RFSepsis) ~ NA_character_,
        grepl("MF", Admission.RFSepsis) ~ "yes",
        TRUE ~ "no"
      )
    ),
    oh_offliquor = factor(
      case_when(
        is.na(Admission.RFSepsis) ~ NA_character_,
        grepl("OL", Admission.RFSepsis) ~ "yes",
        TRUE ~ "no"
      )
    ),
    co_prom = factor(
      case_when(
        (is.na(oh_prom) & is.na(oh_prom2)) ~ NA_character_,
        (oh_prom == "yes" | oh_prom2 == "yes") ~ "yes",
        TRUE ~ "no"
      )
    ),
    oh_delivery = factor(
      case_when(
        is.na(Admission.ModeDelivery) ~ NA_character_,
        grepl("ECS", Admission.ModeDelivery) ~ "emergencyCS",
        grepl("ElCS", Admission.ModeDelivery) ~ "electiveCS",
        grepl("For", Admission.ModeDelivery) ~ "forceps",
        grepl("Vent", Admission.ModeDelivery) ~ "ventouse",
        grepl("SVD", Admission.ModeDelivery) ~ "svd",
      )
    ),
    oh_delivery = fct_relevel(
      oh_delivery,
      "svd",
      "electiveCS",
      "emergencyCS",
      "forceps",
      "ventouse"
    )
  )

# Prepare outcome variables --------------------

### Participant demographics ###

dat <- dat %>%
  rename(
    dis_uid = "Discharge.NeoTreeID",
    dis_session = "Discharge.session"
  ) %>%
  mutate(
    outcome = factor(
      case_when(
        is.na(Discharge.NeoTreeOutcome) ~ NA_character_,
        Discharge.NeoTreeOutcome == "NND" ~ "died",
        TRUE ~ "discharged"
      )
    ),
    outcome_datetime = case_when(
      outcome == "died" ~ ymd_hms(
        Discharge.DateTimeDeath,
        tz = "Africa/Harare"
      ),
      outcome == "discharged" ~ ymd_hms(
        Discharge.DateTimeDischarge,
        tz = "Africa/Harare"
      )
    ),
    adm_dur = difftime(
      outcome_datetime,
      adm_datetime,
      units = "days"
    ),
    adm_dur = as.numeric(adm_dur, units = "days"),
    time = difftime(
      adm_datetime,
      "2019-02-01",
      units = "days"
    ),
    time = as.numeric(time, units = "days")
  )

### Model outcome data ###

dat <- dat %>%
  mutate(
    diagnosis = factor(Discharge.DIAGDIS1),
    diagnosis_other = Discharge.DIAGDIS1OTH,
    diagnosis2 = factor(Discharge.OthProbs),
    diagnosis2_other = Discharge.OthProbsOth,
    cause_death = factor(Discharge.CauseDeath),
    cause_death_other = Discharge.CauseDeathOther,
    cause_death2 = factor(Discharge.ContCauseDeath),
    cause_death2_other = Discharge.ContribOth,
    sepsis = factor(
      case_when(
        # 1. Discharge diagnosis of EONS:
        diagnosis == "EONS" ~ "yes",
        # 2. Dther discharge problem includes EONS:
        grepl("EONS", diagnosis2) ~ "yes",
        grepl("Early Onset Neonatal Sepsis", diagnosis2_other) ~ "yes",
        # 3. Cause of death of EONS:
        cause_death == "EONS" ~ "yes",
        grepl(
          "Early onset neonatal sepsis|earlyonset neonatal sepsis",
          cause_death_other
        ) ~ "yes", # identified from free text
        # 4. Contributory cause of death includes EONS:
        grepl("EONS", cause_death2) ~ "yes",
        # Else, no diagnosis of EONS:
        TRUE ~ "no"
      )
    )
  )

# Apply inclusion/exclusion criteria --------------------

# Data frame of cases to exclude
exclude <- dat %>%
  filter(
    pi_age == "older" |
      pi_gest < 32 |
      et_bw < 1500 |
      et_rr == 0 |
      et_hr == 0 |
      pi_type == "twin2" |
      pi_type == "triplet2" |
      pi_type == "triplet3" |
      diagnosis == "CHD" |
      diagnosis == "G" |
      diagnosis == "OM" |
      grepl("CHD|G|OM", diagnosis2) |
      cause_death == "CA" |
      cause_death == "Gastro" |
      grepl(
        "CHD|G|OM",
        cause_death2
      ) |
      grepl(
        "meningo|trisomy 13/18|omphalo|mmc|complex heart|ancephalocele",
        dat$diagnosis_other,
        ignore.case = TRUE
      ) |
      grepl(
        "meningo|encephalo|spina bifida|acyanotic chd|fallot|mmc",
        dat$diagnosis2_other,
        ignore.case = TRUE
      ) |
      grepl(
        "omphalo|gastroschisis|neural tube defect|multiple abnormalities",
        dat$cause_death_other,
        ignore.case = TRUE
      ) |
      grepl(
        "congenital cardio resp disease",
        dat$cause_death2_other,
        ignore.case = TRUE
      ) |
      adm_dur < 0
  )

# Exclude these cases
dat <- dat %>%
  anti_join(exclude, by = "adm_session") %>%
  droplevels() # drop unused levels from factors

# Select only relevant variables for analysis --------------------
vars <- readLines("Scripts/analysis_vars.txt")
dat <- dat %>% select(all_of(vars))

saveRDS(dat, "Data/prepare2_dat.rds")

# Tidy up global environment --------------------
rm(list = setdiff(ls(), "dat"))
