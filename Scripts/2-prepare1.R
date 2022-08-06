# Prepare data for model development
# Preliminary data cleaning

# Load data --------------------
# Only necessary if not running after "1-import.R"
# But no harm in reassigning "adm/dis" objects here
adm <- readRDS("Data/import_adm.rds")
dis <- readRDS("Data/import_dis.rds")

# Admission data --------------------
adm <- adm %>%
  filter(!duplicated(.[, 2:ncol(.)])) %>%
  mutate(across(everything(), recodeMissing)) %>%
  # Remove entries with no healthcare worker ID (not "signed off")
  filter(!is.na(.$Admission.HCWID)) %>%
  # Strip non-alphanumeric characters from admission ID
  # And standardise ModeDelivery & Gender between adm & dis forms
  mutate(
    Admission.UID_alphanum = gsub("[^a-zA-Z0-9]", "", .$Admission.UID),
    Admission.ModeDelivery = recode(
      Admission.ModeDelivery,
      "1" = "SVD",
      "2" = "Vent",
      "3" = "For",
      "4" = "ElCS",
      "5" = "ECS",
      "6" = "SVD"
    ),
    Admission.Gender = recode(Admission.Gender, "NS" = "U")
  )

# Discharge data --------------------
dis <- dis %>%
  filter(!duplicated(.[, 2:ncol(.)])) %>%
  mutate(across(everything(), recodeMissing)) %>%
  # Remove entries with no healthcare worker ID
  filter(!is.na(.$Discharge.HCWIDDis)) %>%
  mutate(
    Discharge.NeoTreeID_alphanum =
      gsub("[^a-zA-Z0-9]", "", .$Discharge.NeoTreeID)
  ) %>%
  # Strip non-alphanumeric characters from discharge ID
  # And remove entries with an invalid discharge ID
  filter(
    !is.na(.$Discharge.NeoTreeID_alphanum),
    !grepl("^(0)\\1{0,}$", .$Discharge.NeoTreeID_alphanum),
    # "^(0)\\1{0,}$" finds strings of just zeros
    !nchar(.$Discharge.NeoTreeID_alphanum) < 4,
    !grepl("twin", .$Discharge.NeoTreeID_alphanum),
    !grepl("and", .$Discharge.NeoTreeID_alphanum)
  )

# Exclude pilot data period --------------------
# Pilot period = start of data collection in Nov '18 to 31 Jan '19
adm <- adm %>%
  filter(
    ymd_hms(
      Admission.DateTimeAdmission,
      tz = "Africa/Harare"
    ) >= "2019-02-01"
  )

dis <- dis %>%
  filter(
    ymd_hms(
      Discharge.DateTimeDischarge,
      tz = "Africa/Harare"
    ) >= "2019-02-01" |
      ymd_hms(
        Discharge.DateTimeDeath,
        tz = "Africa/Harare"
      ) >= "2019-02-01"
  )

saveRDS(adm, "Data/prepare1_adm.rds")
saveRDS(dis, "Data/prepare1_dis.rds")
