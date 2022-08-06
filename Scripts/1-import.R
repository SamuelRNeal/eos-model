# Read in JSON files exported from Neotree app

stem <- file.path("Data", "_Raw_json")
paths <- list(
  adm = file.path(stem, list.files(stem, "*NeoTree___Zimbabwe*.json")),
  dis = file.path(stem, list.files(stem, "*NeoDischarge___Zimbabwe*.json"))
)

adm <- importNeoTree(paths$adm, type = "Admission")
dis <- importNeoTree(paths$dis, type = "Discharge")

saveRDS(adm, "Data/import_adm.rds")
saveRDS(dis, "Data/import_dis.rds")
