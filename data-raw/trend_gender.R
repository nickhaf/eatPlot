## Skript comes from here: "I:\Methoden\02_IQB-interne_eat_Workshops\eatRep_2024\03_Kap6_Geschlechterdisparitaeten.r"

path_lv09 <- "q:/BT2022/GADS/LV09_gads_v03.db"
path_bt15 <- "q:/BT2022/GADS/BT15_gads_v03.db"
path_bt22 <- "q:/BT2022/GADS/BT22_trend_gads_v04.db"
LEs <- readRDS("q:/BT2022/BT/40_Daten/40_GADS/LEs_v01.RDS")


# Paket laden
library(eatGADS)
library(eatRep)
library(eatTools)


### 4. Beispielanalysen fuer Kapitel 6, "Geschlechtsbezogene Disparitaeten"
###########################################################################
meta22 <- extractMeta(path_bt22)
gendervars <- unique(grep("sex|gender", meta22[, 1], value = TRUE, ignore.case = TRUE))
bt_vars <- c("IDSTUD", "TR_BUNDESLAND", "TR_SCHULFORM", "TR_ZIELGLEICH", "jkrep", "jkzone", "totwgt", "TR_SEX", "Kgender", "Kgender2", "Kgender_imp", "Kgender2_imp", "imp", "kb", "stufe", "bista")
kap6Dat <- getTrendGADS(
  filePaths = c(path_lv09, path_bt15, path_bt22),
  vSelect = bt_vars,
  years = c(2009, 2015, 2022), fast = TRUE
)
datK6Trend <- extractData2(kap6Dat,
                           labels2character = c("TR_BUNDESLAND", "kb", "TR_SCHULFORM", "Kgender"))


trend_gender <- lapply(c("lesen", "hoeren"), function(my_kb) {
  datK6.Trend.auswahl <- subset(datK6Trend, kb == my_kb)

  ### missings auf gender entfernen ... sofern unumputierte gendervariable genommen werden soll
  table(datK6.Trend.auswahl[, c("year", "Kgender")], useNA = "if")
  datK6.Trend.auswahl <- na_omit_selection(datK6.Trend.auswahl, varsToOmitIfNA = c("jkzone", "Kgender"))

  ### Geschlecht als group difference, country als cross-level Difference
  disp <- repMean(
    datL = datK6.Trend.auswahl, ID = "IDSTUD", wgt = "totwgt", type = "jk2", PSU = "jkzone", repInd = "jkrep", imp = "imp", groups = c("TR_BUNDESLAND", "Kgender"),
    group.splits = 0:2, group.differences.by = "Kgender", cross.differences = TRUE, crossDiffSE = "old", dependent = "bista", engine = "BIFIEsurvey", trend = "year", linkErr = subset(LEs, domain == my_kb)
  )
  resDisp <- report2(disp, add = list(kb = my_kb))
  return(resDisp)
})

usethis::use_data(trend_gender, overwrite = TRUE)
