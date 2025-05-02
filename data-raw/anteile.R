## Data after: "Q:\BT2022\BT\60_Bericht\09_Motivation\03_Berechnungen\Erstrechnung\Syntax_Erstrechnung.R"


#Pakete laden
library(eatGADS)
library(eatRep)
library(eatTools)
library(psych)
library(lavaan)
library(tidyverse)

#Pfade setzen
path15 <- "q:/BT2022/BT/40_Daten/40_GADS/BT15_gads_v03.db"
path22 <- "q:/BT2022/BT/40_Daten/40_GADS/BT22_gads_v06.db"

#Variablen sichten
nam22 <- namesGADS(path22)
nam22
nam15 <- namesGADS(path15)
nam15




#Datensatz einschraenken 2022 und Geschlecht umkodieren, sodass Jungen Referenzgruppe sind
kap9Dat<- getGADS_fast(vSelect = c("IDSTUD", "TR_BUNDESLAND", "jkrep", "jkzone", "totwgt", "imp", "kb", "Sfskde_kurz_imp", "Sfsken_kurz_imp", "Sind15_imp", "Sine15_imp", "Kgender", "stufe", "TR_ZIELGLEICH", "bista"),filePath = path22)
datK9  <- extractData(kap9Dat)

#Gruppierungsvariable (Kgender) darf keine missings enthalten
datK9  <- na_omit_selection(datK9, varsToOmitIfNA=c("Kgender"))
datK9.d <- datK9[which(datK9[,"kb"] == "lesen"),]
datK9.e <- datK9[which(datK9[,"kb"] == "read"),]


datK9.d[, "Sfskde_kurz_imp_kat"] <- ifelse(datK9.d[, "Sfskde_kurz_imp"] <=2, yes = "niedrig", no = ifelse(datK9.d[, "Sfskde_kurz_imp"] <3, yes = "mittel", no = "hoch"))
datK9.d[, "Sind15_imp_kat"] <- ifelse(datK9.d[, "Sind15_imp"] <=2, yes = "niedrig", no = ifelse(datK9.d[, "Sind15_imp"] <3, yes = "mittel", no = "hoch"))
datK9.e[, "Sfsken_kurz_imp_kat"] <- ifelse(datK9.e[, "Sfsken_kurz_imp"] <=2, yes = "niedrig", no = ifelse(datK9.e[, "Sfsken_kurz_imp"] <3, yes = "mittel", no = "hoch"))
datK9.e[, "Sine15_imp_kat"] <- ifelse(datK9.e[, "Sine15_imp"] <=2, yes = "niedrig", no = ifelse(datK9.e[, "Sine15_imp"] <3, yes = "mittel", no = "hoch"))

ant_Sskde22 <- repTable(datL = datK9.d, ID="IDSTUD", wgt="totwgt", type="jk2", PSU="jkzone", repInd = "jkrep", imp = "imp",
                        groups = c("TR_BUNDESLAND", "Kgender"), group.splits = 0:2, dependent = "Sfskde_kurz_imp_kat", group.differences.by = "Kgender")
anteile <- report2(ant_Sskde22, add = list ( var = "Selbstkonzept Deutsch"))


usethis::use_data(anteile, overwrite = TRUE)

