## code to prepare `min_stand` dataset goes here

min_stand <- readxl::read_xlsx("Q:/BT2022/BT/60_Bericht/_Probegrafiken/2023-01-26 Vorlagen Balken und Linien/BT2021_Abb3.9.xlsx", sheet = "Daten BT21")

min_stand <- as.data.frame(min_stand)
set.seed(23)
rand_num <- sample(1:100, 16)
unique_states <- unique(min_stand$TR_BUNDESLAND)[!is.na(unique(min_stand$TR_BUNDESLAND))]

for (i in c("group", "TR_BUNDESLAND", "keyword")) {
  for (j in length(unique_states):1) { #this way, so Sachsen-Anhalt is gsubed first
    state <- unique_states[j]
    min_stand[, i] <- gsub(state, paste0("Land-", rand_num[j]), min_stand[, i])
  }
}


usethis::use_data(min_stand, overwrite = TRUE)
