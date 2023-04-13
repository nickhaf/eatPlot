## code to prepare `adjusted_means` dataset goes here
adjusted_means <- as.data.frame(read.csv("Q:/BT2021/BT/60_Bericht/04_Mittelwerte/adjustierte Mittelwerte/03_Syntaxen/02_Kontrollrechnungen_NH/res_dat/adj_bt21.csv", sep = ";", dec = ","))[, -1]

for(i in c("est", "se")){
  adjusted_means[ , i] <- gsub(",", ".", adjusted_means[ , i])
adjusted_means[ , i] <- as.numeric(adjusted_means[, i])
}

set.seed(23)
rand_num <- sample(1:100, 16)
unique_states <- unique(adjusted_means$TR_BUNDESLAND)[!is.na(unique(adjusted_means$TR_BUNDESLAND))]

for (i in c("group", "TR_BUNDESLAND")) {
  for (j in length(unique_states):1) { #this way, so Sachsen-Anhalt is gsubed first
    state <- unique_states[j]
    adjusted_means[, i] <- gsub(state, paste0("Land-", rand_num[j]), adjusted_means[, i])
  }
}

adjusted_means$adjust <- factor(adjusted_means$adjust)

usethis::use_data(adjusted_means, overwrite = TRUE)
