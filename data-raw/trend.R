trend_books <- read.csv2("Q:/BT2021/BT/60_Bericht/06_Soziale_Disparitäten/03_Syntaxen/KAS/Abb65Buecher_mitTrend.csv",
  na.strings = ""
)
trend_books$KBuecher_imp3 <- factor(trend_books$KBuecher_imp3, levels = c("1", "0", "0.vs.1"))

set.seed(23)
rand_num <- sample(1:100, 16)
unique_states <- unique(trend_books$TR_BUNDESLAND)[!is.na(unique(trend_books$TR_BUNDESLAND))]

for (i in c("group", "TR_BUNDESLAND")) {
  for (j in length(unique_states):1) { # this way, so Sachsen-Anhalt is gsubed first
    state <- unique_states[j]
    trend_books[, i] <- gsub(state, paste0("Land-", rand_num[j]), trend_books[, i])
  }
}


usethis::use_data(trend_books, overwrite = TRUE)
