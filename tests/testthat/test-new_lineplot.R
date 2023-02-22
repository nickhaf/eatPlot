uneq_groups <- read.csv2("p:/Methoden/99_Arbeitsordner/eatRep_problem/eatRep_output_3mzp_unequalGroups.csv", sep = ";")

plot_dat <- prep_trend(dat = uneq_groups, competence = "GL", grouping_var = "mh")
