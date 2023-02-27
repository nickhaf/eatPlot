uneq_groups <- read.csv2("p:/Methoden/99_Arbeitsordner/eatRep_problem/eatRep_output_3mzp_unequalGroups.csv", sep = ";")
uneq_groups$mh <- as.factor(uneq_groups$mh)

plot_dat <- prep_trend(dat = uneq_groups,
                       competence = "GL",
                       grouping_var = "mh",
                       x_years = list(c(2011, 2016)),
                       x_braces = list(c(2011, 2016))
                       )

plot_lineplot(plot_dat,
              line_sig = "sig_trend_no_comp")


## Own function for filtering the correct years
