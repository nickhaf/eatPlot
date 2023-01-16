prep_dat <- prep_barplot(adjusted_means, sub_groups = "adjust", sig_niveau = 0.05)
prep_dat <- prep_dat[prep_dat$kb == "GL",]


plot_bar(prep_dat)
