prep_lineplot <- function(data, grouping_var, sig_niveau = 0.05){
  prep_list <- list()

  prep_list[["lineplot"]] <- prep_line(data, grouping_var = grouping_var, sig_niveau = sig_niveau)
  prep_list[["pointplot"]] <- prep_points(data, grouping_var = grouping_var, competence = "GL", sig_niveau = sig_niveau)

}
