d1 <- readRDS(file = "P:/Methoden/02_R_Pakete/eatRep/Outputstruktur/report2_outputs/03_mig_verschachtelt.rds")
bt_example <- d1[[1]][[1]][[1]]

trend_3 <- lapply(bt_example, function(x){

  if(checkmate::test_names(colnames(x), must.include = "est")){
    x$est <- rnorm(n = nrow(x), mean = 500, sd = 100)
  }
  if(checkmate::test_names(colnames(x), must.include = "p")){
    x$p <- runif(n = nrow(x), min = 0, max = 0.1
    )
  }


  return(x)

})

trend_3$group$TR_BUNDESLAND[is.na(trend_3$group$TR_BUNDESLAND)] <- "none"

usethis::use_data(trend_3, overwrite = TRUE)
