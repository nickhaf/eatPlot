test_that("subGroups are created correctly", {
  df <- data.frame()



  expect_equal(2 * 2, 4)
})


data <- as.data.frame(readxl::read_excel("Q:/BT2021/BT/60_Bericht/04_Mittelwerte/adjustierte Mittelwerte/Ergebnisse/01_ohneTrend_z_standard.xlsx"))

data_prep <- prepareBarplot(data = data, subGroups = "adjust", sigNiveau = 0.05)



