skip(test_that("Table-barplot is still the same", {
prep_table <- prep_tableplot(adjusted_means, columns = "adjust", competence = "GL", sig_niveau = 0.05)
p_table <- plot_table(prep_table)


prep_bar <- prep_barplot(adjusted_means, sub_groups = "adjust", sig_niveau = 0.05)
prep_bar <- prep_bar[prep_bar$kb == "GL", ]
p_bar <- plot_bar(prep_bar)

vdiffr::expect_doppelganger("Table bar plot", plot_table_bar(p_table, p_bar))
})
)
