
library(tidyverse)
library(grid)
library(cowplot)
library(gridExtra)
library(kableExtra)

# https://statisticsglobe.com/insert-png-image-between-ggplot2-axis-positions-in-r

# Daten laden -------------------------------------------------------------

bt21_NoTrend <- readxl::read_excel("Q:/BT2021/BT/60_Bericht/04_Mittelwerte/adjustierte Mittelwerte/Ergebnisse/01_ohneTrend_z_standard.xlsx")


bt21_NoTrend_prep <- bt21_NoTrend %>%
  mutate(significant = factor(ifelse(p < 0.05, "yes", "no"))) %>%
  mutate(adj_sig = case_when(
    adjust == "ohneAdj" & significant == "no" ~ "noAdj_noSig",
    adjust == "ohneAdj" & significant == "yes" ~ "noAdj_Sig",
    adjust == "mitAdj" & significant == "no" ~ "Adj_noSig",
    adjust == "mitAdj" & significant == "yes" ~ "Adj_Sig"
  )) %>%
  filter(kb == "GL" & comparison == "crossDiff")



t1 <- bt21_NoTrend %>%
  mutate(significant = factor(ifelse(p < 0.05, "yes", "no"))) %>%
  mutate(adj_sig = case_when(
    adjust == "ohneAdj" & significant == "no" ~ "noAdj_noSig",
    adjust == "ohneAdj" & significant == "yes" ~ "noAdj_Sig",
    adjust == "mitAdj" & significant == "no" ~ "Adj_noSig",
    adjust == "mitAdj" & significant == "yes" ~ "Adj_Sig"
  )) %>%
  filter(kb == "GL" & is.na(comparison) & group != "wholeGroup") %>%
  select(group, est, adjust) %>%
  as.data.frame %>%
  reshape(idvar = "group", timevar = "adjust" , direction = "wide", v.names = c("est"))

t1_long <- t1 %>%
  mutate_at(c("est.ohneAdj", "est.mitAdj"), round) %>%
  mutate_at(c("est.ohneAdj", "est.mitAdj"), as.character) %>%
  mutate(Bundesland = group) %>%
  pivot_longer(cols = c("est.ohneAdj", "est.mitAdj", "Bundesland"), names_to = "estimate" )



# Globales Setup ----------------------------------------------------------
## Plot plot
p1 <- plot_bar(bt21_NoTrend_prep) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

#ggplot_build(p1)$layout$panel_params[[1]]$y.labels

## convert to grobs
t_grob_1 <- tableGrob(t1, theme = t1_theme)

# Set widths/heights to 'fill whatever space I have'
t_grob_1$widths <- unit(rep(1, ncol(t_grob_1)), "null")
t_grob_1$heights <- unit(rep(1, nrow(t_grob_1)), "null")




# Col spanners ------------------------------------------------------------
colnames(t1) <- NULL

header_t1 <- tableGrob(t1[1, 1:3], rows=NULL, cols=c("head1", "head2", "head3"))
p_grob_1 <- ggplotGrob(p1)
t_grob_1 <- gtable_combine(header_t1[1,], t_grob_1, along=2)
#grid.draw(t_grob_1)


kbl(t1) %>%
  kable_classic


# Table as plot -----------------------------------------------------------
# Quelle:

# Format table as plot
p3 <- ggplot() +
  annotation_custom(t_grob_1) +
  scale_x_continuous(breaks = c(1:3), limits = 1:3, expand = c(0,0)) +
  theme(plot.margin = unit(c(0, -1, 0, 0), "cm")) +
  NULL
# scale_y_continuous(breaks = c(1:17),
  #                    limits = c(1, 17),
  #                    expand = c(0,0.4))

  #scale_x_discrete(breaks = colnames(t1),
  #                 limits = c(colnames(t1), ""))


p3 + p1 + plot_layout(nrow = 1)



## combine plot and table grob

grid.arrange(t_grob_1, p_grob_1, ncol = 2#,
             #widths = unit(c(1, 5), "cm"),
             #heights = unit(c(1, 5), "cm"),
             #clip = TRUE
             )



#

# table as plot 2-----------------------------------------------------------
# Quelle: https://stackoverflow.com/questions/73250489/how-to-align-table-with-forest-plot-ggplot2

p3 <- ggplot(t1_long, aes(estimate, group, label = value)) +
  geom_text(size = 3) +
  scale_x_discrete(position = "top", labels = c("rr", "95% CI")) +
  labs(y = NULL, x = NULL) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.ticks = element_blank(),
    axis.title = element_text(face = "bold"),
  )

p3 + p1 + plot_layout(nrow = 1)


















##########################################
## heights/widths argument verstehen ##
############################################

# mit cowplot:
plot_grid(t_grob_1, p_grob_1, align = "h") #, rel_widths = c(0.5, 0.5)


library(ggpubr)
ggarrange(t_grob_1, p_grob_1, ncol = 2, align = "h")



# patchwork ---------------------------------------------------------------
library(patchwork)




# table als ggplot --------------------------------------------------------


p2 <- ggplot(t1_long, aes(x =estimate, y = group)) +
  ggforestplot::geom_stripes(odd = rgb(219, 238, 244, maxColorValue = 255),
                             even = "#00000000") +
  geom_text(aes(label = value, group = estimate), hjust = "outward") +
  theme_bar_iqb() +
  scale_x_discrete(position = "top") +
  theme(plot.margin = unit(c(0, -0.1, 0, 0), "cm")) +
  NULL

p2

plot_grid(p2, p1, nrow = 1, align = "h")


# table as ggplot function ------------------------------------------------
library(ggpmisc)
table_plot <- ggplot()+
  theme_void() +
geom_table(data = t1, label = list(t1), x = 0, y = 0) #  (ggpmsic)

plot_grid(table_plot, p1, nrow = 1, align = "h")


# cowplot -----------------------------------------------------------------

plot_grid(grid.table(t1), p1, nrow = 1, rel_widths = c(1/2, 1/2))

# gtable ------------------------------------------------------------------

a <- tableGrob(t1)
b <- grobTree(rectGrob(), textGrob("new\ncell"))
c <- ggplotGrob(qplot(1:10,1:10))
d <- linesGrob()
mat <- matrix(list(a, b, c, d), nrow = 2)
g <- gtable_matrix(name = "demo", grobs = mat,
                   widths = unit(c(2, 4), "cm"),
                   heights = unit(c(2, 5), c("in", "lines")))
g

plot(g)











mat <- matrix(list(tableGrob(t1), p1), nrow = 1)
gtable_matrix(name = "test", grobs = mat,
              widths = unit(c(4, 4), "cm"),
              heights = unit(c(5), "cm")
              )

a <- gtable(unit(c(5,5), c("cm")), unit(5, "cm"))

gtable_show_layout(a)
a <- gtable_add_grob(a, grobs = list(t_grob_1, p2), 1,1)
a <- gtable_add_grob(a, as.grob(p2), 1, 1)
grid.draw(a)


# ggplotify ---------------------------------------------------------------

# library(ggplotify)
# p3 <- as.ggplot(tableGrob(t1)) +
#   theme_minimal()



# gtable ------------------------------------------------------------------
library(gt)

colnames(t1) <- c("Land", "nicht adjustiert (M)", "adjustiert (M)")
gt_t1 <- t1 %>%
  gt() %>%
  tab_spanner(columns = c(2,3), label = "Mittelwerte") %>%
  tab_style(
    style = list(
      cell_fill(color = rgb(219, 238, 244, maxColorValue = 255))
    ),
    locations = cells_body(
     rows = c(2, 4, 6, 8, 10, 12, 14, 16))
  )

gtsave(gt_t1, filename = "gt_t1.png")

# Idee: als Bild speichern, neu laden und dann als Grob ausrichten


gt_t1



