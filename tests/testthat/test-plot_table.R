
library(tidyverse)
library(grid)
library(cowplot)
library(gridExtra)
library(kableExtra)

# https://www.christophenicault.com/post/understand_size_dimension_ggplot2/

# Setup -------------------------------------------------------------
bt21_NoTrend <- readxl::read_excel("Q:/BT2021/BT/60_Bericht/04_Mittelwerte/adjustierte Mittelwerte/Ergebnisse/01_ohneTrend_z_standard.xlsx")

# Datenaufbereitung
bt21_NoTrend_prep <- bt21_NoTrend %>%
  mutate(significant = factor(ifelse(p < 0.05, "yes", "no"))) %>%
  mutate(adj_sig = case_when(
    adjust == "ohneAdj" & significant == "no" ~ "noAdj_noSig",
    adjust == "ohneAdj" & significant == "yes" ~ "noAdj_Sig",
    adjust == "mitAdj" & significant == "no" ~ "Adj_noSig",
    adjust == "mitAdj" & significant == "yes" ~ "Adj_Sig"
  )) %>%
  filter(kb == "GL" & comparison == "crossDiff")

# Tabelle erstellen
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

# Longformat
t1_long <- t1 %>%
  mutate_at(c("est.ohneAdj", "est.mitAdj"), round) %>%
  mutate_at(c("est.ohneAdj", "est.mitAdj"), as.character) %>%
  mutate(Bundesland = group) %>%
  pivot_longer(cols = c("est.ohneAdj", "est.mitAdj", "Bundesland"), names_to = "estimate" ) %>%
  mutate(value = ifelse(estimate == "Bundesland", NA, value))


## Plot plot
p1 <- plot_bar(bt21_NoTrend_prep) +
  #scale_x_continuous(expand = c(0, 0)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
  scale_x_continuous(sec.axis= sec_axis(
                     trans = ~ . * 1,
                     labels = c("", "", "Abweichung vom Mittelwert für \n Deutschland insgesamt", "", "")
                     )
  ) +
  theme(axis.text.x.top = element_text(size = 10, hjust = 0.5, color = "black", face = "bold"),
        axis.line.x.top = element_line()
        )




# Grobs -------------------------------------------------------------------

#ggplot_build(p1)$layout$panel_params[[1]]$y.labels

## convert to grobs
t_grob_1 <- tableGrob(t1, theme = t1_theme)

# Set widths/heights to 'fill whatever space I have'
t_grob_1$widths <- unit(rep(1, ncol(t_grob_1)), "null")
t_grob_1$heights <- unit(rep(1, nrow(t_grob_1)), "null")


# Colspanners
colnames(t1) <- NULL

header_t1 <- tableGrob(t1[1, 1:3], rows=NULL, cols=c("head1", "head2", "head3"))
p_grob_1 <- ggplotGrob(p1)
t_grob_1 <- gtable_combine(header_t1[1,], t_grob_1, along=2)
#grid.draw(t_grob_1)


grid.arrange(t_grob_1, p_grob_1, ncol = 2#,
             #widths = unit(c(1, 5), "cm"),
             #heights = unit(c(1, 5), "cm"),
             #clip = TRUE
)


kbl(t1) %>%
  kable_classic


# mit cowplot:
plot_grid(t_grob_1, p_grob_1, align = "h") #, rel_widths = c(0.5, 0.5)


library(ggpubr)
ggarrange(t_grob_1, p_grob_1, ncol = 2, align = "h")



# Tabelle als ggplot -----------------------------------------------------------

# Column names sind jetzt axis labels, und können als solche variiert werden:
# https://stackoverflow.com/questions/20571306/multi-row-x-axis-labels-in-ggplot-line-chart

################
## Versuch 1: ##
################

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

##############
# Versuch 2 ##
##############

# Quelle: https://stackoverflow.com/questions/73250489/how-to-align-table-with-forest-plot-ggplot2

p3 <- ggplot(t1_long, aes(x = estimate, y = group, label = value)) + # x-Achse adjustieren
  ggforestplot::geom_stripes(odd = rgb(219, 238, 244, maxColorValue = 255),
                             even = "#00000000") +
  geom_text(size = 3,
            hjust = "center"
            #nudge_x = c(-0.3, 0, 0)
            ) +
  geom_text(data = t1_long %>% filter(estimate == "Bundesland"),
            aes(x = "Bundesland", label = group),
            size = 3,
            hjust = "left",
            nudge_x = -0.55) +
scale_x_discrete(position = "top",
                 labels = c("Land",
                            expression(atop(underline(atop("nicht", "adjustiert")), italic("M"))),
                            "adjustiert")) +
  labs(y = NULL, x = NULL) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 10, hjust = 0.5, color = "black", face = "bold"),
    axis.ticks = element_blank(),
    #axis.title = element_text(face = "bold", hjust = 0),
    plot.margin = unit(c(0, -0.1, 0, 0), "cm")
  )

p_bartable <- plot_grid(p3, p1, nrow = 1, align = "h")
p_bartable


## column spanning labels:
# https://stackoverflow.com/questions/44616530/axis-labels-on-two-lines-with-nested-x-variables-year-below-months
# https://stackoverflow.com/questions/20571306/multi-row-x-axis-labels-in-ggplot-line-chart


#p3 + p1 + plot_layout(nrow = 1)

###############
# Versuch 3 ###
###############

p2 <- ggplot(t1_long, aes(x =estimate, y = group)) +
  ggforestplot::geom_stripes(odd = rgb(219, 238, 244, maxColorValue = 255),
                             even = "#00000000") +
  geom_text(aes(label = value, group = estimate), hjust = 0) +
  theme_bar_iqb() +
  scale_x_discrete(position = "top") +
  theme(plot.margin = unit(c(0, -0.1, 0, 0), "cm")) +
  NULL

p2

plot_grid(p2, p1, nrow = 1, align = "h")

###############
## Versuch 4 ##
###############

library(ggpmisc)
table_plot <- ggplot()+
  theme_void() +
geom_table(data = t1, label = list(t1), x = 0, y = 0) #  (ggpmsic)

plot_grid(table_plot, p1, nrow = 1, align = "h")




# Pakete zur Tabellenerestellung ------------------------------------------

############
## gtable ##
############

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


##############
# ggplotify ##
##############

# library(ggplotify)
# p3 <- as.ggplot(tableGrob(t1)) +
#   theme_minimal()



###########
# gtable ##
###########

library(gt)
# t1$image <- ""
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

gtsave(gt_t1, "gt_t1.png")


# Einfügen von Plots in Tabellenzellen ------------------------------------

################
## kableExtra ##
################

## Möglichkeit: Einfügen von einzelnen Plots in die Tabellenzelle. Dafür zuerst einzelne Ggplot-Objekte erzeugen, abspeichern und dann als png wieder in die Tabelle laden.

p1_a <- plot_bar(bt21_NoTrend_prep %>% filter(group == "Berlin")) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

p1_b <- plot_bar(bt21_NoTrend_prep %>% filter(group == "Hamburg")) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))




gt_t1 %>%
  text_transform(locations = cells_body(columns = image),
                 fn = function(x) {
                   p1_a %>%
                     ggplot_image(height = px(50), aspect_ratio = 5)
                 }

  )







tbl_img <- data.frame(name = c("Berlin"), image = "")

tbl_img %>%
kbl(booktabs = T) %>%
  kable_paper(full_width = F) %>%
  column_spec(2, image = spec_image("Rplot05.png", 50, 50))





# Beispiel für Plots in Zellen  ---------------------------------------------------------------


#
# library(ggplot2)
# library(gt)
#
# # make a plot of each mfr
# tibble_plot <- gtcars %>%
#   group_by(mfr) %>%
#   nest() %>%
#   mutate(plot = map(data, ~ggplot(., aes(hp, trq, size = msrp)) + #you could use the function and it should work
#                       geom_point() +
#                       geom_point(color = "blue") +
#                       theme(legend.position = "none"))) %>%
#   select(-data) %>%
#   # Create empty column (a placeholder for the images)
#   mutate(ggplot = NA)
#
#
# #Creates the length of the tibble
# text_names <- gtcars %>%
#   select(mfr) %>%
#   unique() %>%
#   pull()
#
#
# # Is a bit slow for me
# tibble_output <- tibble(
#   text = text_names,
#   ggplot = NA,
#   .rows = length(text_names)) %>%
#   gt() %>%
#   text_transform(
#     locations = cells_body(vars(ggplot)),
#     fn = function(x) {
#       map(tibble_plot$plot, ggplot_image, height = px(200))
#     }
#   )
#
# tibble_output
#
#
#



# Use PNG in coordinate system --------------------------------------------
library(grid)
install.packages("png")
library(png)
library(patchwork)


my_image1 <-  readPNG("Rplot06.png")


my_image_mod1 <- ggplot(bt21_NoTrend_prep, aes(x = est, y = group)) +
  geom_point(col = "white") +
  xlim(-5, 40) +
  coord_fixed(4.5) +
  annotation_custom(rasterGrob(my_image1, width = 1, height = 1),
                    #xmin = - 4.5, xmax = Inf,
                    #ymin = - Inf, ymax = Inf
                    ) +
  theme_bar_iqb() +
  scale_x_discrete(position = "top") +
  theme(plot.margin = unit(c(0, -0.1, 0, 0), "cm")) +
  NULL

my_image_mod1 + p1



# ggpubr ------------------------------------------------------------------

library(ggpubr)

t1_plot <- ggtexttable(t1, rows = NULL)

t1_plot + p1

plot_grid(t1_plot, p1, nrow = 1, align = "h")


# build every element as own grob and combine manually in loop ------------



# Wunsch: Tabelle hat y-Achse mit den entsprechenden Ländernamen, die dann automatisch aligned werden kann


# Tabelle als ggplot element. Dafür Ab Kapitel 19 lesen: https://ggplot2-book.org/programming.html

# Oder: y-Achse so legen, dass die Ticks genau in der Mitte der jeweiligen Reihe liegen.
