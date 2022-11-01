# The IQB-specific colour schemes, linetypes and pointshapes.


##
fill_iqb <- ggplot2::scale_fill_manual(

  values = c("ohneAdj" = rgb(147, 205, 221,
                             maxColorValue = 255
                             ),
             "mitAdj"  = rgb(33, 89, 104,
                             maxColorValue = 255
                             )
  )
)


fill_iqb_adj_sig <- ggplot2::scale_fill_manual(

  values = c("noAdj_Sig" = rgb(147, 205, 221,
                               maxColorValue = 255),
             "Adj_Sig" = rgb(33, 89, 104, maxColorValue = 255),
             "noAdj_noSig" = "white",
             "Adj_noSig" = "white"
  )
)



colour_iqb <- ggplot2::scale_colour_manual(values = c("ohneAdj" = rgb(147, 205, 221, maxColorValue = 255),
                                                      "mitAdj" = "black")) # Gruppen immer als 1 und 0

grouping_colours <- ggplot2::scale_colour_manual(values = c("0" = rgb(166, 166, 166, maxColorValue = 255),
                                                            "1" = "black"))


## Linetype: solid and dashed
linetype_iqb <- ggplot2::scale_linetype_manual(values=c("bold" = 1,
                                                        "plain" = 2)
)

## Pointshape: triangle and circle
pointshape_iqb <- ggplot2::scale_shape_manual(values=c("Sig" = 17,
                                                       "noSig" = 16)
)
