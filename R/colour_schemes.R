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
                                             "mitAdj" = "black"))
