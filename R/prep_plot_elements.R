## All different plot elements need different data.frames.
## Problem: Es hängt aktuell von den Zeilen ab, was geplotted wird. Man könnte für alle Zeilenelemente eigene Spalten erstellen.
## Also: wholeGroup comparisons, BL comparisons,
## Idee für die FUnktion: Angeben für das jeweilige Element, was geplotted werden soll:


## nur die verschiedenen Signifikanzen ranhängen (within,  wholeGroup)


function(prepped_list){

trend_data <- prepped_list[["trend_data"]]

filtered_list[["bl_vs_wholeGroup"]] <- trend_data[get_group(trend_data$group, BLs) & get_wholeGroup(trend_data$group), ]

filtered_list[["wholeGroup_vs_wholeGroup"]] <- trend_data[get_group(trend_data$group, groups, starts_with = "^") & get_wholeGroup(trend_data$group) & !get_group(trend_data$group, BLs), ]

filtered_list[["bl_vs_bl"]] <- trend_data[get_group(trend_data$group, BLs, ends_with = "$")  & !is.na(trend_data$comparison), ] #& get_group(trend_data$group, groups)





}
