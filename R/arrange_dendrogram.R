arrange_dendrogram <- function(heatmap,
                               dendrogram_top,
                               dendrogram_right,
                               top_right = NULL,
                               widths = c(0.8, 0.2),
                               heights = c(0.2, 0.8)) {

  grob_list <- list(heatmap = ggplotGrob(heatmap),
                    dendrogram_top = ggplotGrob(dendrogram_top),
                    dendrogram_right = ggplotGrob(dendrogram_right))

  if(is.null(top_right))
    top_right <- grid.rect(gp = gpar(col = NA))

  max_width <- unit.pmax(grob_list[["heatmap"]][["widths"]],
                         grob_list[["dendrogram_top"]][["widths"]])

  grob_list[["heatmap"]][["widths"]] <-
    grob_list[["dendrogram_top"]][["widths"]] <-
    max_width

  max_height <- unit.pmax(grob_list[["heatmap"]][["heights"]],
                          grob_list[["dendrogram_right"]][["heights"]])

  grob_list[["heatmap"]][["heights"]] <-
    grob_list[["dendrogram_right"]][["heights"]] <-
    max_height

  grid.arrange(grob_list[["dendrogram_top"]],
               top_right,
               grob_list[["heatmap"]],
               grob_list[["dendrogram_right"]],
               widths = widths, heights = heights)

}
