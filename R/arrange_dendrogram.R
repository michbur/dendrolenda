#' Arrange dendrograms with heatmap
#' 
#' Arranges both dendrograms with heatmap.
#' @param heatmap object containing heatmap plot
#' @param dendrogram_top top dendrogram plot generated with 
#' \code{\link{plot_dendrogram}}
#' @param dendrogram_right right dendrogram plot generated with 
#' \code{\link{plot_dendrogram}}
#' @param top_right element placed in the top right part of the plot.
#' By default \code{NULL} leaving empty space.
#' @param widths \code{numeric} vector of heatmap and right dendrogram widths
#' @param heights \code{numeric} vector of top dendrogram and heatmap heights
#' @importFrom ggplot2 ggplotGrob 
#' @importFrom grid grid.rect unit.pmax
#' @importFrom gridExtra grid.arrange
#' @examples 
#' data(example_data)
#' dendro_top <- plot_dendrogram(d1)
#' dendro_right <- plot_dendrogram(d2) + coord_flip()
#' arrange_dendrogram(hm, dendro_top, dendro_right)
#' @export
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
