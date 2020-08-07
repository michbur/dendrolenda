#' Get legend
#' 
#' Extracts legend grob from a ggplot object
#' @param gg_plot \code{ggplot} object from which legend will be extracted
#' @return object containing legend
#' @importFrom ggplot2 ggplotGrob
#' @export
#' @examples 
#' data(examples_data)
#' get_legend(hm)
get_legend <- function(gg_plot) {
  grob_table <- ggplotGrob(gg_plot)
  grob_table[["grobs"]][[which(sapply(grob_table[["grobs"]], function(x) x[["name"]]) == "guide-box")]]
}
