#' Plot dendrogram
#' 
#' Plots dendrogram as a segment with leaf labels.
#' @param dendro object of class \code{dendrogram}
#' @return dendrogram plot
#' @importFrom dplyr %>%  
#' @import ggplot2
#' @importFrom stats nobs
#' @importFrom ggdendro dendro_data segment
#' @examples 
#' data(example_data)
#' plot_dendrogram(d1)
#' @export
plot_dendrogram <- function(dendro) {
  dendro %>%
    dendro_data %>%
    segment %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_segment() +
    scale_y_continuous("") +
    scale_x_discrete("",
                     limits = factor(1L:nobs(dendro)),
                     labels = labels(dendro))
}
