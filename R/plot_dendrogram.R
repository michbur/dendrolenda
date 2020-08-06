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
