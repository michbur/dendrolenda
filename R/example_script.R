library(ggplot2)
library(tidyr)
library(dplyr)
library(grid)
library(gridExtra)
library(ggdendro)

dat <- expand.grid(x1 = sapply(1L:10, function(dummy)
  paste0(sample(letters, 3, replace = TRUE), collapse = "")),
  x2 = paste0("strain", 1L:6)) %>%
  mutate(value = sample(0L:1, size = 60, replace = TRUE))

d1 <- create_dend(dat, "x1", "x2")
d2 <- create_dend(dat, "x2", "x1")

hm_dat <- mutate(dat,
                 x1 = factor(x1),
                 x1 = factor(x1, levels = levels(x1)[order.dendrogram(d1)]),
                 x2 = factor(x2),
                 x2 = factor(x2, levels = levels(x2)[order.dendrogram(d2)]))



hm <- ggplot(hm_dat, aes(x = x1, y = x2, fill = factor(value))) +
  geom_tile(color = "black") +
  theme(legend.position = "bottom")


# d1 <- plot_dendrogram(d1)
# d2 <- plot_dendrogram(d2) + coord_flip()
# 
# arrange_dendrogram(hm, d1, d2)
# 
# arrange_dendrogram(hm + theme_bw() + theme(legend.position = "bottom"),
#                    d1 + theme_void(),
#                    d2 + theme_void())

get_legend <- function(gg_plot) {
  grob_table <- ggplotGrob(gg_plot)
  grob_table[["grobs"]][[which(sapply(grob_table[["grobs"]], function(x) x[["name"]]) == "guide-box")]]
}

