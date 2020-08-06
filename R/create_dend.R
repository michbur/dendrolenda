

# helper function
create_dend <- function(x, col_name1, col_name2) {
  pivot_wider(dat, names_from = c(col_name2),
              values_from = "value") %>%
    as.data.frame() %>% {
      rownames(.) <- .[[col_name1]]
      .
    } %>%
    select(-1) %>%
    as.matrix() %>%
    dist() %>%
    hclust() %>%
    as.dendrogram()
}
