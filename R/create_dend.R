

# helper function
#' @importFrom stats as.dendrogram hclust dist
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr %>% select
create_dend <- function(x, col_name1, col_name2) {
  pivot_wider(x, names_from = c(col_name2),
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
