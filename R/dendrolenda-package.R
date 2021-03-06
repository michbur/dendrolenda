#' dendrolenda
#'
#' dendrolenda package allows easy visualization of heatmaps with dendrograms.
#' The main function \code{\link{arrange_dendrogram}} aligns dendrograms
#' with heatmap, so manual arrangement of plots is not necessary!
#'
#' @section To hard-working Rafal:
#'
#' We know how hard you work and how much effort you put in the presentation of
#' your results, so here you have a little tool to make your work a bit
#' (hopefully) easier.
#'
#' @section Happy Birthday, Rafal!:
#' We know how many heatmaps you plot \cr
#' and their arrangement can take a lot. \cr
#' So to save you some time and nerves \cr
#' we built a package our friend deserves. \cr
#' Now you can efficiently plot an test. \cr
#' We wish you all the best!
#'
#'
#' @importFrom stats as.dendrogram dist hclust nobs
#' @author Michal Burdukiewicz, Katarzyna Sidorczuk, Stefan Roediger
#' @docType package
#' @name dendrolenda-package
#' @aliases dendrolenda
#'
if(getRversion() >= "2.15.1")
  utils::globalVariables(c("x", "y", "xend", "yend"))
