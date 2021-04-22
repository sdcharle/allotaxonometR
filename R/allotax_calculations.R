# Allotaxonometry
# Main References
# https://arxiv.org/pdf/2002.09770.pdf
# https://gitlab.com/compstorylab/allotaxonometer

#'--------------------------------
#'
#' Calculate values relating to rank turbulence between two groups. Note original code calls things 'types'
#' and 'sizes' whereas we go with types amd size
#'
#' @param df1 a frame with type and size columns
#' @param df2 another frame with type and size columns
#' @param alpha alpha as defined in the paper, ranges from zero to Inf zero accentuates rare, Inf accentuates common
#' @param ties.method the method of handling ties, one of the options for the rank function: "average", "first", "last, "random", "max", "min"
#'
#' @return a list containing:
#'             size_total: sizes for the two groups, expressed as percentage of the total size across groups
#'             types_total: breakdown of types in each group as percentage of types across both groups
#'             exclusive_types: percent of types in a group exclusive to the group
#'             normalization: value used to normalize results
#'             divergences: data frame with columns: type, divergence, sign. Negative sign if type ranked higher in the
#'                             first group
#' @examples
#'
#' @export

rank_turbulence <- function(df1, df2, alpha = 1/3, ties.method = "average") {

  results <- list()

  big_frame <- df1 %>% full_join(df2, by = c('type' = 'type'))

  pct_1 <- sum(big_frame$size.x, na.rm = TRUE)/(sum(big_frame$size.x, na.rm = TRUE) +
                                                  sum(big_frame$size.y, na.rm = TRUE))
  results$size_total <- c(pct_1, 1-pct_1)
  # for NBA should be 48.8, 51.1
  total_types <- length(unique(union(df1$type, df2$type)))

  results$types_total <- c(nrow(df1)/total_types, nrow(df2)/total_types)
  # should be 80.4 79.0 - getting 82.1, 80.5 # get 80.4, 79 if we filter 0 point players...
  # get 82, 80.1 if we filter 0 pts up front

  exc_1 <- sum(!is.na(big_frame$size.x) & big_frame$size.x > 0 & is.na(big_frame$size.y))
  exc_2 <- sum(is.na(big_frame$size.x) & !is.na(big_frame$size.y) & big_frame$size.y > 0)
  # NBA: 26.1, 24.8 # 23.7, 22.3??? #  if filter out non scorers
  results$exclusive_types <- c(exc_1/nrow(df1), exc_2/nrow(df2))

  indices1 = !is.na(big_frame$size.x)
  indices2 = !is.na(big_frame$size.y)

  minsize1 <- min(big_frame$size.x, na.rm = T)
  minsize2 <- min(big_frame$size.y, na.rm = T)

  N1 = sum(indices1)
  N2 = sum(indices2)

  ranks1disjoint <- N2 + N1/2
  x1disjoint = 1/ranks1disjoint

  ranks2disjoint <- N1 + N2/2
  x2disjoint <-  1/ranks2disjoint;

  # fix nas now we did that
  big_frame <- big_frame %>%
    mutate(size.x = replace_na(size.x, minsize1 - 1),
           size.y = replace_na(size.y,minsize2 - 1))

  big_frame$rank.x <- rank(-big_frame$size.x, ties.method = "average")
  big_frame$rank.y <- rank(-big_frame$size.y, ties.method = "average")

  x1 <- big_frame$rank.x ^ -1
  x2 <- big_frame$rank.y ^ -1

  if (alpha < 0) {
    stop('alpha must be >= 0')
  } else if (alpha == Inf) {
    divergence_elements = pmax(x1,x2)
    divergence_elements[(x1==x2)] = 0
  } else if (alpha == 0) {
    divergence_elements = log(pmax(1./x1,1./x2)/pmin(1./x1,1./x2))
  } else {
    divergence_elements = (alpha+1)/alpha*
      (abs(x1^alpha - x2^alpha))^(1./(alpha+1))
  }

  if (alpha == Inf) {
    normalization = sum(x1[indices1]) + sum(x2[indices2]);
  } else if (alpha == 0) {
      normalization = sum(abs(log(x1[indices1] / x2disjoint))) +
      sum(abs(log(x2[indices2] / x1disjoint)))
  }  else {
      normalization <- (alpha+1)/alpha *
        sum((abs(x1[!is.na(x1)]^alpha - x2disjoint^alpha))^(1./(alpha+1))) +
        (alpha+1)/alpha *
        sum((abs(x1disjoint^alpha - x2[!is.na(x2)]^alpha))^(1./(alpha+1)))
  }

  divergence_elements = divergence_elements / normalization;

  signs <- sign(x1 - x2) # pos means side 1 bigga

    # see:
  # https://gitlab.com/compstorylab/allotaxonometer/-/blob/master/scripts-divergences/rank_turbulence_divergence.m
  # rank, I get stephen curry 29.5 -> 8 vs 29 -> 8 sexton 621 -> 39 they have 594 -> 39


  results$alpha = alpha
  results$normalization = normalization
  results$divergences = tibble(type = big_frame$type,
                               divergence = divergence_elements,
                               sign = signs,
                               rank.1 = big_frame$rank.x,
                               rank.2 = big_frame$rank.y,
                               contribution_pct = 100. * abs(divergence_elements)/sum(abs(divergence_elements)) * -1* signs)

  return(results)
  # total div .280 vs .300
}

# To do - implement the other measures


