# Allotaxonometry
# Main References
# https://arxiv.org/pdf/2002.09770.pdf
# https://gitlab.com/compstorylab/allotaxonometer


#'--------------------------------
#'
#' Calculate values relating to rank turbulence between two groups
#'
#' @param df1 a frame with name and count columns
#' @param df2 another frame with name and count columns
#' @param alpha alpha as defined in the paper, ranges from zero to Inf
#' @param ties.method the method of handling ties, one of the options for the rank function: "average", "first", "last, "random", "max", "min"
#'
#' @return a list containing:
#'             count_total: counts for the two groups, expressed as percentage of the total count across groups
#'             items_total: breakdown of items in each group as percentage of items across both groups
#'             exclusive_items: percent of items in a group exclusive to the group
#'             normalization: value used to normalize results
#'             divergences: data frame with columns: name, divergence, sign. Negative sign if item ranked higher in the
#'                             first group
#' @examples
#'
#' @export

rank_turbulence <- function(df1, df2, alpha = 1/3, ties.method = "average") {

  # q, how is it handlin na's?
  # convert xs to zero?

  results <- list()

  big_frame <- df1 %>% full_join(df2, by = c('name' = 'name'))

  pct_1 <- sum(big_frame$count.x, na.rm = TRUE)/(sum(big_frame$count.x, na.rm = TRUE) +
                                                  sum(big_frame$count.y, na.rm = TRUE))
  results$count_total <- c(pct_1, 1-pct_1)
  # for NBA should be 48.8, 51.1
  total_items <- length(unique(union(df1$name, df2$name)))

  results$items_total <- c(nrow(df1)/total_items, nrow(df2)/total_items)
  # should be 80.4 79.0 - getting 82.1, 80.5 # get 80.4, 79 if we filter 0 point players...
  # get 82, 80.1 if we filter 0 pts up front

  exc_1 <- sum(!is.na(big_frame$count.x) & big_frame$count.x > 0 & is.na(big_frame$count.y))
  exc_2 <- sum(is.na(big_frame$count.x) & !is.na(big_frame$count.y) & big_frame$count.y > 0)
  # NBA: 26.1, 24.8 # 23.7, 22.3??? #  if filter out non scorers
  results$exclusive_items <- c(exc_1/nrow(df1), exc_2/nrow(df2))

  indices1 = !is.na(big_frame$count.x)
  indices2 = !is.na(big_frame$count.y)

  mincount1 <- min(big_frame$count.x, na.rm = T)
  mincount2 <- min(big_frame$count.y, na.rm = T)

  N1 = sum(indices1)
  N2 = sum(indices2)

  ranks1disjoint <- N2 + N1/2
  x1disjoint = 1/ranks1disjoint

  ranks2disjoint <- N1 + N2/2
  x2disjoint <-  1/ranks2disjoint;

  # fix nas now we did that
  big_frame <- big_frame %>%
    mutate(count.x = replace_na(count.x, mincount1 - 1),
           count.y = replace_na(count.y,mincount2 - 1))

  big_frame$rank.x <- rank(-big_frame$count.x, ties.method = "average")
  big_frame$rank.y <- rank(-big_frame$count.y, ties.method = "average")

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

  # add - no, yo DON'T want items from set one w/ count 0 in normalization.
  # see:
  # https://gitlab.com/compstorylab/allotaxonometer/-/blob/master/scripts-divergences/rank_turbulence_divergence.m
  # rank, I get stephen curry 29.5 -> 8 vs 29 -> 8 sexton 621 -> 39 they have 594 -> 39

  results$normalization = normalization
  results$divergences = tibble(name = big_frame$name,
                               divergence = divergence_elements,
                               sign = signs,
                               rank.1 = big_frame$rank.x,
                               rank.2 = big_frame$rank.y)

  return(results)
  # total div .280 vs .300
}

