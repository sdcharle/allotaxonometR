# Allotaxonometry - the plots!

#'-----------------------------------------------------
#'
#' Show top n contributors as a labeled bar chart
#'
#' @param div a divergence object
#' @param n how many to show (default = 50)
#' @return a ggplot object you can plot or modify as you see fit
#' @examples
#'
#' @export
plot_contributors <- function(div, n = 50) {

  div <- div$divergences

  # TODO - unicode \u21cb in there
  # https://michaelbach.de/2020/03/22/ggplotAndUnicode.html
  div <- div %>%
    arrange(desc(divergence)) %>%
    head(n) %>%
    mutate(contributor_label = paste0(name, " (", rank.1, " -> ", rank.2, ")"))

  p <- ggplot(div, aes(x = reorder(name, divergence), y = contribution_pct, label = contributor_label)) +
    geom_bar(stat="identity", color='lightblue',fill='steelblue') +
    ylab("Contribution %") +
    xlab("") +
    geom_bar_text(
      reflow = TRUE,
      place = "left",
      outside = FALSE) +
    coord_flip() +
    theme_light() +
    scale_x_discrete(labels = NULL )
  p

}

#'-----------------------------------------------------
#'
#' Show the 2D histogram of the divergences
#'
#' TODO - how best to include labels.
#'
#' @param div a divergence object
#' @param bins how many bins (default = 50)
#' @return a ggplot object you can plot or modify as you see fit
#' @examples
#'
#' @export
plot_2d_hist <- function(div, bins = 50) {

  div <- div$divergences

  p <- ggplot(div, aes(x=rank.2, y=rank.1) ) +
    geom_bin2d(bins = bins) +
    scale_x_log10() +
    scale_y_log10() +
    scale_fill_viridis( option = "magma", begin = 0, end = .97, direction = -1, trans = "log",
                        breaks = c(1,10,100), limits = c(1,100) ,name = "Count") + # why doesn't '100' show
    theme_light()

  p

}

#'-----------------------------------------------------
#'
#' Show the balances: total items, all items, exclusive items
#'
#' @param div a divergence object
#' @return a ggplot object you can plot or modify as you see fit
#' @examples
#'
#' @export
plot_balances <- function(div) {
  print("NOT IMPLEMENTED JACK")
}


# ROTATE?
# pushViewport(viewport(name = "rotate", angle = -135, clip = "off", width = 0.7, height = 0.7))
# print(p, vp = "rotate")
# p

