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
    mutate(contributor_label = paste0(type, " (", rank.1, " -> ", rank.2, ")"))

  p <- ggplot(div, aes(x = reorder(type, divergence), y = contribution_pct, label = contributor_label)) +
    geom_bar(stat="identity", fill='steelblue') +
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
                        breaks = c(1,10,100), limits = c(1,100), name = "Count" ) + # why doesn't '100' show
    theme_light()

  p

}

#'-----------------------------------------------------
#'
#' Show the balances: total items, all items, exclusive items
#'
#' @param div a divergence object
#' @param labels a different set of labels, if you want (default Types, Size, Exclusive)
#' @return a ggplot object you can plot or modify as you see fit
#' @examples
#'
#' @export
plot_balances <- function(div, labels = c('Types', 'Size', 'Exclusive')) {

  # https://www.onceupondata.com/2019/01/25/ggplot2-divergent-bars/
  df <- tibble(Size = round(div$size_total * 100, 2),
               Types = round(div$types_total * 100, 2),
               Exclusive = round(div$exclusive_types * 100, 2),
               group = c(1,2))

  df <- df %>%
    pivot_longer(cols = c(Size, Types, Exclusive)) %>%
    mutate(abs = value,
           value = ifelse(group == 1, -value, value))

  df <- df  %>% left_join( tibble(old_name = c('Types', 'Size', 'Exclusive'), new_name = labels),
                           by = c('name' = 'old_name')) %>%
    select(-name) %>%
    rename(name = new_name)

  ## calculate breaks values
  breaks_values <- pretty(df$value)
  df$name <- factor(df$name,levels = rev(labels))
  df$group <- factor(df$group)
  p <- df %>%
    ggplot(aes(x = name, y = value, fill = group))+
    geom_bar(stat = "identity", show.legend = FALSE)+
    coord_flip() +
    xlab("") +
    ylab("Percent of Balance") +
    scale_y_continuous(breaks = breaks_values,
                       labels = abs(breaks_values)) +
    suppressWarnings(geom_bar_text(aes(label = abs), # says 'ignoring unknown aesthetics: label', but doing what I wish :/
      reflow = TRUE,
      place = "left",
      outside = FALSE)) +
    theme_light() +
    scale_fill_manual(guide = FALSE, breaks = c(1,2), values=c("gray", "lightblue"))
  p
}

# ROTATE?
# pushViewport(viewport(type = "rotate", angle = -135, clip = "off", width = 0.7, height = 0.7))
# print(p, vp = "rotate")
# p

# MUCHO TODOs

# contours
# labels on histo
# more tests!
# put on github


