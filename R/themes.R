#' @importFrom assertthat assert_that
#' @importFrom grid is.unit unit_c
#' @importFrom vctrs vec_cast vec_c


#' @title theme_scientific
#' @description Generates scientific theme.
#' @param ticks Controls the tick size (and direction).
#' @param text.size Text font size by default.
#' @param title.size Title (labels') font size by default.
#' @param text.margin Text margin (added to the tick offset)
#' @param title.margin Title margin (added to the tick offset)
#' @param ... Other parameters passed to \code{ggplot2::theme}
#' @return A theme object.
#' @export
theme_scientific <- function(
    ticks = -u_(10$pt),
    text.size = 10,
    title.size = 15,
    text.margin = u_(5$pt),
    title.margin = u_(5$pt),
    text.color = "#000000",
    ...) {
    assertthat::assert_that(grid::is.unit(ticks), length(ticks) == 1L)

    assertthat::assert_that(grid::is.unit(text.margin))
    assertthat::assert_that(grid::is.unit(title.margin))
    text.size <- vctrs::vec_cast(text.size, double(), x_arg = "text.size", to_arg = "")
    title.size <- vctrs::vec_cast(title.size, double(), x_arg = "text.size", to_arg = "")
    vec_assert(text.color, character(), 1L)

    text.margin <- mar_(text.margin)
    title.margin <- mar_(title.margin)
    return(
        ggplot2::theme_bw() +
        ggplot2::theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks.length = ticks,
            axis.text.x =
                 ggplot2::element_text(size = text.size,
                         margin = with_mar(text.margin, t := ~.x - ticks),
                         colour = text.color),
             axis.text.y =
                     ggplot2::element_text(size = text.size,
                         margin = with_mar(text.margin, r := ~.x - ticks),
                         colour = text.color),
             axis.text.y.right =
                     ggplot2::element_text(size = text.size,
                         margin = with_mar(text.margin, l := ~.x - ticks),
                         colour = text.color),
             axis.text.x.top =
                     ggplot2::element_text(size = text.size,
                         margin = with_mar(text.margin, b := ~.x - ticks),
                         colour = text.color),
    #---------------------------------#
             axis.title.x =
                     ggplot2::element_text(size = title.size,
                         margin = with_mar(title.margin, t := ~.x - ticks),
                         colour = text.color),
             axis.title.y =
                     ggplot2::element_text(size = title.size,
                         margin = with_mar(title.margin, r := ~.x - ticks),
                         colour = text.color),
             axis.title.y.right =
                     ggplot2::element_text(size = title.size,
                         margin = with_mar(title.margin, l := ~.x - ticks),
                         colour = text.color),
             axis.title.x.top =
                     ggplot2::element_text(size = title.size,
                         margin = with_mar(title.margin, b := ~.x - ticks),
                         colour = text.color),
    #---------------------------------#
            legend.position = vctrs::vec_c(1, 0),
            legend.justification = vctrs::vec_c(1.05, -0.05),
            legend.background = element_blank(),
            legend.spacing = u_(0 ~ null),
    #---------------------------------#
            strip.placement = "outside",
            strip.background = element_blank(),
            strip.text = element_text(size = title.size, margin = title.margin),
            strip.switch.pad.grid = u_(0 ~ null),
            strip.switch.pad.wrap = u_(0 ~ null),
    #---------------------------------#
            panel.spacing = u_(0 ~ null),
    #---------------------------------#

             ...))
}
