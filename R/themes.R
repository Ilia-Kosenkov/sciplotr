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
    ticks = u_(-10$pt),
    text.size = 10,
    title.size = 15,
    text.margin = u_(5$pt),
    title.margin = u_(5$pt),
    text.color = "#000000",
    ...) {
    assertthat::assert_that(grid::is.unit(ticks), length(ticks) == 1L)
    assertthat::assert_that(grid::is.unit(text.margin), length(text.margin) == 1L)
    assertthat::assert_that(grid::is.unit(title.margin), length(title.margin) == 1L)
    text.size <- vctrs::vec_cast(text.size, double(), x_arg = "text.size", to_arg = "")
    title.size <- vctrs::vec_cast(title.size, double(), x_arg = "text.size", to_arg = "")
    vec_assert(text.color, character(), 1L)
    return(
        ggplot2::theme_bw() +
        ggplot2::theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.ticks.length = ticks,
            axis.text.x =
                 ggplot2::element_text(size = text.size,
                         margin = new_margin(t = text.margin - ticks),
                         colour = text.color),
             axis.text.y =
                     ggplot2::element_text(size = text.size,
                         margin = new_margin(r = text.margin - ticks),
                         colour = text.color),
             axis.text.y.right =
                     ggplot2::element_text(size = text.size,
                         margin = new_margin(l = text.margin - ticks),
                         colour = text.color),
             axis.text.x.top =
                     ggplot2::element_text(size = text.size,
                         margin = new_margin(b = text.margin - ticks),
                         colour = text.color),
    #---------------------------------#
             axis.title.x =
                     ggplot2::element_text(size = title.size,
                         margin = new_margin(t = title.margin),
                         colour = text.color),
             axis.title.y =
                     ggplot2::element_text(size = title.size,
                         margin = new_margin(r = title.margin),
                         colour = text.color),
             axis.title.y.right =
                     ggplot2::element_text(size = title.size,
                         margin = new_margin(l = title.margin),
                         colour = text.color),
             axis.title.x.top =
                     ggplot2::element_text(size = title.size,
                         margin = new_margin(b = title.margin),
                         colour = text.color),
             ...))
}
