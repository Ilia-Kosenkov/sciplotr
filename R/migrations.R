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
#' @import ggplot2
#' @export
theme_scientific <- function(
    ticks = u_(-10, pt),
    text.size = 10,
    title.size = 15,
    text.margin = u_(5, pt),
    title.margin = u_(5, pt),
    ...) {
    assert_that(is.unit(ticks), length(ticks) == 1L)
    assert_that(is.unit(text.margin), length(text.margin) == 1L)
    assert_that(is.unit(title.margin), length(title.margin) == 1L)
    text.size <- vec_cast(text.size, double(), x_arg = "text.size", to_arg = "")
    title.size <- vec_cast(title.size, double(), x_arg = "text.size", to_arg = "")

    return(theme_bw() +
                theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
               axis.ticks.length = ticks,
               axis.text.x =
                    element_text(size = text.size,
                        margin = new_margin(t = text.margin - ticks),
                        colour = "#000000"),
                axis.text.y =
                        element_text(size = text.size,
                        margin = new_margin(r = text.margin - ticks),
                        colour = "#000000"),
                axis.text.y.right =
                        element_text(size = text.size,
                        margin = new_margin(l = text.margin - ticks),
                        colour = "#000000"),
                axis.text.x.top =
                        element_text(size = text.size,
                        margin = new_margin(b = text.margin - ticks),
                        colour = "#000000"),
    #------------------------------------#
                axis.title.x =
                        element_text(size = title.size,
                        margin = new_margin(t = title.margin),
                        colour = "#000000"),
                axis.title.y =
                        element_text(size = title.size,
                        margin = new_margin(r = title.margin),
                        colour = "#000000"),
                axis.title.y.right =
                        element_text(size = title.size,
                        margin = new_margin(l = title.margin),
                        colour = "#000000"),
                axis.title.x.top =
                        element_text(size = title.size,
                        margin = new_margin(b = title.margin),
                        colour = "#000000"),
                ...))
}

#' @title new_margin
#' @description Creates a new instance of margin. Supports unit arithmetics
#' @param ... Units to construct margin
#' @return An instance of \code{margin}
#' @export
new_margin <- function(...) {
    args <- list2(...)

    if (vec_size(args) == 1L && is_null(names(args))) {
        unit <- args[[1]]
        assert_that(is.unit(unit))
        len = length(unit)
        if (len == 1L)
            margin = rep(unit, 4L)
        else if (len == 2)
            margin = rep(unit,2L)
        else if (len == 4)
            margin = unit
        else
            stop(sprintf("Single unit argument should have lengths of 1, 2 or 4, but input vector has size %d", length(unit)))
    }
    else if (vec_size(args) == 4L && is_null(names(args))) {
        margin <- unit.c(args[[1]], args[[2]], args[[3]], args[[4]])
    }
    else if(!is_null(names(args))){
        names(args) <- sapply(names(args), match.arg,
            vec_c("top", "right", "bottom", "left", "horizontal", "vertical"))
        lapply(args, function(x) assert_that(is.unit(x), length(x) == 1L))

        has_vertical <- vec_in("vertical", names(args))
        has_horizontal <- vec_in("horizontal", names(args))

        if (is_null(args$top))
            args$top <- if(has_vertical) args$vertical else unit(0, "null")
        if (is_null(args$bottom))
            args$bottom <- if(has_vertical) args$vertical else unit(0, "null")

        if (is_null(args$left))
            args$left <- if(has_horizontal) args$horizontal else unit(0, "null")
        if (is_null(args$right))
            args$right <- if(has_horizontal) args$horizontal else unit(0, "null")

        margin <- unit.c(args$top, args$right, args$bottom, args$left)
    }
    else
        stop("Invalid input")
    
    class(margin) <- vec_c("margin", class(margin))
    margin
}


#' Unit division
#'
#' @param e1 \code{unit} on the lhs.
#' @param e2 Number on the rhs.
#'
#' @return Modified \code{unit}
#' @export
`/.unit` <- function(e1, e2) {
    e2 <- vec_cast(e2, double(), x_arg = "e2", to_arg = "")
    vec_assert(e2, double(), size = 1L)
    e1 * (1 / e2)
}


u_ <- function(x, unit, data = NULL) {
    unit <- ensym(unit)
    x <- vec_cast(x, double(), to_arg = "")
    unit(x, as.character(unit), data)
}

npc_ <- function(x) u_(x, npc)
cm_ <- function(x) u_(x, cm)
in_ <- function(x) u_(x, `in`)
pt_ <- function(x) u_(x, pt)

mar_ <- function(t, r, b, l, unit, data = NULL) {
    unit <- ensym(unit)
    t <- vec_cast(t, double(), x_arg = "t", to_arg = "")
    r <- vec_cast(r, double(), x_arg = "r", to_arg = "")
    b <- vec_cast(b, double(), x_arg = "b", to_arg = "")
    l <- vec_cast(l, double(), x_arg = "l", to_arg = "")
    vec_assert(t, size = 1L)
    vec_assert(r, size = 1L)
    vec_assert(b, size = 1L)
    vec_assert(l, size = 1L)

    unit <- as.character(unit)

    margin <-
    unit.c(unit(t, unit, data),
           unit(r, unit, data),
           unit(b, unit, data),
           unit(l, unit, data))

    class(margin) <- vec_c("margin", class(margin))
    margin
}