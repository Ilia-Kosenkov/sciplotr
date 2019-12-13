#' @title Transforms
#' @rdname trans
#' @param n Parameter that weakly controls the number of generated large breaks.
#' @param modifier Affect the set of allowed step sizes for a given \code{n}.
#' @param n_small Same as \code{n}, but for small breaks.
#' @param format Formatter that generates large breaks' labels.
#'
#' @return A \code{ggplot2}'s \code{trans} object
#' @export
identity_sci_trans <- function(
    n = 5L, modifier = vctrs::vec_c(1, 2, 2.5, 5), n_small = 50L,
    format = scales:::format_format()) {
    trans_new("identity_sci", "force", "force",
              breaks = simple_breaks(n = n, modifier = modifier),
              minor_breaks = simple_minor_breaks(n = n_small),
              format = format)
}

#' @rdname trans
#' @export
log10_sci_trans <- function(n = 5L, n_small = 30L, format = scales::format_format()) {
    trans <- function(x) log10(x)
    inv <- function(x) 10 ^ x
    trans_new("log10_sci", trans, inv,
              breaks = simple_log10_breaks(n = n),
              minor_breaks = simple_log10_minor_breaks(n = n_small),
              format = format,
              domain = vctrs::vec_c(1e-300, Inf))
}

#' @rdname trans
#' @export
reverse_sci_trans <- function(
    n = 5L, modifier = vctrs::vec_c(1, 2, 2.5, 5), n_small = 50L,
    format = scales:::format_format()) {
    trans_new("identity_sci", function(x) -x, function(x) -x,
              breaks = simple_breaks(n = n, modifier = modifier),
              minor_breaks = simple_minor_breaks(n = n_small),
              format = format)
}