#' Unit division
#'
#' @param e1 \code{unit} on the lhs.
#' @param e2 Number on the rhs.
#'
#' @return Modified \code{unit}
#' @export
`/.unit` <- function(e1, e2) {
    assertthat::assert_that(is.unit(e1))

    e2 <- vctrs::vec_cast(e2, double(), x_arg = "e2", to_arg = "")
    vctrs::vec_assert(e2, double(), size = 1L)
    e1 * (1 / e2)
}

`-.unit` <- function(e1, e2) {
    assertthat::assert_that(grid::is.unit(e1))
    if (missing(e2))
        return(-1 * e1)
    assertthat::assert_that(grid::is.unit(e2))
    grid:::unit.arithmetic("-", e1, e2)
}

f_u_ <- function(x, .data = NULL) {
    assert_that(is_formula(x))
    val <- vctrs::vec_cast(eval_tidy(f_lhs(x)), double(), to_arg = "")
    vec_assert(val, size = 1L)

    unit <- f_rhs(x)
    assert_that(rlang::is_symbol(unit) || rlang::is_string(unit))
    grid::unit(val, as.character(unit), data = .data)
}

u_ <- function(..., .data = NULL) {
    args <- rlang::enquos(...)
    `$` <- function(e1, e2) {
        if (vctrs::vec_is(e1, double(), 1L) || vctrs::vec_is(e1, integer(), 1L)) {
            unit <- as.character(rlang::ensym(e2))
            return(grid::unit(e1, unit, data = .data))
        }

        return(eval_tidy(quo(.Primitive("$")(e1, !!ensym(e2)))))
    }


    result <- lapply(args, function(arg) {
        if (is_formula(quo_squash(arg)))
            f_u_(quo_squash(arg), .data = .data)
        else
            rlang::eval_tidy(arg, data = list(`$` = `$`))
        })

    rlang::exec(grid::unit.c, !!!result)
}

npc_ <- function(x) u_(x$npc)
cm_ <- function(x) u_(x$cm)
in_ <- function(x) u_(x$`in`)
pt_ <- function(x) u_(x$pt)


#' @title mar_
#' @description Creates a new instance of margin. Supports unit arithmetics
#' @param ... Units to construct margin
#' @return An instance of \code{margin}
#' @export
mar_ <- function(...) {
    args <- rlang::enquos(...)
    args <- purrr::map(args, u_)
    has_no_names <- rlang::is_null(names(args)) ||
        all(!nzchar(names(args)))

    if (vctrs::vec_size(args) == 1L && has_no_names) {
        unit <- args[[1]]
        len <- length(unit)
        if (len == 1L)
            margin <- rep(unit, 4L)
        else if (len == 2)
            margin <- rep(unit, 2L)
        else if (len == 4)
            margin <- unit
        else
            stop(sprintf("Single unit argument should have lengths of 1, 2 or 4, but input vector has size %d", length(unit)))
        }
    else if (vctrs::vec_size(args) == 4L &&  has_no_names) {
        margin <- grid::unit.c(args[[1]], args[[2]], args[[3]], args[[4]])
    }
    else if (!has_no_names) {
        names(args) <-
            purrr::map_chr(names(args), match.arg,
                vctrs::vec_c("top", "right", "bottom", "left", "horizontal", "vertical"))
        purrr::walk(args, ~ assertthat::assert_that(grid::is.unit(.x), length(.x) == 1L))

        has_vertical <- vctrs::vec_in("vertical", names(args))
        has_horizontal <- vctrs::vec_in("horizontal", names(args))

        if (rlang::is_null(args$top))
            args$top <- if (has_vertical) args$vertical else grid::unit(0, "null")
        if (rlang::is_null(args$bottom))
            args$bottom <- if (has_vertical) args$vertical else grid::unit(0, "null")

        if (rlang::is_null(args$left))
            args$left <- if (has_horizontal) args$horizontal else grid::unit(0, "null")
        if (rlang::is_null(args$right))
            args$right <- if (has_horizontal) args$horizontal else grid::unit(0, "null")

        margin <- grid::unit.c(args$top, args$right, args$bottom, args$left)
    }
    else
        stop("Invalid input")

    class(margin) <- vctrs::vec_c("margin", class(margin))
    margin
}

at_ <- function(item, what) UseMethod("at_")
`at_<-` <- function(item, what, value) UseMethod("at_<-")


at_.margin <- function(mar, what) {
    what <- as.character(ensym(what))

    what <- match.arg(what, vec_c("top", "right", "bottom", "left"))
    pos <- switch(what,
            "top" = 1L,
            "right" = 2L,
            "bottom" = 3L,
            "left" = 4L)
    mar[pos]
}

`at_<-.margin` <- function(mar, what, value) {
    what <- ensym(what)

    with_mar(mar, !!what := ~value)
}

with_mar <- function(mar, ...) {
    args <- list2(...)
    names <- names(args)
    assertthat::assert_that(!rlang::is_null(names) && all(nzchar(names)))

    names <- map(names(args), match.arg, vec_c("top", "right", "bottom", "left"))

    names(args) <- names

    args %>%
        map(as_function) %>%
        imap(function(item, name) {
            item(at_(mar, !!name))
        }) -> new_mar

    vec_c("top", "right", "bottom", "left") %>%
        rlang::set_names(.) %>%
        map(~if (is_null(new_mar[[.x]])) at_(mar, !!.x) else new_mar[[.x]]) -> new_mar

    mar_(unit.c(new_mar$top, new_mar$right, new_mar$bottom, new_mar$left))
}

outermost_op <- function(x)  UseMethod("outermost_op")

outermost_op.unit <- function(x) NA_character_

outermost_op.unit.list <- function(x) {
    map_chr(x, ~ ifelse(inherits(.x, "unit.arithmetic"), .x$fname, NA_character_))
}

outermost_op.unit.arithmetic <- function(x) {
    vec_repeat(x$fname, length(x))
}

custom_format <- function(x) {
    UseMethod("custom_format")
}

custom_format.unit <- function(x) {
    grid:::as.character.unit(x)
}

custom_format.unit.list <- function(x) {
    map_chr(x, custom_format)
}

custom_format.unit.arithmetic <- function(x) {
    f_name <- x$fname

    if (vec_in(f_name, vec_c("+", "-"))) {
        paste(custom_format(x$arg1), f_name, custom_format(x$arg2), sep = " ")
    }
    else if (f_name == "*") {
        inner_ops <- outermost_op(x$arg2)
        map2_chr(custom_format(x$arg2), inner_ops,
            ~ ifelse(is.na(.y), paste(x$arg1, "*", .x), paste0(x$arg1, " * ", "(", .x, ")")))
    }
    else {
        paste0(f_name, "(", paste(custom_format(x$arg1), collapse = ", "), ")")
    }
}

as.character.unit.arithmetic <- function(x, ...) {
    custom_format(x)
}

as.character.unit.list <- function(x, ...) {
    custom_format(x)
}

print.unit.list <- function(x, ...) {

    print(as.character(x), quote = FALSE, ...)
}

print.unit.arithmetic <- function(x, ...) {
    print(as.character(x), quote = FALSE, ...)
}

as_list_unit <- function(x) {
    assertthat::assert_that(inherits(x, "unit"))
    len <- length(x)
    `class<-`(purrr::map(seq_len(len), ~ x[.x]), vctrs::vec_c("unit.list", "unit"))
}

flatten_unit <- function(x) {
    if (inherits_only(x, "list"))
        result <- map(x, flatten_unit)
    else if (inherits_any(x, "unit.list"))
        result <- map(x, flatten_unit)
    else if (inherits_any(x, "unit"))
        result <- as_list_unit(x)
    else
        stop("Invalid input type")
    flatten(result)
}

unit_max <- function(..., .item_wise = FALSE) {
    vctrs::vec_assert(.item_wise, logical(), 1L)

    if (.item_wise)
        return(grid::unit.pmax(...))

    args <- rlang::list2(...)
    units <- flatten_unit(args)

    rlang::exec(grid::unit.pmax, !!!units)
}

unit_min <- function(..., .item_wise = FALSE) {
    vctrs::vec_assert(.item_wise, logical(), 1L)

    if (.item_wise)
        return(grid::unit.pmin(...))

    args <- rlang::list2(...)
    units <- flatten_unit(args)

    rlang::exec(grid::unit.pmin, !!!units)
}