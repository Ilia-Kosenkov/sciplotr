log10_floor <- function(x) 10 ^ floor(log10(x))

unique_f <- function(x, eps = 1L) {
    x <- vec_cast(x, double())

    prod <- outer(x, x, are_equal_f, eps = eps)

    purrr::map_int(vctrs::vec_seq_along(x), ~ sum(prod[1:.x, .x])) %>%
        `%==%`(1L) %>%
        which -> inds
    x[inds]
}

outer_unique_which <- function(x, y, eps = 1L) {
    x <- vec_cast(x, double())
    y <- vec_cast(y, double())

    prod <- !outer(x, y, are_equal_f, eps = eps)
    list(x = which(apply(prod, 1, all)), y = which(apply(prod, 2, all)))
}

outer_unique <- function(x, y, eps = 1L) {
    ids <- outer_unique_which(x, y, eps)
    list(x = x[ids$x], y = y[ids$y])
}

round_interval <- function(rng, by) {
    rng <- vec_cast(rng, double())
    by <- vec_assert(vec_cast(by, double), size = 1L)

    by * cc(floor(rng[1] / by), ceiling(rng[2] / by))
}




#lin <- function(x, x0, y0) {
    #dx <- diff(x0)
    #dy <- diff(y0)
    #sz <- len(x)
    #if (sz %==% 0L)
        #return(x)
    #else if (sz %==% 1L)
        #(x - x0[1]) * dy / dx + y0[1]
    #else
        #purrr::map_dbl(x, ~ (.x - x0[1]) * dy / dx + y0[1])
#}

utils::globalVariables(c("id_l"))

locate_inrange <- function(x, range) {
    test <-
        if (range[1] < range[2])
            function(x, l, r)
                x >= l & x < r
        else
            function(x, l, r)
                x <= l & x > r

    tibble::tibble(l = range, r = dplyr::lead(range)) %>%
        dplyr::mutate(id_l = 1L:n(), id_r = id_l + 1L) %>%
        dplyr::slice(-n()) -> data

    purrr::map(x,
        ~ dplyr::filter(data, test(.x, l, r)) %>%
            dplyr::select(id_l, id_r) %>%
            dplyr::slice(1L) %>%
            purrr::flatten_int %>%
            unname)
}

df_grid <- function(rows, cols, margin = FALSE) {
    vars <- purrr::map(
        append(
            purrr::map(rows, unique),
            purrr::map(cols, unique)),
        forcats::as_factor)

    if (margin)
        vars <- purrr::map(vars, cc, forcats::as_factor("(all)"))

    tidyr::expand_grid(!!!vars)
}

get_id <- function(.variables) {

    lengths <- purrr::map_int(.variables, len)
    .vars <- .variables[lengths %!=% 0L]
    vars_len <- len(.vars)

    if (vars_len %==% 0L) {
        n <- len(.variables) %||% 0L
        return(structure(seq_len(n), n = n))
    }
    if (vars_len %==% 1L) {
        return(get_id_var(.vars[[1]]))
    }
    ids <- rev(purrr::map(.variables, get_id_var))
    p <- len(ids)

    ndistinct <- purrr::map_int(ids, attr, "n")
    n <- prod(ndistinct)

    combs <- cc(1, cumprod(ndistinct[-p]))
    mat <- rlang::exec(cbind, !!!ids)
    res <- as.vector((mat - 1L) %*% combs + 1L)
    attr(res, "n") <- n

    get_id_var(res)
}

get_id_var <- function(x) {
    if (length(x) == 0)
        return(structure(integer(), n = 0L))
    levels <- sort(unique(x), na.last = TRUE)
    id <- match(x, levels)
    n <- max(id)
    structure(id, n = n)
}

adjust_angle <- function(x, by = 180) {
    for (i in seq_len(length(x$children))) {
        x$children[[i]]$rot <- x$children[[i]]$rot + by
    }
    x
}

#`%vec_in%` <- vctrs::vec_in
#cc <- vctrs::vec_c

### Temporary solution
##' @export
#len <- function(x) UseMethod("len")
##' @export
#len.default <- vctrs::vec_size
##' @export
#len.unit <- length

split_ex <- function(.data, col, name = NULL, keep = FALSE) {
    content <- dplyr::pull(.data, {{ col }})
    content <- vctrs::vec_recycle_common(!!!content)
    size <- vctrs::vec_size_common(!!!content)
    transposed <- purrr::map(seq_len(size), ~ purrr::map(content, .x))
    result <- purrr::map(transposed, ~ vec_c(!!!.x))

    if (rlang::is_null(name) || rlang::is_empty(name))
        names <- paste0("Split_", seq_len(size))
    else
        names <- paste0(name, "_", seq_len(size))

    result <- dplyr::bind_cols(rlang::set_names(result, names))

    if (!keep)
        .data <- dplyr::select(.data, - {{ col }})

    dplyr::bind_cols(.data, result)
}

#' @export
empty_labels <- function() {
    empty_seq
}

empty_seq <- function(x)
    vctrs::vec_repeat(" ", len(x))

#' @export
lin_unit <- function(x0, x, y) {
    x0 <- vec_cast(x0, double())
    x <- vec_assert(vec_cast(x, double()), size = 2L)
    assertthat::assert_that(len(y) %===% 2L)

    dx <- x[2] - x[1]
    dy <- y[2] - y[1]

    purrr::map(x0, ~ y[1] + dy / dx * (.x - x[1])) -> result
    if (grid::is.unit(y))
        rlang::exec(grid::unit.c, !!!result)
    else
        vctrs::vec_cast(result, vctrs::vec_ptype_common(!!!result))
}