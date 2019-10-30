are_equal_f <- function(x, y, eps = 1) {
    vctrs::vec_assert(eps, size = 1L)
    assertthat::assert_that(eps > 0)

    vec_assert_numeric(x, arg = name_of(x))
    vec_assert_numeric(y, arg = name_of(y))

    rec <- vctrs::vec_recycle_common(x = x, y = y)

    x <- rec$x
    y <- rec$y

    comparator <- function(p, q) {
        if (is.na(p) || is.na(q))
            return(FALSE)

        if (is.infinite(p) || is.infinite(q))
            return(p == q)

        if (p == q)
            return(TRUE)

        p_abs <- abs(p)
        q_abs <- abs(q)
        diff <- abs(p - q)

        delta <- eps * .Machine$double.eps
        # According to IEEE-754 https://en.wikipedia.org/wiki/IEEE_754
        # -0 and 0 are equal, therefore p_abs and q_abs
        # cannot be 0 at the same time
        if (p_abs == 0 && q_abs == 0)
            rlang::abort("Should not happen", "impossible_exception", trace = rlang::trace_back())
        else if (p_abs == 0)
            fact <- q_abs
        else if (q_abs == 0)
            fact <- p_abs
        else
            fact <- min(vctrs::vec_c(p_abs, q_abs))

        return(diff < fact * delta)
    }

    purrr::map2_lgl(x, y, comparator)
}

`%==%` <- function(e1, e2) {
    vctrs::vec_recycle_common(!!!vctrs::vec_cast_common(e1, e2)) %->% c(x, y)
    if (vctrs::vec_is(x, complex()))
        return(are_equal_f(Re(x), Re(y)) & are_equal_f(Im(x), Im(y)))
    if (vctrs::vec_is(x, double()))
        return(are_equal_f(x, y))
    return(x == y)
}

`%!=%` <- function(e1, e2) {
    !(e1 %==% e2)
}

are_same_all <- function(x, eps = 1) {
    # `eps` is tested in `are_equal_f`
    vec_assert_numeric(x, arg = name_of(x))

    if (vec_size(x) == 0L)
        return(FALSE)
    if (vec_size(x) == 1L)
        return(TRUE)

    all(are_equal_f(vctrs::vec_slice(x, 1L), vctrs::vec_slice(x, 2L:vctrs::vec_size(x)), eps = eps))

}

name_of <- function(x) {
    y <- enexpr(x)
}

vec_assert_numeric <- function(x, size = NULL, arg = rlang::as_label(substitute(x))) {
    if (!vctrs::vec_is(x, integer(), size = size) && !vctrs::vec_is(x, double(), size = size))
        vctrs::stop_incompatible_type(x, numeric(), x_arg = arg)

    invisible(vec_cast(x, double(), x_arg = rlang::as_label(x)))
}

vec_cast_integerish <- function(x, x_arg = "x") {
    if (vec_is(x, integer()))
        return(x)

    if (vec_is(x, double())) {
        diffs <- (abs(x) - floor(abs(x))) %==% 0
        inds <- which(!diffs)
        if (vec_is_empty(inds))
            return(vctrs::allow_lossy_cast(vec_cast(x, integer(), x_arg = rlang::as_label(x))))

        vctrs::stop_incompatible_cast(x[inds[1]], integer(), x_arg = rlang::as_label(x[inds[1]]))
    }
}

vec_assert_integerish <- function(x, size = NULL, x_arg = "x") {
    if (vec_is(x, integer()))
        result <- x

    if (vec_is(x, double())) {
        diffs <- (abs(x) - floor(abs(x))) %==% 0
        inds <- which(!diffs)
        if (vec_is_empty(inds))
            result <- vctrs::allow_lossy_cast(vec_cast(x, integer(), x_arg = rlang::as_label(x)))
        else
            vctrs::stop_incompatible_cast(x[inds[1]], integer(), x_arg = rlang::as_label(x[inds[1]]))
    }
    if (!rlang::is_null(size)) {
        vctrs::vec_assert(result, size = size)
    }

    invisible(result)
}

log10_floor <- function(x) 10 ^ floor(log10(x))

unique_f <- function(x, eps = 1L) {
    x <- vec_assert_numeric(x)

    prod <- outer(x, x, are_equal_f, eps = eps)

    purrr::map_int(vctrs::vec_seq_along(x), ~ sum(prod[1:.x, .x])) %>%
        `%==%`(1L) %>%
        which -> inds
    x[inds]
}

outer_unique_which <- function(x, y, eps = 1L) {
    x <- vec_assert_numeric(x)
    y <- vec_assert_numeric(y)

    prod <- !outer(x, y, are_equal_f, eps = eps)
    list(x = which(apply(prod, 1, all)), y = which(apply(prod, 2, all)))
}

outer_unique <- function(x, y, eps = 1L) {
    ids <- outer_unique_which(x, y, eps)
    list(x = x[ids$x], y = y[ids$y])
}

round_interval <- function(rng, by) {
    rng <- vec_assert_numeric(rng)
    by <- vec_assert_numeric(by, size = 1L)

    by * cc(floor(rng[1] / by), ceiling(rng[2] / by))
}




lin <- function(x, x0, y0) {
    dx <- diff(x0)
    dy <- diff(y0)
    sz <- len(x)
    if (sz %==% 0L)
        return(x)
    else if (sz %==% 1L)
        (x - x0[1]) * dy / dx + y0[1]
    else
        purrr::map_dbl(x, ~ (.x - x0[1]) * dy / dx + y0[1])
}


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
            magrittr::extract(1, cc("id_l", "id_r")) %>%
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

adjust_angle <- function(x) {
    x$angle <- ggplot2:::adjust_angle(x$angle)
    x
}

`%vec_in%` <- vctrs::vec_in
cc <- vctrs::vec_c

len <- function(x) UseMethod("len")
len.default <- vctrs::vec_size
len.unit <- length

split_ex <- function(.data, col, name = NULL, keep = FALSE) {
    content <- dplyr::pull(.data, {{ col }})
    content <- vctrs::vec_recycle_common(!!!content)
    size <- vctrs::vec_size_common(!!!content)
    transposed <- purrr::map(seq_len(size), ~ purrr::map(content, .x))
    result <- purrr::map(transposed, ~ vctrs::vec_cast(.x, vctrs::vec_ptype_common(!!!.x)))

    if (rlang::is_null(name) || rlang::is_empty(name))
        names <- paste0("Split_", seq_len(size))
    else
        names <- paste0(name, "_", seq_len(size))

    result <- dplyr::bind_cols(rlang::set_names(result, names))

    if (!keep)
        .data <- dplyr::select(.data, - {{ col }})

    dplyr::bind_cols(.data, result)
}