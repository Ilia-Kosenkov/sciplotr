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

    by * vctrs::vec_c(floor(rng[1] / by), ceiling(rng[2] / by))
}

name_filler <- function() ""

labels_filler <- function() function(x) vctrs::vec_recycle("", vctrs::vec_size(x))

facet_labeller <- function(modifier = NULL) {
    if (rlang::is_null(modifier))
        return(ggplot2::label_value)

    mod <- rlang::as_function(modifier)

    function(labels, multi_line = TRUE) {
        mod(labels)
    }
}