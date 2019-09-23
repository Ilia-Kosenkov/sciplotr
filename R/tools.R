are_equal_f <- function(x, y, eps = 1) {
    vctrs::vec_assert(eps, size = 1L)
    assertthat::assert_that(eps > 0)

    vec_allow_numeric(x, arg = name_of(x))
    vec_allow_numeric(y, arg = name_of(y))

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
        if (p_abs < delta || q_abs < delta || diff < delta)
            return(diff < delta)

        return((diff / min(vctrs::vec_c(p_abs, q_abs, 1))) < delta)
    }

    purrr::map2_lgl(x, y, comparator)
}

are_same_all <- function(x, eps = 1) {
    # `eps` is tested in `are_equal_f`
    vec_allow_numeric(x, arg = name_of(x))

    if (vec_size(x) == 0L)
        return(FALSE)
    if (vec_size(x) == 1L)
        return(TRUE)

    all(are_equal_f(vctrs::vec_slice(x, 1L), vctrs::vec_slice(x, 2L:vctrs::vec_size(x)), eps = eps))

}

name_of <- function(x) {
    y <- enexpr(x)
}

vec_allow_numeric <- function(x, size = NULL, arg = rlang::as_label(substitute(x))) {
    if (!vctrs::vec_is(x, integer(), size = size) && !vctrs::vec_is(x, double(), size = size))
        vctrs::stop_incompatible_type(x, numeric(), x_arg = arg)
}