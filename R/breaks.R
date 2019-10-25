simple_breaks <- function(n = 5, modifier = vctrs::vec_c(1, 2, 2.5, 5), ...) {
    function(x) {
        x <- x[is.finite(x)]
        if (vec_is_empty(x)) {
            return(numeric())
        }
        rng <- range(x)
        generate_simple_breaks(rng, fancy_step(rng, n, modifier))
    }
}

simple_minor_breaks <- function(n = 50L) {
    function(b, limits, m) {
        generate_simple_minor_breaks(b, limits, n = n)
    }
}

simple_log10_breaks <- function(n = 5L) {
    function(x) {
        x <- x[is.finite(x)]
        if (vec_is_empty(x)) {
            return(numeric())
        }
        rng <- range(x)
        generate_simple_log10_breaks(rng, n)
    }
}

simple_log10_minor_breaks <- function(n = 30L) {
    function(b, limits, m) {
        generate_simple_log10_minor_breaks(b, limits, n = n)
    }
}

fancy_step <- function(range, n = 6, modifier = c(1, 2, 2.5, 5)) {
    modifier <- c(0.1 * modifier, modifier)

    large_steps <- 10 ^ floor(log10(abs(diff(range))))

    mod_ind <- abs(abs(diff(range)) / (modifier * large_steps) - n)
    mod_ind <- which(are_equal_f(mod_ind, min(mod_ind)))

    min(large_steps * modifier[mod_ind])
}

generate_simple_breaks <- function(range, step = fancy_step(range, n = 6L, modifier = vctrs::vec_c(1, 2, 2.5, 5))) {

    step * (ceiling(range[1] / step):floor(range[2] / step))
}

generate_simple_minor_breaks <- function(breaks, limits, n = 40L) {
    if (vctrs::vec_size(breaks) < 2L)
        return(double(0))

    diffs <- diff(breaks)
    # Temporarily ignore this
    #if (!are_same_all(diffs, eps = 1))
    ## Probably can handle this case also
    ##stop("Unequally spaced major breaks")
    #print(RLibs::glue_fmt("{diffs:%26.16e}"))


    df <- diffs[1L]

    digit <- round(df / log10_floor(df))

    modifier <- vec_c(1, 2, 2.5, 5)

    if (are_equal_f(digit, 2L))
        modifier <- vec_c(1, 5)
    if (are_equal_f(digit, 3L))
        modifier <- vec_c(1, 3)
    if (are_equal_f(digit, 5L))
        modifier <- vec_c(1, 2.5, 5)

    modifier <- vec_c(0.1 * modifier, modifier, 10 * modifier)

    step <- 0.1 * log10_floor(df)

    extra_rng <- vec_c(min(breaks) - df, max(breaks) + df)
    diff(extra_rng) / (step * modifier)

    sizes <- abs(diff(extra_rng) / (step * modifier) - n)

    ind <- which(are_equal_f(sizes, min(sizes)))

    small_breaks <- generate_simple_breaks(vec_c(0, df), modifier[ind] * step)

    extended_breaks <-
        purrr::map(vec_c(min(breaks) - df, breaks, max(breaks) + df), ~ .x + small_breaks) %>%
        purrr::flatten_dbl %>%
        unique_f

    extended_breaks <- outer_unique(extended_breaks, breaks)$x

    extended_breaks[extended_breaks >= limits[1] & extended_breaks <= limits[2]]
}

generate_simple_log10_breaks <- function(lim, n = 5L) {
    tick_set <- list(
    #vctrs::vec_c(0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50),
                    vctrs::vec_c(0.1, 0.5, 1, 5, 10, 50),
                    vctrs::vec_c(0.1, 1, 10))
    if (diff(log10(lim)) <= 1L) {
        mult <- log10_floor(min(lim))
        y_dig <- lim / mult
        step <- fancy_step(y_dig, n = n, modifier = scales::discard(tick_set[[1]], vctrs::vec_c(1, 10)))

        breaks <- mult * generate_simple_breaks(y_dig, step)
    }
    else {
        breaks <- 10 ^ generate_simple_breaks(log10(lim), 1)
        get_breaks <- function(tcks)
            purrr::map(breaks, ~ tcks * .x) %>%
                purrr::flatten_dbl %>%
                    scales::discard(lim) %>%
                        unique_f(eps = 1)

        breaks <- purrr::map(tick_set, get_breaks)
        delta_lens <- abs(purrr::map_int(breaks, vctrs::vec_size) - n)
        id <- which(are_equal_f(delta_lens, min(delta_lens)))

        breaks <- breaks[[id]]
    }
    return(breaks)
}

generate_simple_log10_minor_breaks <- function(brs, lim, n = 30L) {
    brs <- 10 ^ brs
    lim <- 10 ^ lim
    tick_set <- list(
                    vctrs::vec_c(0.1 * (1:9), 1:9, 10 * (1:9)),
                    vctrs::vec_c(0.1, 0.5, 1, 5, 10, 50),
                    vctrs::vec_c(0.1, 1, 10))

    if (diff(log10(lim)) <= 1L) {
        mult <- log10_floor(min(lim))
        y_dig <- lim / mult
        breaks <- mult * (generate_simple_minor_breaks(brs / mult, y_dig, n = n))
    }
    else {
        brs_2 <- unique_f(log10_floor(brs))
        get_breaks <- function(tcks)
            purrr::map(brs_2, ~ tcks * .x) %>%
                purrr::flatten_dbl %>%
                    scales::discard(lim) %>%
                        unique_f

        breaks <- purrr::map(tick_set, get_breaks)
        delta_lens <- abs(purrr::map_int(breaks, vctrs::vec_size) - n)
        id <- which(are_equal_f(delta_lens, min(delta_lens)))

        breaks <- breaks[[id]]
        breaks <- breaks[outer_unique_which(brs, breaks)$y]
    }
    breaks <- log10(breaks)
    return(breaks)
}


new_breaks <- function(breaks, minor_breaks, name) {
    vctrs::vec_assert(name, character(), 1L)
    assertthat::assert_that(nzchar(name))

    breaks <- rlang::as_function(breaks)
    minor_breaks <- rlang::as_function(minor_breaks)

    structure(list(get_breaks = breaks, get_minor_breaks = minor_breaks, name = name), class = "breaks_gen")
}

print.breaks_gen <- function(x, ...) {
    cat("Breaks generator: ", x$name, "\n")
}
