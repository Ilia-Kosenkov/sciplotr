identity_sci_trans <- function(n = 5, modifier = vctrs::vec_c(1, 2, 2.5, 5), n_small = 100L) {

    trans_new("identity_sci", "force", "force",
              breaks = simple_breaks(n = n, modifier = modifier),
              minor_breaks = simple_minor_breaks(n = n_small))
}

simple_breaks <- function(n = 5, modifier = vctrs::vec_c(1, 2, 2.5, 5), ...) {
    function(x) {
        x <- x[is.finite(x)]
        if (vec_is_empty(x)) {
            return(numeric())
        }
        rng <- range(x)
        generate_simple_breaks(rng, fancy_step(rng, n, modifier)) %>% print
    }
}

simple_minor_breaks <- function(n = 100L) {
    function(b, limits, m) {
        generate_simple_minor_breaks(b, limits, n = n) %>% print
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
        return(vctrs::new_double(0))

    diffs <- diff(breaks)
    if (!are_same_all(diffs))
        # Probably can handle this case also
        stop("Unequally spaced major breaks")

    df <- diffs[1L]

    digit <- df / log10_floor(df)

    modifier <- vec_c(1, 2, 2.5, 5)

    if (digit == 2L)
        modifier <- vec_c(1, 5)
    if (digit == 3L)
        modifier <- vec_c(1, 3)
    if (digit == 5L)
        modifier <- vec_c(1, 2.5, 5)

    modifier <- vec_c(0.1 * modifier, modifier, 10 * modifier)

    step <- 0.1 * df

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

