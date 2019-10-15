identity_sci_trans <- function(n = 5L, modifier = vctrs::vec_c(1, 2, 2.5, 5), n_small = 50L) {
    trans_new("identity_sci", "force", "force",
              breaks = simple_breaks(n = n, modifier = modifier),
              minor_breaks = simple_minor_breaks(n = n_small))
}

log10_sci_trans <- function() {
    trans <- function(x) log10(x)
    inv <- function(x) 10 ^ x
    trans_new("log10_sci", trans, inv,
              breaks = simple_log10_breaks(),
              minor_breaks = simple_log10_minor_breaks(),
              domain = vctrs::vec_c(1e-300, Inf))
}

