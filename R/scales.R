
scale_x_sci <- function(name = waiver(), breaks = waiver(), minor_breaks = waiver(),
    labels = waiver(), limits = NULL, expand = waiver(), oob = censor,
    na.value = NA_real_, position = "bottom",
    sec.axis = waiver(),
    breaks_n = 3,
    breaks_modifiers = vctrs::vec_c(1, 2, 2.5, 5),
    minor_breaks_n = 50L) {

    breaks_n <- vec_assert_integerish(breaks_n, size = 1L)
    breaks_modifiers <- vec_assert_numeric(breaks_modifiers)

    scale_x_continuous(name = name, breaks = breaks, minor_breaks = minor_breaks,
        labels = labels, limits = NULL, expand = expand, oob = oob,
        na.value = na.value, trans = identity_sci_trans(breaks_n, breaks_modifiers, minor_breaks_n),
        position = position,
        sec.axis = sec.axis)
}

scale_y_sci <- function(name = waiver(), breaks = waiver(), minor_breaks = waiver(),
    labels = waiver(), limits = NULL, expand = waiver(), oob = censor,
    na.value = NA_real_, position = "left",
    sec.axis = waiver(),
    breaks_n = 3,
    breaks_modifiers = vctrs::vec_c(1, 2, 2.5, 5),
    minor_breaks_n = 50L) {

    breaks_n <- vec_assert_integerish(breaks_n, size = 1L)
    breaks_modifiers <- vec_assert_numeric(breaks_modifiers)

    scale_y_continuous(name = name, breaks = breaks, minor_breaks = minor_breaks,
        labels = labels, limits = NULL, expand = expand, oob = oob,
        na.value = na.value, trans = identity_sci_trans(breaks_n, breaks_modifiers, minor_breaks_n),
        position = position,
        sec.axis = sec.axis)
}


scale_x_log10_sci <- function(name = waiver(), breaks = waiver(), minor_breaks = waiver(),
    labels = waiver(), limits = NULL, expand = waiver(), oob = censor,
    na.value = NA_real_, position = "bottom",
    sec.axis = waiver()) {

    scale_x_continuous(name = name, breaks = breaks, minor_breaks = minor_breaks,
        labels = labels, limits = NULL, expand = expand, oob = oob,
        na.value = na.value,
        trans = log10_sci_trans(),
        position = position,
        sec.axis = sec.axis)
}

scale_y_log10_sci <- function(name = waiver(), breaks = waiver(), minor_breaks = waiver(),
    labels = waiver(), limits = NULL, expand = waiver(), oob = censor,
    na.value = NA_real_, position = "left",
    sec.axis = waiver()) {

    scale_y_continuous(name = name, breaks = breaks, minor_breaks = minor_breaks,
        labels = labels, limits = NULL, expand = expand, oob = oob,
        na.value = na.value,
        trans = log10_sci_trans(),
        position = position,
        sec.axis = sec.axis)
}