
utils::globalVariables(c(".label", ".state"))
# https://github.com/tidyverse/ggplot2/blob/fa000f786cb0b641600b6de68ae0f96e2ffc5e75/R/guides-axis.r#L64
guide_train.axis <- function(guide, scale, aesthetic = NULL) {

    aesthetic <- aesthetic %||% scale$aesthetics[1]
    breaks <- scale$get_breaks()

    ## WATCH: adding minor breaks
    breaks_minor <- scale$get_breaks_minor()
    breaks_minor <- breaks_minor[outer_unique_which(breaks_minor, breaks)$x]

    empty_ticks <- vctrs::new_data_frame(
        list(aesthetic = numeric(0), .value = numeric(0), .label = character(0)))

    names(empty_ticks) <- c(aesthetic, ".value", ".label")

    if (length(intersect(scale$aesthetics, guide$available_aes)) == 0) {
        warning(
            "axis guide needs appropriate scales: ",
            paste(guide$available_aes, collapse = ", "),
            call. = FALSE)
        guide$key <- empty_ticks
    }
    else if (length(breaks) == 0)
        guide$key <- empty_ticks
    else {
        ticks <- vctrs::new_data_frame(rlang::set_names(list(scale$map(breaks)), aesthetic))
        ticks$.value <- breaks
        ticks$.label <- scale$get_labels(breaks)

        lims <- scale$continuous_range %||% scale$get_limits()


        ## Addung minor ticks to the existing set
        ticks$.type <- "major"
        df <-
            new_data_frame(
                rlang::set_names(
                    list(
                        scale$map(breaks_minor),
                        breaks_minor,
                        vctrs::vec_repeat("", vctrs::vec_size(breaks_minor)),
                        vctrs::vec_repeat("minor", vctrs::vec_size(breaks_minor))),
                    names(ticks)))

        ticks <- vctrs::vec_rbind(ticks, df)

        if (is.list(ticks$.label)) {
            if (any(sapply(ticks$.label, is.language)))
                ticks$.label <- do.call(expression, ticks$.label)
            else
                ticks$.label <- unlist(ticks$.label)
        }

        guide$key <- ticks
    }

    guide$name <- paste0(guide$name, "_", aesthetic)
    guide$hash <- digest::digest(list(guide$title, guide$key$.value, guide$key$.label, guide$key$.type, guide$name))
    guide
}

guide_axis_censor <- function(guide, scale, axis_end_offset = c(0.05, 0.05)) {
    if (rlang::is_null(guide$key))
        return(guide)
    axis_end_offset <- vctrs::vec_recycle(vctrs::vec_cast(axis_end_offset %||% 0.05, double()), size = 2L)

    lims <- scale$continuous_range %||% scale$get_limits()

    delta <- abs(diff(lims)) * axis_end_offset

    idx <- (abs(guide$key$.value - lims[1]) < delta[1]) | (abs(guide$key$.value - lims[2]) < delta[2])
    guide$key <- dplyr::mutate(guide$key, .label = dplyr::if_else(idx, " ", .label))
    guide$hash <- digest::digest(list(guide$title, guide$key$.value, guide$key$.label, guide$key$.type, guide$name))
    guide
}