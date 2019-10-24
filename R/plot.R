# https://github.com/tidyverse/ggplot2/blob/c84d9a075280d374892e5a3e0e25dd0ba246caad/R/plot.r#L80
ggplot_sci <- function(
    data = NULL, mapping = aes(), ..., environment = parent.frame()) {

    if (rlang::is_missing(mapping) && !rlang::inherits_any(mapping, "uneval"))
        rlang::abort("Mapping should be created with `aes() or `aes_()`.", .subclass = "sciplotr_invalid_arg")

    data <- ggplot2::fortify(data, ...)

    p <- structure(list(
        data = data,
        layers = list(),
        scales = ggplot2:::scales_list(),
        mapping = mapping,
        theme = theme_sci(),
        coordinates = coord_sci(default = TRUE),
        facet = ggplot2:::facet_null(),
        plot_env = environment
    ), class = c("gg", "ggplot"))

    p$labels <- ggplot2:::make_labels(mapping)

    ggplot2::set_last_plot(p)
    p
}