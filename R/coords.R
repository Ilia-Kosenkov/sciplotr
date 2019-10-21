CoordSci <-
    ggproto("CoordSci", CoordCartesian,
        # https://github.com/tidyverse/ggplot2/blob/115c3960d0fd068f1ca4cfe4650c0e0474aabba5/R/coord-cartesian-.r#L99
        setup_panel_params = function(self, scale_x, scale_y, params = list()) {
            append(ggproto_parent(CoordCartesian, self)$setup_panel_params(scale_x, scale_y, params),
                   list(ticks_minor_size_f = self$ticks_minor_size_f))
            #c(view_scales_from_scale(scale_x, self$limits$x, self$expand),
              #view_scales_from_scale(scale_y, self$limits$y, self$expand),
              #list(ticks_minor_size_f = self$ticks_minor_size_f))
        } ,
        # https://github.com/tidyverse/ggplot2/blob/23e324197e0a5ddd764588d42838b0d96da9b68d/R/coord-cartesian-.r#L116
        #render_axis_h = function(panel_params, theme) {
            #arrange <- panel_params$x.arrange %||% c("secondary", "primary")
            #arrange_scale_keys <- c(primary = "x", secondary = "x.sec")[arrange]
            #arrange_scales <- panel_params[arrange_scale_keys]

            #ticks_minor_size_f <- panel_params$ticks_minor_size_f
            
            #list(
                 #top = draw_view_scale_axis(arrange_scales[[1]], "top", theme, ticks_minor_size_f),
                 #bottom = draw_view_scale_axis(arrange_scales[[2]], "bottom", theme, ticks_minor_size_f))
        #},
        # https://github.com/tidyverse/ggplot2/blob/115c3960d0fd068f1ca4cfe4650c0e0474aabba5/R/coord-cartesian-.r#L185
        render_axis_h = function(panel_params, theme) {
            list(
              top = panel_guides_grob(panel_params$guides, position = "top", theme = theme),
              bottom = panel_guides_grob(panel_params$guides, position = "bottom", theme = theme)
            )
        } #,

        # https://github.com/tidyverse/ggplot2/blob/23e324197e0a5ddd764588d42838b0d96da9b68d/R/coord-cartesian-.r#L127
        #render_axis_v = function(panel_params, theme) {
            #arrange <- panel_params$y.arrange %||% c("primary", "secondary")
            #arrange_scale_keys <- c(primary = "y", secondary = "y.sec")[arrange]
            #arrange_scales <- panel_params[arrange_scale_keys]

            #ticks_minor_size_f <- panel_params$ticks_minor_size_f

            #list(
                #left = draw_view_scale_axis(arrange_scales[[1]], "left", theme, ticks_minor_size_f),
                #right = draw_view_scale_axis(arrange_scales[[2]], "right", theme, ticks_minor_size_f))
        #}
    )

coord_sci <- function(
    xlim = NULL, ylim = NULL,
    expand = TRUE, default = FALSE, clip = "on",
    ticks.minor.size.f = 0.5) {
    ggproto(NULL, CoordSci,
            limits = list(x = xlim, y = ylim),
            expand = expand, default = default, clip = clip,
            ticks_minor_size_f = ticks.minor.size.f)
}


ggplot(mtcars, aes(hp, mpg)) +
    theme_scientific() +
    scale_x_continuous() +
    coord_sci() +
    geom_point() -> plt

print(plt)