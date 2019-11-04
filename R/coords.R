CoordSci <-
    ggproto("CoordSci", CoordCartesian,
        # https://github.com/tidyverse/ggplot2/blob/115c3960d0fd068f1ca4cfe4650c0e0474aabba5/R/coord-cartesian-.r#L99
        #setup_panel_params = function(self, scale_x, scale_y, params = list()) {
            #res <- append(ggproto_parent(CoordCartesian, self)$setup_panel_params(scale_x, scale_y, params),
                   #list(ticks_minor_size_f = self$ticks_minor_size_f))

            #rlang::abort(" ")
            ##c(view_scales_from_scale(scale_x, self$limits$x, self$expand),
              ##view_scales_from_scale(scale_y, self$limits$y, self$expand),
              ##list(ticks_minor_size_f = self$ticks_minor_size_f))
        #} ,
        # https://github.com/tidyverse/ggplot2/blob/fa000f786cb0b641600b6de68ae0f96e2ffc5e75/R/coord-cartesian-.r#L138
        train_panel_guides = function(self, panel_params, layers, default_mapping, params = list()) {
            aesthetics <- c("x", "y", "x.sec", "y.sec")
            names(aesthetics) <- aesthetics
            panel_params$guides <- lapply(aesthetics, function(aesthetic) {
                axis <- substr(aesthetic, 1, 1)
                guide <- panel_params$guides[[aesthetic]]
                ## Using modified training method
                guide <- guide_train(guide, panel_params[[aesthetic]])
                guide <- ggplot2:::guide_transform(guide, self, panel_params)
                guide <- ggplot2:::guide_geom(guide, layers, default_mapping)
                guide
            })
            panel_params
        },
        # https://github.com/tidyverse/ggplot2/blob/115c3960d0fd068f1ca4cfe4650c0e0474aabba5/R/coord-cartesian-.r#L185
        render_axis_h = function(panel_params, theme) {
            list(
              top = panel_guides_grob(panel_params$guides, position = "top", theme = theme),
              bottom = panel_guides_grob(panel_params$guides, position = "bottom", theme = theme))
        },
        # https://github.com/tidyverse/ggplot2/blob/115c3960d0fd068f1ca4cfe4650c0e0474aabba5/R/coord-cartesian-.r#L192
        render_axis_v = function(panel_params, theme) {
            list(
              left = panel_guides_grob(panel_params$guides, position = "left", theme = theme),
              right = panel_guides_grob(panel_params$guides, position = "right", theme = theme))
        }
    )

coord_sci <- function(
    xlim = NULL, ylim = NULL,
    expand = TRUE, default = FALSE, clip = "on") {
    ggproto(NULL, CoordSci,
            limits = list(x = xlim, y = ylim),
            expand = expand, default = default, clip = clip)
}