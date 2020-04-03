stat_density2d_sci <- function(
    mapping = NULL, data = NULL, geom = "density_2d",
    position = "identity", ..., contour = TRUE, contour_var = "density",
    n = 100, h = NULL, adjust = c(1, 1), na.rm = FALSE, show.legend = NA,
    inherit.aes = TRUE) {

    layer(data = data, mapping = mapping, stat = StatDensity2dSci,
        geom = geom, position = position, show.legend = show.legend,
        inherit.aes = inherit.aes, params = list(
            na.rm = na.rm,
            contour = contour, contour_var = contour_var, n = n,
            h = h, adjust = adjust, ...))
}

stat_density2d_filled_sci <- function(
    mapping = NULL, data = NULL, geom = "density_2d_filled",
    position = "identity", ..., contour = TRUE, contour_var = "density",
    n = 100, h = NULL, adjust = c(1, 1), na.rm = FALSE, show.legend = NA,
    inherit.aes = TRUE) {

    layer(data = data, mapping = mapping, stat = StatDensity2dFilledSci,
        geom = geom, position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,  params = list(
            na.rm = na.rm,
            contour = contour, contour_var = contour_var, n = n,
            h = h, adjust = adjust, ...))
}

geom_density2d_sci <- function(
    mapping = NULL, data = NULL, stat = "density_2d_sci",
    position = "identity", ..., contour_var = "density",
    lineend = "butt", linejoin = "round", linemitre = 10,
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    layer(data = data, mapping = mapping,
        stat = stat, geom = GeomDensity2d,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes, params = list(
            lineend = lineend, linejoin = linejoin,
            linemitre = linemitre, contour = TRUE,
            contour_var = contour_var, na.rm = na.rm, ...))
}

geom_density2d_filled_sci <- function(
    mapping = NULL, data = NULL, stat = "density_2d_filled_sci",
    position = "identity", ..., contour_var = "density",
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

    layer(data = data, mapping = mapping, stat = stat,
          geom = GeomDensity2dFilled, position = position,
          show.legend = show.legend, inherit.aes = inherit.aes,
            params = list(
                na.rm = na.rm, contour = TRUE,
                contour_var = contour_var, ...))
}
stat_density_2d_sci <- stat_density2d_sci
stat_density_2d_filled_sci <- stat_density2d_filled_sci
geom_density_2d_sci <- geom_density2d_sci
geom_density_2d_fill_sci <- geom_density2d_filled_sci

StatDensity2dSci <- ggproto("StatDensity2dSci", StatDensity2d,
    compute_layer = function(self, data, params, layout) {
        # first run the regular layer calculation to infer densities
        data <- ggproto_parent(Stat, self)$compute_layer(data, params, layout)

        # if we're not contouring we're done
        if (!isTRUE(params$contour)) return(data)

        # set up data and parameters for contouring
        contour_var <- params$contour_var %||% "density"
        if (!isTRUE(contour_var %in% c("density", "ndensity", "count")))
            abort(glue(
                "Unsupported value for `contour_var`: {contour_var}\n",
                "Supported values are \"density\", \"ndensity\", and \"count\"."))
        
        data$z <- data[[contour_var]]
        z.range <- range(data$z, na.rm = TRUE, finite = TRUE)
        params <- params[intersect(names(params), c("bins", "binwidth", "breaks"))]
        params$z.range <- z.range

        if (isTRUE(self$contour_type == "bands")) {
            contour_stat <- StatContourFilledSci
        } else {
            # lines is the default
            contour_stat <- StatContourSci
        }

        args <- c(list(data = quote(data), scales = quote(scales)), params)
        ggplot2:::dapply(data, "PANEL", function(data) {
            scales <- layout$get_scales(data$PANEL[1])
            tryCatch(do.call(contour_stat$compute_panel, args), error = function(e) {
                warn(glue::glue("Computation failed in `{ggplot2:::snake_class(self)}()`:\n{e$message}"))
                new_data_frame()
            })
        })
    })

StatDensity2dFilledSci <- ggproto("StatDensity2dFilledSci", StatDensity2dSci,
    default_aes = aes(colour = NA, fill = after_stat(level)),
    contour_type = "bands"
)

StatContourSci <- ggproto("StatContourSci", StatContour,
    compute_group = function(data, scales, z.range, bins = NULL, binwidth = NULL,
                           breaks = NULL, na.rm = FALSE) {
        ### Custom density
        default_levels <- c(0.2, 0.5, 0.7, 0.9)
        z <- sort(data$density)
        dx <- mean(diff(data$x %>% unique), na.rm = TRUE)
        dy <- mean(diff(data$y %>% unique), na.rm = TRUE)
        cz <- cumsum(z) * dx * dy
        lvls <- breaks %||% default_levels
        breaks <- map_dbl(lvls, ~ approx(cz, z, xout = 1 - .x)$y)
        ### End

        breaks <- ggplot2:::contour_breaks(z.range, bins, binwidth, breaks)

        isolines <- ggplot2:::xyz_to_isolines(data, breaks)
        map_int(isolines, ~ vec_size(.x$id)) -> reps
        
        path_df <- ggplot2:::iso_to_path(isolines, data$group[1])
        path_df$level <- as.numeric(path_df$level)
        path_df$prob <- map2(lvls, reps, vec_repeat) %>% flatten_dbl
        path_df$nlevel <- rescale_max(path_df$level)

        path_df
    }
)

StatContourFilledSci <- ggproto("StatContourFilledSci", StatContourFilled,
    compute_group = function(data, scales, z.range, bins = NULL, binwidth = NULL, breaks = NULL, na.rm = FALSE) {
        ### Custom density
        default_levels <- c(0.2, 0.5, 0.7, 0.9)
        z <- sort(data$density)
        dx <- mean(diff(data$x %>% unique), na.rm = TRUE)
        dy <- mean(diff(data$y %>% unique), na.rm = TRUE)
        cz <- cumsum(z) * dx * dy
        lvls <- sort(breaks %||% default_levels, decreasing = TRUE)
        breaks <- map_dbl(lvls, ~ approx(cz, z, xout = 1 - .x)$y)
        na_br <- is.na(breaks)
        breaks <- breaks[!na_br]
        ### End
        breaks <- ggplot2:::contour_breaks(z.range, bins, binwidth, breaks)

        #if (breaks[n_breaks] < z.range[2])
            breaks <- vec_c(breaks, z.range[2])

        isobands <- ggplot2:::xyz_to_isobands(data, breaks)
        names(isobands)
        map_int(isobands, ~vec_size(.x$id)) -> rep

        names(isobands) <- ggplot2:::pretty_isoband_levels(names(isobands))

        path_df <- ggplot2:::iso_to_polygon(isobands, data$group[1])

        path_df$level <- ordered(path_df$level, levels = names(isobands))
        path_df$level_low <- breaks[as.numeric(path_df$level)]
        path_df$level_high <- breaks[as.numeric(path_df$level) + 1]
        path_df$level_mid <- 0.5 * (path_df$level_low + path_df$level_high)
        path_df$nlevel <- rescale_max(path_df$level_high)

        path_df$prob <- map2(lvls[!na_br], rep, vec_repeat) %>% flatten_dbl

        path_df
    })

ggplot_sci(mtcars, aes(hp, mpg)) +
    geom_density2d_filled_sci(aes(fill = as_factor(..prob..)), breaks = c(0.16, 0.32, 0.68, 0.84, 0.975)) +
    geom_density2d_sci(col = "black", breaks = c(0.16, 0.32, 0.68, 0.84, 0.975)) +
    scale_fill_viridis_d(direction = -1) +
    scale_x_sci() +
    scale_y_sci() +
    geom_point() -> plt

print(plt)