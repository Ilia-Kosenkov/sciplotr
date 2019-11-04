# https://github.com/tidyverse/ggplot2/blob/fb731913c57ee5fe972b836bc8989403bf2ab65f/R/plot-build.r#L28
#ggplot_build.ggplot <- function(plot) {
    #plot <- ggplot2:::plot_clone(plot)
    #if (length(plot$layers) == 0) {
        #plot <- plot + geom_blank()
    #}

    #layers <- plot$layers
    #layer_data <- lapply(layers, function(y) y$layer_data(plot$data))

    #scales <- plot$scales
    ## Apply function to layer and matching data
    ##by_layer <- function(f) {
        ##out <- vector("list", length(data))
        ##for (i in seq_along(data)) {
            ##out[[i]] <- f(l = layers[[i]], d = data[[i]])
        ##}
        ##out
    ##}

    #by_layer <- function(f) purrr::map2(layers, data, ~f(l = .x, d = .y))

    ## Allow all layers to make any final adjustments based
    ## on raw input data and plot info
    #data <- layer_data
    #data <- by_layer(function(l, d) l$setup_layer(d, plot))

    ## Initialise panels, add extra data for margins & missing faceting
    ## variables, and add on a PANEL variable to data
    #### TODO : Replace by custom layout
    #layout <- ggplot2:::create_layout(plot$facet, plot$coordinates)
    #data <- layout$setup(data, plot$data, plot$plot_env)

    ## Compute aesthetics to produce data with generalised variable names
    #data <- by_layer(function(l, d) l$compute_aesthetics(d, plot))

    ## Transform all scales
    #data <- lapply(data, ggplot2:::scales_transform_df, scales = scales)

    ## Map and train positions so that statistics have access to ranges
    ## and all positions are numeric
    #scale_x <- function() scales$get_scales("x")
    #scale_y <- function() scales$get_scales("y")

    #layout$train_position(data, scale_x(), scale_y())
    #data <- layout$map_position(data)

    ## Apply and map statistics
    #data <- by_layer(function(l, d) l$compute_statistic(d, layout))
    #data <- by_layer(function(l, d) l$map_statistic(d, plot))

    ## Make sure missing (but required) aesthetics are added
    #ggplot2:::scales_add_missing(plot, c("x", "y"), plot$plot_env)

    ## Reparameterise geoms from (e.g.) y and width to ymin and ymax
    #data <- by_layer(function(l, d) l$compute_geom_1(d))

    ## Apply position adjustments
    #data <- by_layer(function(l, d) l$compute_position(d, layout))

    ## Reset position scales, then re-train and map.  This ensures that facets
    ## have control over the range of a plot: is it generated from what is
    ## displayed, or does it include the range of underlying data
    #layout$reset_scales()
    #layout$train_position(data, scale_x(), scale_y())
    #layout$setup_panel_params()# %T>% print### Axes are created around here
    #data <- layout$map_position(data)

    ## Train and map non-position scales
    #npscales <- scales$non_position_scales()
    #if (npscales$n() > 0) {
        #lapply(data, ggplot2:::scales_train_df, scales = npscales)
        #data <- lapply(data, ggplot2:::scales_map_df, scales = npscales)
    #}

    ## Fill in defaults etc.
    #data <- by_layer(function(l, d) l$compute_geom_2(d))

    ## Let layer stat have a final say before rendering
    #data <- by_layer(function(l, d) l$finish_statistics(d))

    ## Let Layout modify data before rendering
    #data <- layout$finish_data(data)

    #structure(
    #list(data = data, layout = layout, plot = plot),
    #class = "ggplot_built"
  #)
#}