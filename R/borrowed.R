# https://github.com/tidyverse/ggplot2/blob/23e324197e0a5ddd764588d42838b0d96da9b68d/R/guides-axis.r#L19
draw_axis <- function(break_positions,
                      # --Extra parameter #1--
                      break_positions_minor,
                      # ----------------------
                      break_labels, axis_position, theme,
                      check.overlap = FALSE, angle = NULL, n_dodge = 1,
                      # --Extra parameter #2--
                      ticks_minor_size_f = 0.5
                      # ----------------------
                      ) {

    axis_position <- match.arg(axis_position, c("top", "bottom", "right", "left"))
    aesthetic <- if (axis_position %in% c("top", "bottom")) "x" else "y"

    # resolve elements
    line_element_name <- paste0("axis.line.", aesthetic, ".", axis_position)
    tick_element_name <- paste0("axis.ticks.", aesthetic, ".", axis_position)
    tick_length_element_name <- paste0("axis.ticks.length.", aesthetic, ".", axis_position)
    label_element_name <- paste0("axis.text.", aesthetic, ".", axis_position)

    line_element <- calc_element(line_element_name, theme)
    tick_element <- calc_element(tick_element_name, theme)
    tick_length <- calc_element(tick_length_element_name, theme)
    label_element <- calc_element(label_element_name, theme)

    # override label element parameters for rotation
    if (inherits(label_element, "element_text")) {
        label_overrides <- ggplot2:::axis_label_element_overrides(axis_position, angle)
        # label_overrides is always an element_text(), but in order for the merge to
        # keep the new class, the override must also have the new class
        class(label_overrides) <- class(label_element)
        label_element <- merge_element(label_overrides, label_element)
    }

    # conditionally set parameters that depend on axis orientation
    is_vertical <- axis_position %in% c("left", "right")

    position_dim <- if (is_vertical) "y" else "x"
    non_position_dim <- if (is_vertical) "x" else "y"
    position_size <- if (is_vertical) "height" else "width"
    non_position_size <- if (is_vertical) "width" else "height"
    gtable_element <- if (is_vertical) gtable_row else gtable_col
    measure_gtable <- if (is_vertical) gtable_width else gtable_height
    measure_labels_non_pos <- if (is_vertical) grobWidth else grobHeight

    # conditionally set parameters that depend on which side of the panel
    # the axis is on
    is_second <- axis_position %in% c("right", "top")

    tick_direction <- if (is_second) 1 else-1
    non_position_panel <- if (is_second) unit(0, "npc") else unit(1, "npc")
    tick_coordinate_order <- if (is_second) c(2, 1) else c(1, 2)

    # conditionally set the gtable ordering
    labels_first_gtable <- axis_position %in% c("left", "top") # refers to position in gtable

    # set common parameters
    n_breaks <- length(break_positions)

    ### --Adding small breaks--
    n_breaks_minor <- length(break_positions_minor)
    ### -----------------------

    opposite_positions <- c("top" = "bottom", "bottom" = "top", "right" = "left", "left" = "right")
    axis_position_opposite <- unname(opposite_positions[axis_position])

    # draw elements
    line_grob <- exec(
        element_grob, line_element,
        !!position_dim := unit(c(0, 1), "npc"),
        !!non_position_dim := unit.c(non_position_panel, non_position_panel)
      )

    if (n_breaks == 0) {
        return(
          ggplot2:::absoluteGrob(
            gList(line_grob),
            width = grobWidth(line_grob),
            height = grobHeight(line_grob)))
    }

    # break_labels can be a list() of language objects
    if (is.list(break_labels)) {
        if (any(vapply(break_labels, is.language, logical(1)))) {
            break_labels <- do.call(expression, break_labels)
        } else {
            break_labels <- unlist(break_labels)
        }
    }

    # calculate multiple rows/columns of labels (which is usually 1)
    dodge_pos <- rep(seq_len(n_dodge), length.out = n_breaks)
    dodge_indices <- split(seq_len(n_breaks), dodge_pos)

    label_grobs <- lapply(dodge_indices, function(indices) {
        ggplot2:::draw_axis_labels(
          break_positions = break_positions[indices],
          break_labels = break_labels[indices],
          label_element = label_element,
          is_vertical = is_vertical,
          check.overlap = check.overlap)
    })

    ### ---Merging minor and major ticks into one table & sorting----
    ticks_data <- data.frame(
        breaks = c(break_positions, break_positions_minor),
        size = c(rep(1, n_breaks), rep(ticks_minor_size_f, n_breaks_minor)))
    ticks_data <- ticks_data[order(ticks_data$breaks),]

    # Indices for ordering
    inds <- as.vector(sapply(seq_len(n_breaks + n_breaks_minor),
            function(x) x + (tick_coordinate_order - 1) * (n_breaks + n_breaks_minor)))

    # Variable-length ticks
    non_pos_dims <- unit.c(non_position_panel + (tick_direction * tick_length * ticks_data$size),
                           rep(non_position_panel, n_breaks + n_breaks_minor))[inds]

    # create gtable
    ticks_grob <- exec(
        element_grob, tick_element,
        !!position_dim := rep(unit(ticks_data$breaks, "native"), each = 2),
        !!non_position_dim := non_pos_dims,
        id.lengths = rep(2, times = n_breaks + n_breaks_minor))
    ### -------------------------------------------------------------

    non_position_sizes <- paste0(non_position_size, "s")
    label_dims <- do.call(unit.c, lapply(label_grobs, measure_labels_non_pos))
    grobs <- c(list(ticks_grob), label_grobs)
    grob_dims <- unit.c(tick_length, label_dims)

    if (labels_first_gtable) {
        grobs <- rev(grobs)
        grob_dims <- rev(grob_dims)
    }

    gt <- exec(
        gtable_element,
        name = "axis",
        grobs = grobs,
        !!non_position_sizes := grob_dims,
        !!position_size := unit(1, "npc")
      )

    # create viewport
    justvp <- exec(
        viewport,
        !!non_position_dim := non_position_panel,
        !!non_position_size := measure_gtable(gt),
        just = axis_position_opposite
      )

    ggplot2:::absoluteGrob(
        gList(line_grob, gt),
        width = gtable_width(gt),
        height = gtable_height(gt),
        vp = justvp
      ) -> grb
    tmp <<- grb
}


# https://github.com/tidyverse/ggplot2/blob/23e324197e0a5ddd764588d42838b0d96da9b68d/R/coord-cartesian-.r#L156
draw_view_scale_axis <- function(view_scale, axis_position, theme,
        # --Extra parameter #1--
        ticks_minor_size_f
        # ----------------------
) {
    if (is.null(view_scale) || view_scale$is_empty()) {
        return(zeroGrob())
    }

    draw_axis(
        break_positions = view_scale$break_positions(),
        ### --Calling redefined version with extra breaks supported--
        break_positions_minor = view_scale$break_positions_minor(),
        ### ---------------------------------------------------------
        break_labels = view_scale$get_labels(),
        axis_position = axis_position,
        theme = theme,
        ticks_minor_size_f = ticks_minor_size_f)
}

## https://github.com/tidyverse/ggplot2/blob/23e324197e0a5ddd764588d42838b0d96da9b68d/R/coord-cartesian-.r#L139
#view_scales_from_scale <- function(scale, coord_limits = NULL, expand = TRUE) {
    #expansion <- ggplot2:::default_expansion(scale, expand = expand)
    #limits <- scale$get_limits()
    #continuous_range <- ggplot2:::expand_limits_scale(scale, expansion, limits, coord_limits = coord_limits) %>% print
    #aesthetic <- scale$aesthetics[1]

    #view_scales <- list(
        #ggplot2:::view_scale_primary(scale, limits, continuous_range),
        #sec = ggplot2:::view_scale_secondary(scale, limits, continuous_range),
        #arrange = scale$axis_order(),
        #range = continuous_range) %>% print

    #names(view_scales) <- c(aesthetic, paste0(aesthetic, ".", names(view_scales)[-1]))

    #view_scales
#}s
# https://github.com/tidyverse/ggplot2/blob/23e324197e0a5ddd764588d42838b0d96da9b68d/R/axis-secondary.R#L82
sec_axis_sci <- function(
    axis_trans = NULL,
    name = name_filler(),
    breaks = waiver(),
    labels = waiver(),
    breaks_trans = waiver()) {
    # sec_axis() historically accpeted two-sided formula, so be permissive.
    if (length(axis_trans) > 2) axis_trans <- axis_trans[c(1, 3)]

    axis_trans <- as_function(axis_trans)

    if (rlang::is_null(breaks_trans) || ggplot2:::is.waive(breaks_trans))
        breaks_trans <- identity_sci_trans()

    ggproto(NULL, AxisSecondarySci,
        breaks_trans = breaks_trans,
        trans = axis_trans,
        name = name,
        breaks = breaks,
        labels = labels
      )
}

# https://github.com/tidyverse/ggplot2/blob/23e324197e0a5ddd764588d42838b0d96da9b68d/R/axis-secondary.R#L127
AxisSecondarySci <- ggproto("AxisSecondarySci", AxisSecondary,
    breaks_trans = NULL,
    #trans = NULL,
    #axis = NULL,
    #name = waiver(),
    #breaks = waiver(),
    #labels = waiver(),

    ## This determines the quality of the remapping from the secondary axis and
    ## back to the primary axis i.e. the exactness of the placement of the
    ## breakpoints of the secondary axis.
    #detail = 1000,

    #empty = function(self) {
        #is.null(self$trans)
    #},
    # Inherit settings from the primary axis/scale
    init = function(self, scale) {
        if (self$empty()) return()
        if (!is.function(self$trans)) stop("transformation for secondary axes must be a function", call. = FALSE)
        if (ggplot2:::is.derived(self$name) && !ggplot2:::is.waive(scale$name)) self$name <- scale$name
        if (ggplot2:::is.derived(self$breaks)) self$breaks <- scale$breaks
        if (ggplot2:::is.waive(self$breaks)) self$breaks <- scale$trans$breaks
        if (ggplot2:::is.derived(self$labels)) self$labels <- scale$labels
        if (rlang::is_null(self$breaks_trans) || ggplot2:::is.waive(self$breaks_trans))
            self$breaks_trans <- identity_sci_trans()
        if (ggplot2:::is.derived(self$breaks_trans))
            self$breaks_trans <- scale$trans
        
    },

    transform_range = function(self, range) {
        self$trans(range)
    },

    mono_test = function(self, scale) {
        range <- scale$range$range
        along_range <- seq(range[1], range[2], length.out = self$detail)
        old_range <- scale$trans$inverse(along_range)

        # Create mapping between primary and secondary range
        full_range <- self$transform_range(old_range)

        # Test for monotonicity
        if (length(unique(sign(diff(full_range)))) != 1)
            stop("transformation for secondary axes must be monotonic")
    },

    break_info = function(self, range, scale) {
        if (self$empty()) return()
        
        # Test for monotonicity on unexpanded range
        self$mono_test(scale)

        # Get scale's original range before transformation
        along_range <- seq(range[1], range[2], length.out = self$detail)
        old_range <- scale$trans$inverse(along_range)

        # Create mapping between primary and secondary range
        full_range <- self$transform_range(old_range)

        # Get break info for the secondary axis
        new_range <- range(full_range, na.rm = TRUE)

        # patch for date and datetime scales just to maintain functionality
        # works only for linear secondary transforms that respect the time or date transform
        if (scale$trans$name %in% c("date", "time")) {
            temp_scale <- self$create_scale(new_range, trans = scale$trans)
            range_info <- temp_scale$break_info()
            old_val_trans <- rescale(range_info$major, from = c(0, 1), to = range)
            old_val_minor_trans <- rescale(range_info$minor, from = c(0, 1), to = range)
        } 
        else {
            temp_scale <- self$create_scale(new_range) #%T>% print
            #print(temp_scale$break_info())
            range_info <- temp_scale$break_info() #%T>% print
            
            # Map the break values back to their correct position on the primary scale
            old_val <- lapply(range_info$major_source, function(x) which.min(abs(full_range - x)))
            old_val <- old_range[unlist(old_val)]
            old_val_trans <- scale$trans$transform(old_val)

            old_val_minor <- lapply(range_info$minor_source, function(x) which.min(abs(full_range - x)))
            old_val_minor <- old_range[unlist(old_val_minor)]
            old_val_minor_trans <- scale$trans$transform(old_val_minor)

            # rescale values from 0 to 1
            range_info$major[] <- round(
                rescale(
                    scale$map(old_val_trans, range(old_val_trans)),
                    from = range),
                digits = 3)
            range_info$minor[] <- round(
                rescale(
                    scale$map(old_val_minor_trans, range(old_val_minor_trans)),
                    from = range),
                digits = 3)
        }

        # The _source values should be in (primary) scale_transformed space,
        # so that the coord doesn't have to know about the secondary scale transformation
        # when drawing the axis. The values in user space are useful for testing.
        range_info$major_source_user <- range_info$major_source
        range_info$minor_source_user <- range_info$minor_source
        range_info$major_source[] <- old_val_trans
        range_info$minor_source[] <- old_val_minor_trans

        names(range_info) <- paste0("sec.", names(range_info))
        range_info
    },

    # Temporary scale for the purpose of calling break_info()
    create_scale = function(self, range, trans = self$breaks_trans) {
        scale <- ggproto(NULL, ScaleContinuousPosition,
                name = self$name,
                breaks = self$breaks,
                labels = self$labels,
                limits = range,
                expand = c(0, 0),
                trans = trans)
        scale$train(range)
        scale
    }#,
    #make_title = function(title) {
        #title
    #}
)

#(RLibs::read_smart(fs::path("work_set.fth")) %>%
    #filter(Filter == "H") %>%
    #ggplot(aes(JD, Flux ^ 2)) +
    #geom_point() +
    #coord_sci() +
    #theme_scientific(plot.margin = mar_(1 ~ cm)) +
    #scale_y_log10_sci(sec.axis = sec_axis_sci(~-2.5 * log10(. / 1e-45))) +
    #scale_x_sci(sec.axis = weak_dup_axis_sci())) %>%
    #egg::expose_layout() %>%
    #print


(mtcars %>%
    ggplot(aes(x = hp, y = mpg, col = as_factor(cyl), shape = as_factor(gear))) +
    geom_point() +
    theme_scientific(
        plot.margin = mar_(0 ~ cm)) +
    coord_sci() +
    scale_x_sci(expand = expansion(vctrs::vec_c(0.05, 0.2), 0),
        sec.axis = weak_dup_axis_sci()) +
    scale_y_sci(
        name = NULL,
        sec.axis = sec_axis_sci(~., labels = labels_filler())) +
    facet_grid(rows = vars(gear), cols = vars(cyl), 
        labeller = facet_labeller())
        ) %T>%
            {assign("temp_plot", ., envir = .GlobalEnv) } %>%
    #egg::expose_layout() %>%
    print
