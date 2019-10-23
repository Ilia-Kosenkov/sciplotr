# https://github.com/tidyverse/ggplot2/blob/fa000f786cb0b641600b6de68ae0f96e2ffc5e75/R/guides-axis.r#L180
draw_axis <- function(break_positions, break_labels, axis_position, theme,
                      check.overlap = FALSE, angle = NULL, n.dodge = 1,
                      break_types = vctrs::vec_recycle("major", vctrs::vec_size(break_positions))) {
    
    axis_position <- match.arg(axis_position, c("top", "bottom", "right", "left"))
    aesthetic <- if (axis_position %in% c("top", "bottom")) "x" else "y"

    # resolve elements
    line_element_name <- paste0("axis.line.", aesthetic, ".", axis_position)
    tick_element_name <- paste0("axis.ticks.", aesthetic, ".", axis_position)
    tick_length_element_name <- paste0("axis.ticks.length.", aesthetic, ".", axis_position)
    ## WATCH: obtaining minor tick element
    tick_minor_length_element_name <- paste0("axis.ticks.minor.length.", aesthetic, ".", axis_position)
    ##
    label_element_name <- paste0("axis.text.", aesthetic, ".", axis_position)

    line_element <- calc_element(line_element_name, theme)
    tick_element <- calc_element(tick_element_name, theme)
    tick_length <- calc_element(tick_length_element_name, theme)
    label_element <- calc_element(label_element_name, theme)
    ###
    tick_minor_length <- calc_element(tick_minor_length_element_name, theme)
    ###

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
    opposite_positions <- c("top" = "bottom", "bottom" = "top", "right" = "left", "left" = "right")
    axis_position_opposite <- unname(opposite_positions[axis_position])

    # draw elements
    line_grob <- exec(
        element_grob, line_element,
        !!position_dim := unit(c(0, 1), "npc"),
        !!non_position_dim := unit.c(non_position_panel, non_position_panel))

    if (n_breaks == 0) {
        return(
            ggplot2:::absoluteGrob(
                gList(line_grob),
                width = grobWidth(line_grob),
                height = grobHeight(line_grob)))
    }

    # break_labels can be a list() of language objects
    if (is.list(break_labels)) {
        if (any(vapply(break_labels, is.language, logical(1))))
            break_labels <- do.call(expression, break_labels)
        else
            break_labels <- unlist(break_labels)
    }

    # calculate multiple rows/columns of labels (which is usually 1)
    ## TODO : exclude minor ticks labels?

    n_breaks_major <- vctrs::vec_size(break_labels)
    dodge_pos <- rep(seq_len(n.dodge), length.out = n_breaks_major)#n_breaks)
    #dodge_indices <- split(seq_len(n_breaks), dodge_pos)
    dodge_indices <- split(seq_len(n_breaks_major), dodge_pos)
    labelled_pos <- break_positions[break_types == "major"]

    label_grobs <-
        lapply(dodge_indices,
            function(indices) {
                ggplot2:::draw_axis_labels(
                    break_positions = labelled_pos,#break_positions[indices],
                    break_labels = break_labels[indices],
                    label_element = label_element,
                    is_vertical = is_vertical,
                    check.overlap = check.overlap)
            })


    ## Generating variable length ticks
    tick_length_actual <- rep(tick_length, n_breaks)
    tick_length_actual[break_types == "minor"] <- tick_minor_length

    actual_tick_pos <- unit.c(non_position_panel + tick_direction * tick_length_actual,
                              rep(non_position_panel, n_breaks))

    actual_tick_pos <-
        actual_tick_pos[as.vector(sapply(1:n_breaks - 1, function(x) x + c(1, n_breaks + 1)[tick_coordinate_order]))]
    ##
    ticks_grob <- exec(
        element_grob, tick_element,
        !!position_dim := rep(unit(break_positions, "native"), each = 2),
        !!non_position_dim := actual_tick_pos,
        id.lengths = rep(2, times = n_breaks))

    # create gtable
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
        !!position_size := unit(1, "npc"))

    # create viewport
    justvp <- exec(
        viewport,
        !!non_position_dim := non_position_panel,
        !!non_position_size := measure_gtable(gt),
        just = axis_position_opposite)

    ggplot2:::absoluteGrob(
        gList(line_grob, gt),
        width = gtable_width(gt),
        height = gtable_height(gt),
        vp = justvp)
}



# https://github.com/tidyverse/ggplot2/blob/115c3960d0fd068f1ca4cfe4650c0e0474aabba5/R/coord-cartesian-.r#L222
panel_guides_grob <- function(guides, position, theme) {
    guide <- ggplot2:::guide_for_position(guides, position) %||% ggplot2:::guide_none()
    guide_gengrob(guide, theme)
}

guide_gengrob.axis <- function(guide, theme) {
    aesthetic <- names(guide$key)[!grepl("^\\.", names(guide$key))][1]
    draw_axis(break_positions = guide$key[[aesthetic]],
        break_labels = guide$key[guide$key$.type == "major", ".label"],
        axis_position = guide$position, theme = theme, check.overlap = guide$check.overlap,
        angle = guide$angle, n.dodge = guide$n.dodge,
        break_types = guide$key[[".type"]])
}



ggplot(mtcars, aes(hp, mpg)) +
    theme_sci() +
    scale_x_sci() +
    scale_y_sci() +
    coord_sci() +
    geom_point() -> plt

print(plt)