
# https://github.com/tidyverse/ggplot2/blob/115c3960d0fd068f1ca4cfe4650c0e0474aabba5/R/facet-wrap.r#L80
facet_sci <- function(
    facets, nrow = NULL, ncol = NULL, scales = "fixed",
    shrink = TRUE, labeller = "label_value", as.table = TRUE,
    drop = TRUE, dir = "h") {

    scales <- match.arg(scales, c("fixed", "free_x",
        "free_y", "free"))
    dir <- match.arg(dir, c("h", "v"))
    free <- list(x = any(scales %in% c("free_x", "free")),
        y = any(scales %in% c("free_y", "free")))

    ## Use left for now
    strip.position <- "left"

    if (identical(dir, "v")) {
        nrow_swap <- ncol
        ncol_swap <- nrow
        nrow <- ggplot2:::sanitise_dim(nrow_swap)
        ncol <- ggplot2:::sanitise_dim(ncol_swap)
    }
    else {
        nrow <- ggplot2:::sanitise_dim(nrow)
        ncol <- ggplot2:::sanitise_dim(ncol)
    }

    labeller <- ggplot2:::check_labeller(labeller)
    facets <- ggplot2:::wrap_as_facets_list(facets)
    ggproto(NULL, FacetSci, shrink = shrink, params = list(facets = facets,
        free = free, as.table = as.table, strip.position = strip.position,
        drop = drop, ncol = ncol, nrow = nrow, labeller = labeller,
        dir = dir))
}

# https://github.com/tidyverse/ggplot2/blob/115c3960d0fd068f1ca4cfe4650c0e0474aabba5/R/facet-wrap.r#L137
FacetSci <- ggproto("FacetSci", FacetWrap,
    draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
        if ((params$free$x || params$free$y) && !coord$is_free()) {
            stop(snake_class(coord), " doesn't support free scales", call. = FALSE)
        }
        
        if (inherits(coord, "CoordFlip")) {
            if (params$free$x) {
                layout$SCALE_X <- seq_len(nrow(layout))
            } else {
                layout$SCALE_X <- 1L
            }
            if (params$free$y) {
                layout$SCALE_Y <- seq_len(nrow(layout))
            } else {
                layout$SCALE_Y <- 1L
            }
        }

        ncol <- max(layout$COL)
        nrow <- max(layout$ROW)
        n <- nrow(layout)
        panel_order <- order(layout$ROW, layout$COL)
        layout <- layout[panel_order,]
        panels <- panels[panel_order]
        panel_pos <- ggplot2:::convertInd(layout$ROW, layout$COL, nrow)


        ## Here there are 4 x n axes, 4 per each plot
        ## Secondary? axes have less precision than primary
        axes <- render_axes(ranges, ranges, coord, theme, transpose = TRUE)
        assign("tmp", axes, envir = .GlobalEnv)
        if (length(params$facets) == 0) {
            # Add a dummy label
            labels_df <- new_data_frame(list("(all)" = "(all)"), n = 1)
        }
        else {
            labels_df <- layout[names(params$facets)]
        }

        attr(labels_df, "facet") <- "wrap"

        ## Here there are 4 x n strips, unclear what they do
        strips <- render_strips(
            structure(labels_df, type = "rows"),
            structure(labels_df, type = "cols"),
            params$labeller, theme)

        # If user hasn't set aspect ratio, and we have fixed scales, then
        # ask the coordinate system if it wants to specify one
        aspect_ratio <- theme$aspect.ratio
        if (is.null(aspect_ratio) &&
            !params$free$x &&
            !params$free$y) {
            aspect_ratio <- coord$aspect(ranges[[1]])
        }

        if (is.null(aspect_ratio)) {
            aspect_ratio <- 1
            respect <- FALSE
        }
        else {
            respect <- TRUE
        }

        empty_table <- matrix(list(zeroGrob()), nrow = nrow, ncol = ncol)
        panel_table <- empty_table
        panel_table[panel_pos] <- panels
        empties <- apply(panel_table, c(1, 2), function(x) ggplot2:::is.zero(x[[1]]))

        panel_table <-
            gtable_matrix("layout", panel_table,
                widths = unit(rep(1, ncol), "null"),
                heights = unit(rep(aspect_ratio, nrow), "null"),
                respect = respect, clip = coord$clip, z = matrix(1, ncol = ncol, nrow = nrow))

        panel_table$layout$name <- paste0("panel-", rep(seq_len(ncol), nrow), "-", rep(seq_len(nrow), each = ncol))

        ## `panel_table` contains only plot panels

        panel_table <- gtable_add_col_space(panel_table,
            theme$panel.spacing.x %||% theme$panel.spacing)
        panel_table <- gtable_add_row_space(panel_table,
            theme$panel.spacing.y %||% theme$panel.spacing)

        # Add axes
        axis_mat_x_top <- empty_table
        axis_mat_x_top[panel_pos] <- axes$x$top[layout$SCALE_X]
        axis_mat_x_bottom <- empty_table
        axis_mat_x_bottom[panel_pos] <- axes$x$bottom[layout$SCALE_X]
        axis_mat_y_left <- empty_table
        axis_mat_y_left[panel_pos] <- axes$y$left[layout$SCALE_Y]
        axis_mat_y_right <- empty_table
        axis_mat_y_right[panel_pos] <- axes$y$right[layout$SCALE_Y]

        print(panel_pos)
        print(axes$x$top[layout$SCALE_X])

        
        axis_height_top <- unit(
            apply(axis_mat_x_top, 1, max_height, value_only = TRUE),
            "cm")

        axis_height_bottom <- unit(
            apply(axis_mat_x_bottom, 1, max_height, value_only = TRUE),
            "cm")

        axis_width_left <- unit(
            apply(axis_mat_y_left, 2, max_width, value_only = TRUE),
            "cm")

        axis_width_right <- unit(
            apply(axis_mat_y_right, 2, max_width, value_only = TRUE),
            "cm")

        # Add back missing axes
        if (any(empties)) {
            first_row <- which(apply(empties, 1, any))[1] - 1
            first_col <- which(apply(empties, 2, any))[1] - 1
            row_panels <- which(layout$ROW == first_row & layout$COL > first_col)
            row_pos <- ggplot2:::convertInd(layout$ROW[row_panels], layout$COL[row_panels], nrow)
            row_axes <- axes$x$bottom[layout$SCALE_X[row_panels]]
            col_panels <- which(layout$ROW > first_row & layout$COL == first_col)
            col_pos <- ggplot2:::convertInd(layout$ROW[col_panels], layout$COL[col_panels], nrow)
            col_axes <- axes$y$right[layout$SCALE_Y[col_panels]]
            inside <- (theme$strip.placement %||% "inside") == "inside"
            if (params$strip.position == "bottom" &&
                !inside &&
                any(!vapply(row_axes, ggplot2:::is.zero, logical(1))) &&
                !params$free$x) {
                warning("Suppressing axis rendering when strip.position = 'bottom' and strip.placement == 'outside'", call. = FALSE)
            } else {
                axis_mat_x_bottom[row_pos] <- row_axes
            }
            if (params$strip.position == "right" &&
                !inside &&
                any(!vapply(col_axes, ggplot2:::is.zero, logical(1))) &&
                !params$free$y) {
                warning("Suppressing axis rendering when strip.position = 'right' and strip.placement == 'outside'", call. = FALSE)
            } else {
                axis_mat_y_right[col_pos] <- col_axes
            }
        }


        panel_table <- ggplot2:::weave_tables_row(panel_table, axis_mat_x_top, -1, axis_height_top, "axis-t", 3)
        panel_table <- ggplot2:::weave_tables_row(panel_table, axis_mat_x_bottom, 0, axis_height_bottom, "axis-b", 3)
        panel_table <- ggplot2:::weave_tables_col(panel_table, axis_mat_y_left, -1, axis_width_left, "axis-l", 3)
        panel_table <- ggplot2:::weave_tables_col(panel_table, axis_mat_y_right, 0, axis_width_right, "axis-r", 3)

        ## TODO : Fix stripes
        strip_padding <- convertUnit(theme$strip.switch.pad.wrap, "cm") %T>% print
        strip_name <- paste0("strip-", substr(params$strip.position, 1, 1))
        strip_mat <- empty_table

        strip_mat[panel_pos] <- unlist(unname(strips), recursive = FALSE)[["left"]]
        placement <- -2
        strip_pad <- axis_width_left

        strip_pad[as.numeric(strip_pad) != 0] <- strip_padding
        strip_width <- unit(apply(strip_mat, 2, max_width, value_only = TRUE), "cm")
        panel_table <- ggplot2:::weave_tables_col(panel_table, strip_mat, placement, strip_width, strip_name, 2, coord$clip)
        strip_pad[as.numeric(strip_pad) != 0] <- strip_padding
        panel_table <- ggplot2:::weave_tables_col(panel_table, col_shift = placement, col_width = strip_pad)

        panel_table
    }
)


(mtcars %>%
    ggplot_sci(aes(x = hp, y = mpg, col = as_factor(cyl), shape = as_factor(gear))) +
    geom_point() +
    scale_x_sci(sec.axis = sec_axis_sci(~.)) +
    scale_y_sci(sec.axis = sec_axis_sci(~.)) +
    facet_wrap(~gear,# ncol = 1,
        labeller = facet_labeller())
    ) %T>% { assign("temp_plot", ., envir = .GlobalEnv) } -> plt #%>%
#egg::expose_layout() %>%
#print

#egg::expose_layout(plt)
plt %>%
    postprocess_axes(
        axes_margin = mar_(h = u_(1.5 ~ cm), v = u_(1.5 ~ cm)),
        text_margin = mar_(h = u_(1 ~ cm), v = u_(1 ~ cm))
        ) -> tbl
grid.newpage()
grid.draw(tbl)
print(tbl)
#print(convertX(sum(tbl$grobs[[7]]$children[[2]]$grobs[[2]]$x - tbl$grobs[[8]]$children[[2]]$grobs[[1]]$x), "native", TRUE))
#gtable::gtable_show_layout(tbl)