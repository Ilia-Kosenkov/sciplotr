
# https://github.com/tidyverse/ggplot2/blob/269be6fe56a71bef2687ac4c1f39992de45ae87a/R/facet-grid-.r#L111
facet_sci <- function(rows = NULL, cols = NULL, scales = "fixed",
                      space = "fixed", shrink = TRUE,
                      labeller = "label_value", as.table = TRUE,
                      drop = TRUE) {


    scales <- match.arg(scales, vctrs::vec_c("fixed", "free_x", "free_y", "free"))
    free <- list(
        x = any(vctrs::vec_in(scales, vctrs::vec_c("free_x", "free"))),
        y = any(vctrs::vec_in(scales, vctrs::vec_c("free_y", "free"))))

    space <- match.arg(space, vctrs::vec_c("fixed", "free_x", "free_y", "free"))
    space_free <- list(
        x = any(vctrs::vec_in(space, vctrs::vec_c("free_x", "free"))),
        y = any(vctrs::vec_in(space, vctrs::vec_c("free_y", "free"))))


    facets_list <- ggplot2:::grid_as_facets_list(rows, cols)

    # Check for deprecated labellers
    labeller <- ggplot2:::check_labeller(labeller)

    ggproto(NULL, FacetSci,
        shrink = shrink,
        params = list(rows = facets_list$rows, cols = facets_list$cols, margins = FALSE,
            free = free, space_free = space_free, labeller = labeller,
            as.table = as.table, drop = drop))
}

# https://github.com/tidyverse/ggplot2/blob/269be6fe56a71bef2687ac4c1f39992de45ae87a/R/facet-grid-.r#L189
FacetSci <- ggproto("FacetSci", FacetGrid,
    compute_layout = function(data, params) {
        rows <- params$rows
        cols <- params$cols

        dups <- intersect(names(rows), names(cols))

        if (vctrs::vec_size(dups) > 0)
            rlang::abort(
            paste0(
                "Faceting variables can only appear in row or cols, not both.\n",
                "Problems: ",
                paste0(dups, collapse = "'")),
            "sciplotr_invalid_arg")

        base_rows <-
            dplyr::mutate_all(
                dplyr::as_tibble(combine_vars(data, params$plot_env, rows, drop = params$drop)),
                forcats::as_factor)

        if (!rlang::is_null(params$as.table) && !params$as.table)
            base_rows <- dplyr::mutate_all(base_rows, forcats::fct_rev)

        base_cols <-
            dplyr::mutate_all(
                dplyr::as_tibble(combine_vars(data, params$plot_env, cols, drop = params$drop)),
                forcats::as_factor)

        base <- df_grid(base_rows, base_cols, params$margin)

        if (vctrs::vec_size(base) %==% 0L)
            return(tibble::tibble(PANEL = as_factor(1L), ROW = 1L, COL = 1L, SCALE_X = 1L, SCALE_Y = 1L))

        # Create panel info dataset
        panel <- get_id(base)
        panel <- factor(panel, levels = seq_len(attr(panel, "n")))

        rows <-
            if (!vctrs::vec_size(names(rows)))
                vctrs::vec_repeat(1L, vctrs::vec_size(panel))
            else
                get_id(base[names(rows)])

        cols <-
            if (!vctrs::vec_size(names(cols)))
                vctrs::vec_repeat(1L, vctrs::vec_size(panel))
            else
                get_id(base[names(cols)])

        panels <- tibble::tibble(!!!append(list(PANEL = panel, ROW = rows, COL = cols), base))
        panels <- dplyr::arrange(panels, PANEL)
        rownames(panels) <- NULL

        panels$SCALE_X <- if (params$free$x) panels$COL else 1L
        panels$SCALE_Y <- if (params$free$y) panels$ROW else 1L

        panels
    },
    draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
        if ((params$free$x || params$free$y) && !coord$is_free())
            rlang::abort(
                paste0(
                    class(coord),
                    " doesn't support free scales"),
                "sciplotr_invalid_arg")

        cols <- which(layout$ROW %==% 1L)
        rows <- which(layout$COL %==% 1L)

        axes <- render_axes(ranges[cols], ranges[rows], coord, theme, transpose = TRUE)

        col_vars <- dplyr::distinct(layout[names(params$cols)])
        row_vars <- dplyr::distinct(layout[names(params$rows)])
        # Adding labels metadata, useful for labellers
        attr(col_vars, "type") <- "cols"
        attr(col_vars, "facet") <- "grid"
        attr(row_vars, "type") <- "rows"
        attr(row_vars, "facet") <- "grid"


        strips <- render_strips(col_vars, row_vars, params$labeller, theme)

        aspect_ratio <- theme$aspect.ratio
        if (rlang::is_null(aspect_ratio) && !params$free$x && !params$free$y)
            aspect_ratio <- coord$aspect(ranges[[1]])

        if (rlang::is_null(aspect_ratio)) {
            aspect_ratio <- 1
            respect <- FALSE
        }
        else
            respect <- TRUE

        ncol <- max(layout$COL)
        nrow <- max(layout$ROW)
        panel_table <- matrix(panels, nrow = nrow, ncol = ncol, byrow = TRUE)

        # @kohske
        # Now size of each panel is calculated using PANEL$ranges, which is given by
        # coord_train called by train_range.
        # So here, "scale" need not to be referred.
        #
        # In general, panel has all information for building facet.
        if (params$space_free$x) {
            ps <- dplyr::pull(dplyr::filter(layout, ROW %==% 1L), PANEL)
            widths <- purrr::map_dbl(ps, ~diff(ranges[[.x]]$x.range))
            panel_widths <- unit(widths, "null")
        }
        else
            panel_widths <- unit(vctrs::vec_repeat(1, ncol), "null")

        if (params$space_free$y) {
            ps <- dplyr::pull(dplyr::filter(layout, COL %==% 1L), PANEL)
            heights <- purrr::map_dbl(ps, ~diff(ranges[[.x]]$y.range))
            panel_heights <- unit(heights, "null")
        }
        else
            panel_heights <- unit(vctrs::vec_repeat(1 * aspect_ratio, nrow), "null")

        panel_table <-
            gtable::gtable_matrix(
                "layout", panel_table,
                panel_widths, panel_heights,
                respect = respect, clip = coord$clip,
                z = matrix(1L, ncol = ncol, nrow = nrow))

        panel_table$layout$name <-
            paste0(
                "panel-",
                vctrs::vec_repeat(seq_len(ncol), times = nrow),
                "-",
                vctrs::vec_repeat(seq_len(nrow), each = ncol))

        panel_table <- gtable::gtable_add_col_space(
            panel_table,
            theme$panel.spacing.x %||% theme$panel.spacing)
        panel_table <- gtable::gtable_add_row_space(
            panel_table,
            theme$panel.spacing.y %||% theme$panel.spacing)

        # Add axes
        panel_table <- gtable::gtable_add_rows(panel_table, ggplot2::max_height(axes$x$top), 0)
        panel_table <- gtable::gtable_add_rows(panel_table, ggplot2::max_height(axes$x$bottom), -1)
        panel_table <- gtable::gtable_add_cols(panel_table, ggplot2::max_width(axes$y$left), 0)
        panel_table <- gtable::gtable_add_cols(panel_table, ggplot2::max_width(axes$y$right), -1)
        panel_pos_col <- ggplot2::panel_cols(panel_table)
        panel_pos_rows <- ggplot2::panel_rows(panel_table)

        panel_table <- gtable::gtable_add_grob(panel_table, axes$x$top, 1, panel_pos_col$l, clip = "off", name = paste0("axis-t-", seq_along(axes$x$top)), z = 3)
        panel_table <- gtable::gtable_add_grob(panel_table, axes$x$bottom, -1, panel_pos_col$l, clip = "off", name = paste0("axis-b-", seq_along(axes$x$bottom)), z = 3)
        panel_table <- gtable::gtable_add_grob(panel_table, axes$y$left, panel_pos_rows$t, 1, clip = "off", name = paste0("axis-l-", seq_along(axes$y$left)), z = 3)
        panel_table <- gtable::gtable_add_grob(panel_table, axes$y$right, panel_pos_rows$t, -1, clip = "off", name = paste0("axis-r-", seq_along(axes$y$right)), z = 3)

        # Add strips
        ## Redo strip positions
        switch_x <- !rlang::is_null(params$switch) && vctrs::vec_in(params$switch, vctrs::vec_c("both", "x"))
        switch_y <- !rlang::is_null(params$switch) && vctrs::vec_in(params$switch, vctrs::vec_c("both", "y"))
        inside_x <- (theme$strip.placement.x %||% theme$strip.placement %||% "inside") == "inside"
        inside_y <- (theme$strip.placement.y %||% theme$strip.placement %||% "inside") == "inside"
        strip_padding <- grid::convertUnit(theme$strip.switch.pad.grid, "cm")
        panel_pos_col <- ggplot2::panel_cols(panel_table)

        if (!rlang::is_null(strips$x$bottom)) {
            panel_table <- gtable::gtable_add_rows(panel_table, strip_padding, -1)
            panel_table <- gtable::gtable_add_rows(panel_table, ggplot2::max_height(strips$x$bottom), -1)
            panel_table <- gtable::gtable_add_grob(panel_table, strips$x$bottom, -1, panel_pos_col$l, clip = "on", name = paste0("strip-b-", seq_along(strips$x$bottom)), z = 2)
        }
        if (!rlang::is_null(strips$x$top)) {
            panel_table <- gtable::gtable_add_rows(panel_table, strip_padding, 0)
            panel_table <- gtable::gtable_add_rows(panel_table, ggplot2::max_height(strips$x$top), 0)
            panel_table <- gtable::gtable_add_grob(panel_table, strips$x$top, 1, panel_pos_col$l, clip = "on", name = paste0("strip-t-", seq_along(strips$x$top)), z = 2)
        }

        panel_pos_rows <- ggplot2::panel_rows(panel_table)

        if (!rlang::is_null(strips$y$left)) {
            panel_table <- gtable::gtable_add_cols(panel_table, strip_padding, 0)
            panel_table <- gtable::gtable_add_cols(panel_table, ggplot2::max_width(strips$y$left), 0)
            panel_table <- gtable::gtable_add_grob(panel_table, strips$y$left, panel_pos_rows$t, 1, clip = "on", name = paste0("strip-l-", seq_along(strips$y$left)), z = 2)
        }
        if (!rlang::is_null(strips$y$right)) {
            panel_table <- gtable::gtable_add_cols(panel_table, strip_padding, -1)
            panel_table <- gtable::gtable_add_cols(panel_table, ggplot2::max_width(strips$y$right), -1)
            panel_table <- gtable::gtable_add_grob(panel_table, strips$y$right, panel_pos_rows$t, -1, clip = "on", name = paste0("strip-r-", seq_along(strips$y$right)), z = 2)
        }
        panel_table
    }
)


(mtcars %>%
    ggplot_sci(aes(x = hp, y = mpg, col = as_factor(cyl), shape = as_factor(gear))) +
    geom_point() +
    scale_x_sci(name = NULL, sec.axis = sec_axis_sci(~.)) +
    scale_y_sci(name = NULL, sec.axis = sec_axis_sci(~.)) +
    facet_sci(gear ~ am, # ncol = 1,
        #as.table = FALSE,
        scales = "free")
    ) %T>% { assign("temp_plot", ., envir = .GlobalEnv) } -> plt #%>%
#egg::expose_layout() %>%
#print

#egg::expose_layout(plt)
plt %>%
    postprocess_axes(
        axes_margin = mar_(h = u_(1.5 ~ cm), v = u_(1.5 ~ cm)),
        text_margin = mar_(h = u_(0 ~ null), v = u_(0 ~ null))
        ) -> tbl
grid.newpage()
grid.draw(tbl)
#print(tbl)
#print(convertX(sum(tbl$grobs[[7]]$children[[2]]$grobs[[2]]$x - tbl$grobs[[8]]$children[[2]]$grobs[[1]]$x), "native", TRUE))
#gtable::gtable_show_layout(tbl)