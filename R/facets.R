## https://github.com/tidyverse/ggplot2/blob/269be6fe56a71bef2687ac4c1f39992de45ae87a/R/facet-grid-.r#L111
#' @title Fcet Sci
#' @description Generates a facet similar to \code{ggplot2::facet_grid}.
#' @param rows Rows variable (or a formula).
#' @param cols Columns variable.
#' @param scales Controls whetehre scales are shared between different panels.
#' @param space Controls the space.
#' @param shrink Controls behaviour depending on the statistics.
#' @param labeller A set of up to four \code{rlang}-lambdas that control how
#' different panel labels are generated based on the facetting variables.
#' @param as.table Controls the arrangement of plots.
#' @param rotate.y Should the y labels be rotated (compared to default behaviour of \code{facet_grid}).
#' @param margins Should the marginal (i.e. combined) plots be added to the layout?
#' @param panel.labels Controls whether panel labels are generated.
#' @param inner.ticks Controls whether inner ticks are generated.
#' @param panel.labeller Function that generated panel labels (i.e. (a), (b), (c), etc)
#' @param drop Should empty factor levels be dropped.
#'
#' @export
facet_sci <- function(rows = NULL, cols = NULL, scales = "fixed",
                      space = "fixed", shrink = TRUE,
                      labeller = label_f(.f_left = ~.x$rows), as.table = TRUE,
                      rotate.y = TRUE, margins = FALSE,
                      panel.labels = TRUE,
                      inner.ticks = TRUE,
                      panel.labeller = waiver(),
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

    ggplot2::ggproto(NULL, FacetSci,
        shrink = shrink,
        params = list(
            rows = facets_list$rows, cols = facets_list$cols,
            margins = margins,
            free = free, space_free = space_free,
            labeller = labeller,
            rotate_y = rotate.y, panel_labels = panel.labels,
            inner_ticks = inner.ticks,
            panel_labeller = panel.labeller,
            as.table = as.table, drop = drop))
}

utils::globalVariables(c("data", "cols"))


# https://github.com/tidyverse/ggplot2/blob/269be6fe56a71bef2687ac4c1f39992de45ae87a/R/facet-grid-.r#L189
#' @title FacetSci
#' @export
FacetSci <- ggplot2::ggproto("FacetSci", ggplot2::FacetGrid,
    compute_layout = function(data, params) {
        rows <- params$rows
        cols <- params$cols

        dups <- intersect(names(rows), names(cols))

        if (len(dups) > 0)
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

        if (params$inner_ticks %||% FALSE)
            stripped_axes <- nullify_axes_tick_labels(axes)

        col_vars <- dplyr::distinct(layout[names(params$cols)])
        row_vars <- dplyr::distinct(layout[names(params$rows)])
        # Adding labels metadata, useful for labellers
        attr(col_vars, "type") <- "cols"
        attr(col_vars, "facet") <- "grid"
        attr(row_vars, "type") <- "rows"
        attr(row_vars, "facet") <- "grid"

        ## Custom strips
        strips <- build_strip(col_vars, row_vars, params$labeller, theme, params$rotate_y)

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
            panel_widths <- grid::unit(widths, "null")
        }
        else
            panel_widths <- grid::unit(vctrs::vec_repeat(1, ncol), "null")

        if (params$space_free$y) {
            ps <- dplyr::pull(dplyr::filter(layout, COL %==% 1L), PANEL)
            heights <- purrr::map_dbl(ps, ~diff(ranges[[.x]]$y.range))
            panel_heights <- grid::unit(heights, "null")
        }
        else
            panel_heights <- grid::unit(vctrs::vec_repeat(1 * aspect_ratio, nrow), "null")

        panel_table <-
            gtable::gtable_matrix(
                "layout", panel_table,
                panel_widths, panel_heights,
                respect = respect, clip = coord$clip,
                z = matrix(1L, ncol = ncol, nrow = nrow))

        panel_table$layout$name <-
            paste0(
                "panel-",
                vctrs::vec_repeat(seq_len(ncol), each = nrow),
                "-",
                vctrs::vec_repeat(seq_len(nrow), times = ncol))

        ## Adding panel labs
        if (params$panel_labels %||% FALSE)
            panel_table <-
                gen_panel_labs_grobs(
                    panel_table,
                    make_panel_labs(layout[names(col_vars)], layout[names(row_vars)], params$panel_labeller),
                    theme)

        ## Inner axes
        if (params$inner_ticks %||% FALSE) {
            panel_pos_cols <- ggplot2::panel_cols(panel_table)$l
            for (i in seq_len(nrow - 1L)) {
                pos <- 3L * (i - 1L) + 1L
                panel_table <- gtable::gtable_add_rows(panel_table, npc_(0), pos)
                panel_table <- gtable::gtable_add_rows(panel_table, npc_(0), pos + 1L)
                panel_table <- gtable::gtable_add_grob(
                    panel_table,
                    stripped_axes$x$bottom,
                    pos + 1L, panel_pos_cols, clip = "off",
                    name = paste("axis-b", 1:ncol, i + 0L, sep = "-"),
                    z = 3)
                panel_table <- gtable::gtable_add_grob(
                    panel_table,
                    stripped_axes$x$top,
                    pos + 2L, panel_pos_cols, clip = "off",
                    name = paste("axis-t", 1:ncol, i + 1L, sep = "-"),
                    z = 3)

            }
            panel_pos_rows <- ggplot2::panel_rows(panel_table)$t
            for (i in seq_len(ncol - 1L)) {
                pos <- 3L * (i - 1L) + 1L
                panel_table <- gtable::gtable_add_cols(panel_table, npc_(0), pos)
                panel_table <- gtable::gtable_add_cols(panel_table, npc_(0), pos + 1L)
                panel_table <- gtable::gtable_add_grob(
                    panel_table,
                    stripped_axes$y$right,
                    panel_pos_rows, pos + 1L, clip = "off",
                    name = paste("axis-r", i + 0L, 1:nrow, sep = "-"),
                    z = 3)
                panel_table <- gtable::gtable_add_grob(
                    panel_table,
                    stripped_axes$y$left,
                    panel_pos_rows, pos + 2L, clip = "off",
                    name = paste("axis-l", i + 1L, 1:nrow, sep = "-"),
                    z = 3)

            }
        }

        # Add axes
        panel_table <- gtable::gtable_add_rows(panel_table, ggplot2::max_height(axes$x$top), 0)
        panel_table <- gtable::gtable_add_rows(panel_table, ggplot2::max_height(axes$x$bottom), -1)
        panel_table <- gtable::gtable_add_cols(panel_table, ggplot2::max_width(axes$y$left), 0)
        panel_table <- gtable::gtable_add_cols(panel_table, ggplot2::max_width(axes$y$right), -1)
        panel_pos_col <- ggplot2::panel_cols(panel_table)
        panel_pos_rows <- ggplot2::panel_rows(panel_table)

        panel_table <- gtable::gtable_add_grob(
            panel_table, axes$x$top, 1, panel_pos_col$l,
            clip = "off", name = paste("axis-t", seq_along(axes$x$top), 1L, sep = "-"), z = 3)
        panel_table <- gtable::gtable_add_grob(
            panel_table, axes$x$bottom, -1, panel_pos_col$l,
            clip = "off", name = paste("axis-b", seq_along(axes$x$bottom), nrow, sep = "-"), z = 3)
        panel_table <- gtable::gtable_add_grob(
            panel_table, axes$y$left, panel_pos_rows$t, 1,
            clip = "off", name = paste("axis-l", 1, seq_along(axes$y$left), sep = "-"), z = 3)
        panel_table <- gtable::gtable_add_grob(
            panel_table, axes$y$right, panel_pos_rows$t, -1,
            clip = "off", name = paste("axis-r", ncol, seq_along(axes$y$right), sep = "-"), z = 3)

        # Add strips
        panel_pos_col <- ggplot2::panel_cols(panel_table)

        panel_table <- gtable::gtable_add_rows(panel_table, unit_max(get_height(strips$x$bottom)), -1)
        panel_table <- gtable::gtable_add_rows(panel_table, unit_max(get_height(strips$x$top)), 0)
        
        if (!rlang::is_null(strips$x$bottom))
            panel_table <- gtable::gtable_add_grob(panel_table, strips$x$bottom, -1, panel_pos_col$l, clip = "on", name = paste0("strip-b-", seq_along(strips$x$bottom)), z = 2)
        if (!rlang::is_null(strips$x$top))
            panel_table <- gtable::gtable_add_grob(panel_table, strips$x$top, 1, panel_pos_col$l, clip = "on", name = paste0("strip-t-", seq_along(strips$x$top)), z = 2)

        panel_pos_rows <- ggplot2::panel_rows(panel_table)

        panel_table <- gtable::gtable_add_cols(panel_table, unit_max(get_width(strips$y$left)), 0)
        panel_table <- gtable::gtable_add_cols(panel_table, unit_max(get_width(strips$y$right)), -1)

        if (!rlang::is_null(strips$y$left))
            panel_table <- gtable::gtable_add_grob(panel_table, strips$y$left, panel_pos_rows$t, 1, clip = "on", name = paste0("strip-l-", seq_along(strips$y$left)), z = 2)
        if (!rlang::is_null(strips$y$right)) 
            panel_table <- gtable::gtable_add_grob(panel_table, strips$y$right, panel_pos_rows$t, -1, clip = "on", name = paste0("strip-r-", seq_along(strips$y$right)), z = 2)

        panel_table
    }
)

# https://github.com/tidyverse/ggplot2/blob/269be6fe56a71bef2687ac4c1f39992de45ae87a/R/labeller.r#L486
build_strip <- function(cols, rows, labeller, theme, rotate_y = TRUE) {
    labeller <- match.fun(labeller)

    element_x <- calc_element("strip.text.x", theme)
    element_y <- calc_element("strip.text.y", theme)

    ## For weird compatibility with `ggplot2:::ggstrip`
    
    labels <- purrr::map_if(labeller(list(cols = cols, rows = rows)), ~!(rlang::is_null(.x)),
                         ~ `dim<-`(unlist(.x), vctrs::vec_c(vctrs::vec_size(.x), 1L)))

    if (rlang::inherits_any(element_y, "element_blank")) {
        y_strips <- list(
            left = rep(list(ggplot2::zeroGrob()), vctrs::vec_size(rows)),
            right = rep(list(ggplot2::zeroGrob()), vctrs::vec_size(rows)))
    }
    else {
        
        gp_y <- grid::gpar(
            fontsize = element_y$size,
            col = element_y$colour,
            fontfamily = element_y$family,
            fontface = element_y$face,
            lineheight = element_y$lineheight)
        
        if (rlang::is_null(labels$left))
            left_grobs <-
                rep(list(ggplot2::zeroGrob()), vctrs::vec_size(rows))
        else {
            left_grobs <- ggplot2:::create_strip_labels(labels$left, element_y, gp_y)
            left_grobs <- ggplot2:::ggstrip(left_grobs, theme, element_y, gp_y, FALSE, "on")
        }

        element_y_rot <-
            if (rlang::inherits_any(element_y, "element_text") && rotate_y)
                adjust_angle(element_y)
            else
                element_y

        if (rlang::is_null(labels$right))
            right_grobs <-
                rep(list(ggplot2::zeroGrob()), vctrs::vec_size(rows))
        else {

            right_grobs <- ggplot2:::create_strip_labels(labels$right, element_y_rot, gp_y)
            right_grobs <- ggplot2:::ggstrip(right_grobs, theme, element_y_rot, gp_y, FALSE, "on")
        }

        y_strips <- list(left = left_grobs, right = right_grobs)
    }

    if (rlang::inherits_any(element_x, "element_blank")) {
        x_strips <- list(
            top = rep(list(ggplot2::zeroGrob()), vctrs::vec_size(cols)),
            bottom = rep(list(ggplot2::zeroGrob()), vctrs::vec_size(cols)))
    }
    else {

        gp_x <- grid::gpar(
            fontsize = element_x$size,
            col = element_x$colour,
            fontfamily = element_x$family,
            fontface = element_x$face,
            lineheight = element_x$lineheight)

        if (rlang::is_null(labels$bottom))
            bottom_grobs <-
                rep(list(ggplot2::zeroGrob()), vctrs::vec_size(cols))
        else {
            bottom_grobs <- ggplot2:::create_strip_labels(labels$bottom, element_x, gp_x)
            bottom_grobs <- ggplot2:::ggstrip(bottom_grobs, theme, element_x, gp_x, TRUE, "on")
        }

        if (rlang::is_null(labels$top))
            top_grobs <-
                rep(list(ggplot2::zeroGrob()), vctrs::vec_size(cols))
        else {
            top_grobs <- ggplot2:::create_strip_labels(labels$top, element_x, gp_x)
            top_grobs <- ggplot2:::ggstrip(top_grobs, theme, element_x, gp_x, TRUE, "on")
        }
        x_strips <- list(top = top_grobs, bottom = bottom_grobs)
    }

    return(list(x = x_strips, y = y_strips))
}

utils::globalVariables(c("Cols", "Rows"))

make_panel_labs <- function(cols, rows, .f) {
    assertthat::assert_that(vctrs::vec_size(cols) %==% vctrs::vec_size(rows))
    if (ggplot2:::is.waive(.f))
        .f <- ~paste0("(", letters[.x$Id], ")")
    else if (rlang::is_null(.f))
        .f <- ~""

    if (ncol(rows) %==% 0L)
        row_comb <- forcats::as_factor(rep(0, vctrs::vec_size(rows)))
    else
        row_comb <- interaction(rows, sep = ":")

    if (ncol(cols) %==% 0L)
        col_comb <- forcats::as_factor(rep(0, vctrs::vec_size(cols)))
    else
        col_comb <- interaction(cols, sep = ":")

    tbl <- dplyr::mutate(tibble::tibble(Cols = col_comb, Rows = row_comb),
                         ColId = as.integer(Cols),
                         RowId = as.integer(Rows))
    tbl <- dplyr::mutate(dplyr::arrange(tbl, Rows, Cols), Id = 1:n())

    .f <- rlang::as_function(.f)
    tbl <- dplyr::mutate(tbl, Label = purrr::pmap(tbl, ~.f(rlang::list2(...))))
}

utils::globalVariables(c("T", "R", "B", "L", "Label", "Name"))

gen_panel_labs_grobs <- function(panel, labs_table, theme) {

    elem <- calc_element("facet.lab", theme)
    x_pos <- calc_element("facet.lab.x", theme) %||% npc_(0.1)
    y_pos <- calc_element("facet.lab.y", theme) %||% npc_(0.9)
    panel_desc <- dplyr::select(get_grobs_desc(panel, "panel"), X, Y, L, R, T, B)

    panel_desc <- dplyr::inner_join(panel_desc, labs_table, by = c("X" = "ColId", "Y" = "RowId"))

    panel_desc <- dplyr::mutate(panel_desc, Name = paste("panel-lab", X, Y, sep = "-"))
    grob_desc <- purrr::pmap(dplyr::select(panel_desc, L, R, T, B, Label, Name), list)

    if (rlang::inherits_any(elem, "element_blank"))
        purrr::reduce(
            grob_desc,
            ~ gtable::gtable_add_grob(
                .x,
                zeroGrob(),
                t = .y$T, l = .y$L,
                r = .y$L, b = .y$B,
                name = .y$Name),
            .init = panel)
    else {
        gp <- grid::gpar(
            fontsize = elem$size,
            col = elem$colour,
            fontfamily = elem$family,
            fontface = elem$face)

        purrr::reduce(
            grob_desc,
            ~ gtable::gtable_add_grob(
                .x,
                textGrob(.y$Label,
                    x = x_pos,
                    y =  y_pos,
                    hjust = elem$hjust, vjust = elem$vjust,
                    rot = elem$angle %||% 0, gp = gp),
                t = .y$T, l = .y$L,
                r = .y$L, b = .y$B,
                name = .y$Name),
            .init = panel)
    }
}

nullify_axes_tick_labels <- function(axes_desc) {
    worker <- function(axes) {
        if (!rlang::is_null(axes)) {
            purrr::map_int(purrr::map(axes, "children"), purrr::detect_index, ~ !rlang::inherits_any(.x, "zeroGrob")) -> grob_pos
            grob_lists <- purrr::map2(axes, grob_pos, ~ purrr::pluck(.x, "children", .y, "grobs"))
            ids <- purrr::map_int(grob_lists, detect_index, rlang::inherits_any, "titleGrob")
            purrr::reduce2(
                grob_pos,
                seq_along(ids),
            ## This causes an error
                function(src, g_pos, id) {
                    path <- list(id, "children", g_pos, "grobs", ids[id])
                    if (rlang::is_null(pluck(src, !!!path)))
                        src
                    else
                        purrr::assign_in(src, path, ggplot2::zeroGrob())
                },
                .init = axes)
        }
        else
            axes
    }

    axes_desc <- purrr::assign_in(axes_desc, vctrs::vec_c("x", "bottom"), worker(purrr::pluck(axes_desc, "x", "bottom")))
    axes_desc <- purrr::assign_in(axes_desc, vctrs::vec_c("x", "top"), worker(purrr::pluck(axes_desc, "x", "top")))
    axes_desc <- purrr::assign_in(axes_desc, vctrs::vec_c("y", "left"), worker(purrr::pluck(axes_desc, "y", "left")))
    axes_desc <- purrr::assign_in(axes_desc, vctrs::vec_c("y", "right"), worker(purrr::pluck(axes_desc, "y", "right")))

}