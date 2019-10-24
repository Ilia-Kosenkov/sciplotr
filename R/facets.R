
# https://github.com/tidyverse/ggplot2/blob/269be6fe56a71bef2687ac4c1f39992de45ae87a/R/facet-grid-.r#L111
facet_sci <- function(rows = NULL, cols = NULL, scales = "fixed",
                      space = "fixed", shrink = TRUE,
                      labeller = "label_value", as.table = TRUE,
                      switch = NULL, drop = TRUE) {

    scales <- match.arg(scales, vctrs::vec_c("fixed", "free_x", "free_y", "free"))
    free <- list(
        x = any(vctrs::vec_in(scales, vctrs::vec_c("free_x", "free"))),
        y = any(vctrs::vec_in(scales, vctrs::vec_c("free_y", "free"))))

    space <- match.arg(space, vctrs::vec_c("fixed", "free_x", "free_y", "free"))
    space_free <- list(
        x = any(vctrs::vec_in(space, vctrs::vec_c("free_x", "free"))),
        y = any(vctrs::vec_in(space, vctrs::vec_c("free_y", "free"))))

    if (!is.null(switch) && !switch %in% c("both", "x", "y")) {
        stop("switch must be either 'both', 'x', or 'y'", call. = FALSE)
    }

    facets_list <- ggplot2:::grid_as_facets_list(rows, cols)

    # Check for deprecated labellers
    labeller <- ggplot2:::check_labeller(labeller)

    ggproto(NULL, FacetSci,
        shrink = shrink,
        params = list(rows = facets_list$rows, cols = facets_list$cols, margins = FALSE,
            free = free, space_free = space_free, labeller = labeller,
            as.table = as.table, switch = switch, drop = drop))
}

# https://github.com/tidyverse/ggplot2/blob/269be6fe56a71bef2687ac4c1f39992de45ae87a/R/facet-grid-.r#L189
FacetSci <- ggproto("FacetSci", FacetGrid,
  compute_layout = function(data, params) {
      rows <- params$rows
      cols <- params$cols

      dups <- intersect(names(rows), names(cols))
      if (length(dups) > 0) {
          stop(
        "Faceting variables can only appear in row or cols, not both.\n",
        "Problems: ", paste0(dups, collapse = "'"),
        call. = FALSE
      )
      }

      base_rows <- combine_vars(data, params$plot_env, rows, drop = params$drop)
      if (!params$as.table) {
          rev_order <- function(x) factor(x, levels = rev(ulevels(x)))
          base_rows[] <- lapply(base_rows, rev_order)
      }
      base_cols <- combine_vars(data, params$plot_env, cols, drop = params$drop)
      base <- ggplot2:::df.grid(base_rows, base_cols)

      if (nrow(base) == 0) {
          return(vctrs::new_data_frame(list(PANEL = 1L, ROW = 1L, COL = 1L, SCALE_X = 1L, SCALE_Y = 1L)))
      }

      # Add margins
      base <- reshape2::add_margins(base, list(names(rows), names(cols)), params$margins)
      # Work around bug in reshape2
      base <- unique(base)

      # Create panel info dataset
      panel <- id(base, drop = TRUE)
      panel <- factor(panel, levels = seq_len(attr(panel, "n")))

      rows <- if (!length(names(rows))) rep(1L, length(panel)) else id(base[names(rows)], drop = TRUE)
      cols <- if (!length(names(cols))) rep(1L, length(panel)) else id(base[names(cols)], drop = TRUE)

      panels <- new_data_frame(c(list(PANEL = panel, ROW = rows, COL = cols), base))
      panels <- panels[order(panels$PANEL),, drop = FALSE]
      rownames(panels) <- NULL

      panels$SCALE_X <- if (params$free$x) panels$COL else 1L
      panels$SCALE_Y <- if (params$free$y) panels$ROW else 1L

      panels
  },
  draw_panels = function(panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {
      if ((params$free$x || params$free$y) && !coord$is_free()) {
          stop(snake_class(coord), " doesn't support free scales", call. = FALSE)
      }

      cols <- which(layout$ROW == 1)
      rows <- which(layout$COL == 1)
      axes <- render_axes(ranges[cols], ranges[rows], coord, theme, transpose = TRUE)

      col_vars <- unique(layout[names(params$cols)])
      row_vars <- unique(layout[names(params$rows)])
      # Adding labels metadata, useful for labellers
      attr(col_vars, "type") <- "cols"
      attr(col_vars, "facet") <- "grid"
      attr(row_vars, "type") <- "rows"
      attr(row_vars, "facet") <- "grid"
      strips <- render_strips(col_vars, row_vars, params$labeller, theme)

      aspect_ratio <- theme$aspect.ratio
      if (is.null(aspect_ratio) && !params$free$x && !params$free$y) {
          aspect_ratio <- coord$aspect(ranges[[1]])
      }
      if (is.null(aspect_ratio)) {
          aspect_ratio <- 1
          respect <- FALSE
      } else {
          respect <- TRUE
      }
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
          ps <- layout$PANEL[layout$ROW == 1]
          widths <- vapply(ps, function(i) diff(ranges[[i]]$x.range), numeric(1))
          panel_widths <- unit(widths, "null")
      } else {
          panel_widths <- rep(unit(1, "null"), ncol)
      }
      if (params$space_free$y) {
          ps <- layout$PANEL[layout$COL == 1]
          heights <- vapply(ps, function(i) diff(ranges[[i]]$y.range), numeric(1))
          panel_heights <- unit(heights, "null")
      } else {
          panel_heights <- rep(unit(1 * aspect_ratio, "null"), nrow)
      }

      panel_table <- gtable_matrix("layout", panel_table,
      panel_widths, panel_heights, respect = respect, clip = coord$clip, z = matrix(1, ncol = ncol, nrow = nrow))
      panel_table$layout$name <- paste0('panel-', rep(seq_len(ncol), nrow), '-', rep(seq_len(nrow), each = ncol))

      panel_table <- gtable_add_col_space(panel_table,
      theme$panel.spacing.x %||% theme$panel.spacing)
      panel_table <- gtable_add_row_space(panel_table,
      theme$panel.spacing.y %||% theme$panel.spacing)

      # Add axes
      panel_table <- gtable_add_rows(panel_table, max_height(axes$x$top), 0)
      panel_table <- gtable_add_rows(panel_table, max_height(axes$x$bottom), -1)
      panel_table <- gtable_add_cols(panel_table, max_width(axes$y$left), 0)
      panel_table <- gtable_add_cols(panel_table, max_width(axes$y$right), -1)
      panel_pos_col <- panel_cols(panel_table)
      panel_pos_rows <- panel_rows(panel_table)

      panel_table <- gtable_add_grob(panel_table, axes$x$top, 1, panel_pos_col$l, clip = "off", name = paste0("axis-t-", seq_along(axes$x$top)), z = 3)
      panel_table <- gtable_add_grob(panel_table, axes$x$bottom, -1, panel_pos_col$l, clip = "off", name = paste0("axis-b-", seq_along(axes$x$bottom)), z = 3)
      panel_table <- gtable_add_grob(panel_table, axes$y$left, panel_pos_rows$t, 1, clip = "off", name = paste0("axis-l-", seq_along(axes$y$left)), z = 3)
      panel_table <- gtable_add_grob(panel_table, axes$y$right, panel_pos_rows$t, -1, clip = "off", name = paste0("axis-r-", seq_along(axes$y$right)), z = 3)

      # Add strips
      switch_x <- !is.null(params$switch) && params$switch %in% c("both", "x")
      switch_y <- !is.null(params$switch) && params$switch %in% c("both", "y")
      inside_x <- (theme$strip.placement.x %||% theme$strip.placement %||% "inside") == "inside"
      inside_y <- (theme$strip.placement.y %||% theme$strip.placement %||% "inside") == "inside"
      strip_padding <- convertUnit(theme$strip.switch.pad.grid, "cm")
      panel_pos_col <- panel_cols(panel_table)
      if (switch_x) {
          if (!is.null(strips$x$bottom)) {
              if (inside_x) {
                  panel_table <- gtable_add_rows(panel_table, max_height(strips$x$bottom), -2)
                  panel_table <- gtable_add_grob(panel_table, strips$x$bottom, -2, panel_pos_col$l, clip = "on", name = paste0("strip-b-", seq_along(strips$x$bottom)), z = 2)
              } else {
                  panel_table <- gtable_add_rows(panel_table, strip_padding, -1)
                  panel_table <- gtable_add_rows(panel_table, max_height(strips$x$bottom), -1)
                  panel_table <- gtable_add_grob(panel_table, strips$x$bottom, -1, panel_pos_col$l, clip = "on", name = paste0("strip-b-", seq_along(strips$x$bottom)), z = 2)
              }
          }
      } else {
          if (!is.null(strips$x$top)) {
              if (inside_x) {
                  panel_table <- gtable_add_rows(panel_table, max_height(strips$x$top), 1)
                  panel_table <- gtable_add_grob(panel_table, strips$x$top, 2, panel_pos_col$l, clip = "on", name = paste0("strip-t-", seq_along(strips$x$top)), z = 2)
              } else {
                  panel_table <- gtable_add_rows(panel_table, strip_padding, 0)
                  panel_table <- gtable_add_rows(panel_table, max_height(strips$x$top), 0)
                  panel_table <- gtable_add_grob(panel_table, strips$x$top, 1, panel_pos_col$l, clip = "on", name = paste0("strip-t-", seq_along(strips$x$top)), z = 2)
              }
          }
      }
      panel_pos_rows <- panel_rows(panel_table)
      if (switch_y) {
          if (!is.null(strips$y$left)) {
              if (inside_y) {
                  panel_table <- gtable_add_cols(panel_table, max_width(strips$y$left), 1)
                  panel_table <- gtable_add_grob(panel_table, strips$y$left, panel_pos_rows$t, 2, clip = "on", name = paste0("strip-l-", seq_along(strips$y$left)), z = 2)
              } else {
                  panel_table <- gtable_add_cols(panel_table, strip_padding, 0)
                  panel_table <- gtable_add_cols(panel_table, max_width(strips$y$left), 0)
                  panel_table <- gtable_add_grob(panel_table, strips$y$left, panel_pos_rows$t, 1, clip = "on", name = paste0("strip-l-", seq_along(strips$y$left)), z = 2)
              }
          }
      } else {
          if (!is.null(strips$y$right)) {
              if (inside_y) {
                  panel_table <- gtable_add_cols(panel_table, max_width(strips$y$right), -2)
                  panel_table <- gtable_add_grob(panel_table, strips$y$right, panel_pos_rows$t, -2, clip = "on", name = paste0("strip-r-", seq_along(strips$y$right)), z = 2)
              } else {
                  panel_table <- gtable_add_cols(panel_table, strip_padding, -1)
                  panel_table <- gtable_add_cols(panel_table, max_width(strips$y$right), -1)
                  panel_table <- gtable_add_grob(panel_table, strips$y$right, panel_pos_rows$t, -1, clip = "on", name = paste0("strip-r-", seq_along(strips$y$right)), z = 2)
              }
          }
      }
      panel_table
  }
)


(mtcars %>%
    ggplot_sci(aes(x = hp, y = mpg, col = as_factor(cyl), shape = as_factor(gear))) +
    geom_point() +
    scale_x_sci(sec.axis = sec_axis_sci(~.)) +
    scale_y_sci(sec.axis = sec_axis_sci(~.)) +
    facet_sci(rows = vars(gear),# ncol = 1,
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