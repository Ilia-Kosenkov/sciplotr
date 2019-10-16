#   MIT License
#
#   Copyright(c) 2019 Ilia Kosenkov [ilia.kosenkov.at.gm@gmail.com]
#
#   Permission is hereby granted, free of charge, to any person obtaining a copy
#   of this software and associated documentation files(the "Software"), to deal
#   in the Software without restriction, including without limitation the rights
#   to use, copy, modify, merge, publish, distribute, sublicense, and / or sell
#   copies of the Software, and to permit persons to whom the Software is
#   furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission
#   notice shall be included in all
#   copies or substantial portions of the Software.
#
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
#   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
#   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
#   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH
#   THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

postprocess_axes <- function(
    gg_table,
    axes_margin = NULL, text_margin = NULL,
    unit_strategy = unit_max) {
    # Assuming label occupies exactly one grid cell

    x_lab_pos <- get_grobs_layout(gg_table, "xlab") %>%
        purrr::map_int(3)

    y_lab_pos <- get_grobs_layout(gg_table, "ylab") %>%
        purrr::map_int(1)

    if (rlang::is_null(text_margin)) {
        x_val <- unit_strategy(gg_table$heights[x_lab_pos])
        gg_table$heights[x_lab_pos] <- x_val

        y_val <- unit_strategy(gg_table$widths[y_lab_pos])
        gg_table$widths[y_lab_pos] <- y_val

    }
    else {
        gg_table$heights[x_lab_pos["xlab-t"]] <- at_(text_margin, t)
        gg_table$heights[x_lab_pos["xlab-b"]] <- at_(text_margin, b)

        gg_table$widths[y_lab_pos["ylab-l"]] <- at_(text_margin, l)
        gg_table$widths[y_lab_pos["ylab-r"]] <- at_(text_margin, r)
    }

    ax_t_pos <- get_grobs_layout(gg_table, "axis-t") %>%
        purrr::map_int(3)

    ax_b_pos <- get_grobs_layout(gg_table, "axis-b") %>%
        purrr::map_int(3)

    ax_l_pos <- get_grobs_layout(gg_table, "axis-l") %>%
        purrr::map_int(1)

    ax_r_pos <- get_grobs_layout(gg_table, "axis-r") %>%
        purrr::map_int(1)

    if (rlang::is_null(axes_margin)) {
        x_val <- unit_strategy(gg_table$heights[vctrs::vec_c(ax_t_pos, ax_b_pos)])
        gg_table$heights[vctrs::vec_c(ax_t_pos, ax_b_pos)] <- x_val

        y_val <- unit_strategy(gg_table$widths[vctrs::vec_c(ax_l_pos, ax_r_pos)])
        gg_table$widths[vctrs::vec_c(ax_l_pos, ax_r_pos)] <- y_val
    }
    else {
        gg_table$heights[ax_t_pos] <- at_(axes_margin, t)
        gg_table$heights[ax_b_pos] <- at_(axes_margin, b)

        gg_table$widths[ax_l_pos] <- at_(axes_margin, l)
        gg_table$widths[ax_r_pos] <- at_(axes_margin, r)
    }

    print(gg_table)
    print(gg_table$widths)
    print(gg_table$heights)
    gg_table
}

### Required
get_grob_ids_raw <- function(grid, pattern) {
    grid$layout %>%
        tibble::rowid_to_column %>%
        dplyr::filter(stringr::str_detect(name, pattern)) %>%
        dplyr::transmute(Name = name, Id = rowid) %>%
        as.list
}

### Required
get_grobs_layout <- function(grid, pattern) {
    vctrs::vec_assert(pattern, character(), 1L)

    get_grob_ids_raw(grid, pattern) %->% c(grob_names, ids)

    grid$layout %>%
        dplyr::slice(ids) %>%
        dplyr::mutate(data = purrr::pmap(list(l, r, t, b), vctrs::vec_c)) %>%
        dplyr::select(name, data) %$% {
            rlang::set_names(data, name) %>%
                purrr::map(~vctrs::allow_lossy_cast(vctrs::vec_cast(.x, integer())))
        }
}


### Required
get_cell_size <- function(grid, x, y) {

    vctrs::vec_assert(x, integer(), 1L)
    vctrs::vec_assert(y, integer(), 1L)
    assertthat::assert_that(
        x >= 1L, x <= length(grid$widths),
        y >= 1L, y <= length(grid$heights))

    x <- vctrs::vec_cast(x, integer())
    y <- vctrs::vec_cast(y, integer())

    grid::unit.c(grid$widths[x], grid$heights[y])
}


### Requried
get_grobs_size <- function(grid, pattern) {
    layout <- get_grobs_layout(grid, pattern)

    layout %>% purrr::map(~list(
            width = sum(grid$widths[seq(from = .x[1], to = .x[2], by = 1L)]),
            height = sum(grid$heights[seq(from = .x[3], to = .x[4], by = 1L)])))
}
