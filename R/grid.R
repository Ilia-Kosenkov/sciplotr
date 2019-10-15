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
    axes_margin = NULL, text_matgin = NULL) {
    # Assuming label occupies exactly one grid cell

    x_pos <- get_grobs_layout(gg_table, "xlab") %>%
        purrr::map_int(3)

    y_pos <- get_grobs_layout(gg_table, "ylab") %>%
        purrr::map_int(1)

    if (rlang::is_null(axes_margin)) {
        x_val <- unit_max(gg_table$heights[pos])
        gg_table$heights[pos] <- x_val

        y_val <- unit_max(gg_table$widths[pos])
        gg_table$widths[pos] <- y_val

    }
    else {
        gg_table$heights[x_pos["xlab-t"]] <- at_(axes_margin, t)
        gg_table$heights[x_pos["xlab-b"]] <- at_(axes_margin, b)

        gg_table$widths[y_pos["ylab-l"]] <- at_(axes_margin, l)
        gg_table$widths[y_pos["ylab-r"]] <- at_(axes_margin, r)
    }

    get_grobs_layout(gg_table, "axis") %>% print


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

get_grobs <- function(grid, pattern) {
    assert_that(passes(is_string(pattern)))
    assert_that(not(is_missing(grid)))

    get_grob_ids_raw(grid, pattern) %->% c(grob_names, ids)

    grid$grobs[ids] %>% set_names(grob_names)
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

### Requried
set_grid_height <- function(grid, y, unit) {

    vctrs::vec_assert(y, integer(), 1)
    assertthat::assert_that(y >= 1, y <= length(grdi$heights))
    assert_that(is.unit(unit), length(unit) == 1L)

    y <- vctrs::vec_cast(y, integer())

    grid$heights[y] <- unit
    grid
}

set_grobs_layout <- function(grid, pattern, layout) {
    assert_that(passes(is_string(pattern)))
    assert_that(not(is_empty(grid)))
    vec_assert(layout, integer(), 4L)

    get_grob_ids_raw(grid, pattern) %->% c(grob_names, ids)
    vec_c("l", "r", "t", "b") %>%
        syms %>%
        rlang::set_names(.) %>% 
        map2(layout, ~ quo(if_else_weak(vec_in(row_number(), ids), !!.y, !!(.x)))) -> exprs

    grid$layout %<>% mutate(!!!exprs)
    
    grid
}

set_grobs <- function(grid, pattern, grob = grid::nullGrob()) {
    assert_that(passes(is_string(pattern)))
    assert_that(not(is_empty(grid)))
    assert_that(not(is_empty(grob)))

    get_grob_ids_raw(grid, pattern) %->% c(grob_names, ids)

    grob <- rep(list(grob), vec_size(ids))
    grid$grobs[ids] <- grob
    grid
}

update_grobs <- function(grid, pattern, ...) {
    assert_that(passes(is_string(pattern)))
    assert_that(not(is_missing(grid)))

    args <- enquos(...)

    get_grob_ids_raw(grid, pattern) %->% c(grob_names, ids)

    args %>%
        names %>%
        str_split("\\$") %>%
        map_if(~not(vec_size(.x) == 1L && .x == "."), reduce, ~ glue("`$`({.x}, {.y})"), .init = ".") %>%
        flatten_chr %>%
        parse_exprs %>%
        map2(args, ~ quo(!!.x <<- !!f_rhs(.y))) %>% 
        map(quo_squash) -> exprs

    grid$grobs[ids] %<>% map(function(.) {
        env <- current_env()
        walk(exprs, eval_tidy, env = env)
        .
    })

    grid
}



set_grid_width <- function(grid, x, unit) {

    assert_that(not(is_empty(grid)))
    vec_assert(x, integer(), 1)
    assert_that(vec_within(x, 1, length(grid$widths)))
    assert_that(is.unit(unit), length(unit) == 1L)

    x <- vec_cast(x, integer())

    grid$widths[x] <- unit
    grid
}



remove_grid_column <- function(grid, column) {
    assert_that(not(is_empty(grid)))
    vec_assert(column, integer(), 1L)
    assert_that(vec_within(column, 1L, length(grid$widths)))

    grid$widths <- grid$widths[-column]

    grid$layout %<>%
       mutate_at(vars(l, r), ~if_else(. >= column, . - 1L, .))

    grid
}

insert_grid_column <- function(grid, column, width) {
    assert_that(not(is_empty(grid)))
    vec_assert(column, integer(), 1L)
    assert_that(vec_within(column, 1L, length(grid$widths)))
    assert_that(passes(is.unit(width)), length(width) == 1L)

    grid$widths[1L + (column:length(grid$widths))] <- grid$widths[column:length(grid$widths)]
    grid$widths[column] <- width

    grid$layout %<>%
       mutate_at(vars(l, r), ~ if_else(. >= column, . + 1L, .))

    grid
}

insert_grid_row <- function(grid, row, height) {
    assert_that(not(is_empty(grid)))
    vec_assert(row, integer(), 1L)
    assert_that(vec_within(row, 1L, length(grid$heights)))
    assert_that(passes(is.unit(height)), length(height) == 1L)

    grid$heights[1L + (row:length(grid$heights))] <- grid$heights[row:length(grid$heights)]
    grid$heights[row] <- height

    grid$layout %<>%
       mutate_at(vars(t, b), ~ if_else(. >= row, . + 1L, .))

    grid
}

remove_grid_row <- function(grid, row) {
    assert_that(not(is_empty(grid)))
    vec_assert(row, integer(), 1L)
    assert_that(vec_within(row, 1L, length(grid$heights)))

    grid$widths <- grid$heights[-row]

    grid$layout %<>%
        mutate_at(vars(t, b), ~if_else(. >= row, . - 1L, .))

    grid
}

set_grob_position <- function(grob, x, y) {
    assert_that(passes(is.unit(x)), length(x) == 1L)
    assert_that(passes(is.unit(y)), length(y) == 1L)
    if (is_null(grob))
        return(grob)


    if (!is_null(grob$x))
        grob$x <- x
    if (!is_null(grob$y))
        grob$y <- y

    if (!is_null(grob$vp)) {
        old_class <- class(grob$vp)
        grob$vp %<>% map(set_grob_position, x, y)
        class(grob$vp) <- old_class
    }

    if (!is_null(grob$children)) {
        old_class <- class(grob$children)
        grob$children %<>% map(set_grob_position, x, y)
        class(grob$children) <- old_class
    }

    grob
}

