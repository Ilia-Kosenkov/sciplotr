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

    grobs <- get_grobs_desc(gg_table, "lab") %>% print
        
    if (rlang::is_null(text_margin)) {
        inds <- grobs %>% filter(Type == "xlab") %>% pull(T)
        x_val <- grobs %>% filter(Type == "xlab") %>% pull(Height) %>% unit_strategy
        gg_table$heights[inds] <- x_val

        inds <- grobs %>% filter(Type == "ylab") %>% pull(L)
        y_val <- grobs %>% filter(Type == "ylab") %>% pull(Width) %>% unit_strategy
        gg_table$widths[inds] <- y_val

    }
    else {
        inds <- grobs %>% filter(Type == "xlab", Side == "t") %>% pull(T)
        gg_table$heights[inds] <- at_(text_margin, t)

        inds <- grobs %>% filter(Type == "xlab", Side == "b") %>% pull(T)
        gg_table$heights[inds] <- at_(text_margin, b)

        inds <- grobs %>% filter(Type == "ylab", Side == "l") %>% pull(L)
        gg_table$widths[inds] <- at_(text_margin, l)

        inds <- grobs %>% filter(Type == "ylab", Side == "r") %>% pull(L)
        gg_table$widths[inds] <- at_(text_margin, r)
    }

    #ax_t_pos <- get_grobs_layout(gg_table, "axis-t") %>%
        #purrr::map_int(3)

    #ax_b_pos <- get_grobs_layout(gg_table, "axis-b") %>%
        #purrr::map_int(3)

    #ax_l_pos <- get_grobs_layout(gg_table, "axis-l") %>%
        #purrr::map_int(1)

    #ax_r_pos <- get_grobs_layout(gg_table, "axis-r") %>%
        #purrr::map_int(1)

    #if (rlang::is_null(axes_margin)) {
        #x_val <- unit_strategy(gg_table$heights[vctrs::vec_c(ax_t_pos, ax_b_pos)])
        #gg_table$heights[vctrs::vec_c(ax_t_pos, ax_b_pos)] <- x_val

        #y_val <- unit_strategy(gg_table$widths[vctrs::vec_c(ax_l_pos, ax_r_pos)])
        #gg_table$widths[vctrs::vec_c(ax_l_pos, ax_r_pos)] <- y_val
    #}
    #else {
        #gg_table$heights[ax_t_pos] <- at_(axes_margin, t)
        #gg_table$heights[ax_b_pos] <- at_(axes_margin, b)

        #gg_table$widths[ax_l_pos] <- at_(axes_margin, l)
        #gg_table$widths[ax_r_pos] <- at_(axes_margin, r)
    #}


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

### Requried
at_ <- function(item, what) UseMethod("at_")
### Requried
`at_<-` <- function(item, what, value) UseMethod("at_<-")

### Requried
at_.margin <- function(mar, what) {
    what <- as.character(ensym(what))

    what <- match.arg(what, vec_c("top", "right", "bottom", "left"))
    pos <- switch(what,
            "top" = 1L,
            "right" = 2L,
            "bottom" = 3L,
            "left" = 4L)
    mar[pos] -> val
    mar_class_pos <- which("margin" == class(val))
    if (!is_empty(mar_class_pos))
        class(val) <- class(val)[-mar_class_pos]

    val
}

### Requried
`at_<-.margin` <- function(mar, what, value) {
    what <- ensym(what)

    with_mar(mar, !!what := ~value)
}

### Requried
get_grobs_desc <- function(grid, pattern) {
    layout <- get_grobs_layout(grid, pattern)
    grobs <- tibble::tibble(GrobName = names(layout))
    grobs %>%
        dplyr::mutate(Matches = purrr::map(GrobName,
            stringr::str_match, 
            stringr::regex("^(\\w*?)(?:-([trbl]))?(?:-(\\d+)-(\\d+))?$", ignore_case = TRUE))) %>%
        dplyr::mutate(Matches = purrr::map_chr(Matches, ~ paste(.x[1, -1], collapse = ":"))) %>%
        tidyr::separate(Matches, vctrs::vec_c("Type", "Side", "X", "Y"), sep = ":") %>%
        dplyr::mutate_at(dplyr::vars(X, Y), readr::parse_integer) %>%
        dplyr::mutate_at(dplyr::vars(Type, Side), forcats::as_factor) -> grobs

    
    layout %>%
        enframe %>%
        separate_1(value) %>%
        set_names(vctrs::vec_c("GrobName", "L", "R", "T", "B")) %>%
        mutate(
            Width = grid:::unit.list.from.list(
                map2(L, R,
                    ~ if (.x == .y) grid$widths[.x:.y] else sum(grid$widths[.x:.y]))),
            Height = grid:::unit.list.from.list(
                map2(T, B,
                    ~ if (.x == .y) grid$heights[.x:.y] else sum(grid$heights[.x:.y])))) -> layout

    grobs %>% inner_join(layout, by = "GrobName")
}
