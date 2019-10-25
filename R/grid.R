
postprocess_axes <- function(
    gg,
    axes_margin = NULL, text_margin = NULL,
    unit_strategy = unit_max) {

    UseMethod("postprocess_axes")
}

postprocess_axes.default <- function(
    gg,
    axes_margin = NULL, text_margin = NULL,
    unit_strategy = unit_max) {

    rlang::abort("Cannot process object of unsupported type.", "sciplotr_invalid_arg")

}

postprocess_axes.ggplot <- function(
    gg,
    axes_margin = NULL, text_margin = NULL,
    unit_strategy = unit_max) {

    postprocess_axes(
        ggplot2::ggplot_gtable(
            ggplot2::ggplot_build(
                gg)),
        axes_margin, text_margin, unit_strategy)
}

postprocess_axes.gtable <- function(
    gg,
    axes_margin = NULL, text_margin = NULL,
    unit_strategy = unit_max) {
    # Assuming label occupies exactly one grid cell

    grobs <- get_grobs_desc(gg, "lab")
        
    if (rlang::is_null(text_margin)) {
        inds <- grobs %>% filter(Type == "xlab") %>% pull(T)
        x_val <- grobs %>% filter(Type == "xlab") %>% pull(Height) %>% unit_strategy
        gg$heights[inds] <- x_val

        inds <- grobs %>% filter(Type == "ylab") %>% pull(L)
        y_val <- grobs %>% filter(Type == "ylab") %>% pull(Width) %>% unit_strategy
        gg$widths[inds] <- y_val

    }
    else {
        inds <- grobs %>% filter(Type == "xlab", Side == "t") %>% pull(T)
        gg$heights[inds] <- at_(text_margin, t)

        inds <- grobs %>% filter(Type == "xlab", Side == "b") %>% pull(T)
        gg$heights[inds] <- at_(text_margin, b)

        inds <- grobs %>% filter(Type == "ylab", Side == "l") %>% pull(L)
        gg$widths[inds] <- at_(text_margin, l)

        inds <- grobs %>% filter(Type == "ylab", Side == "r") %>% pull(L)
        gg$widths[inds] <- at_(text_margin, r)
    }

    grobs <- get_grobs_desc(gg, "axis")


    if (rlang::is_null(axes_margin)) {
        subset <- filter(grobs, vctrs::vec_in(Side, vctrs::vec_c("t", "b")))
        inds <- pull(subset, T)
        x_val <- unit_strategy(pull(subset, Height))
        gg$heights[inds] <- x_val

        subset <- filter(grobs, vctrs::vec_in(Side, vctrs::vec_c("l", "r")))
        inds <- pull(subset, L)
        y_val <- unit_strategy(pull(subset, Width))
        gg$widths[inds] <- y_val
    }
    else {
        subset <- grobs %>% filter(Side == "t")
        inds <- subset %>% filter(T %==% min(T)) %>% pull("T")
        gg$heights[inds] <- at_(axes_margin, t)

        inds <- subset %>% filter(T %!=% min(T)) %>% pull("T")
        if (vctrs::vec_size(inds) > 0)
            gg$heights[inds] <- u_(0 ~ null)

        subset <- grobs %>% filter(Side == "b")
        inds <- subset %>% filter(T %==% max(T)) %>% pull("T")
        gg$heights[inds] <- at_(axes_margin, b)

        inds <- subset %>% filter(T %!=% max(T)) %>% pull("T")
        if (vctrs::vec_size(inds) > 0)
            gg$heights[inds] <- u_(0 ~ null)


        subset <- grobs %>% filter(Side == "l")
        inds <- subset %>% filter(L %==% min(L)) %>% pull("L")
        gg$widths[inds] <- at_(axes_margin, l)

        inds <- subset %>% filter(L %!=% min(L)) %>% pull("L")
        if (vctrs::vec_size(inds) > 0)
            gg$widths[inds] <- u_(0 ~ null)


        subset <- grobs %>% filter(Side == "r")
        inds <- subset %>% filter(L %==% max(L)) %>% pull("L")
        gg$widths[inds] <- at_(axes_margin, r)

        inds <- subset %>% filter(L %!=% max(L)) %>% pull("L")
        if (vctrs::vec_size(inds) > 0)
            gg$widths[inds] <- u_(0 ~ null)
    }

    gg
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
get_grobs_desc <- function(grid, pattern) {
    layout <- get_grobs_layout(grid, pattern)
    grobs <- tibble::tibble(GrobName = names(layout))
    grobs %>%
        dplyr::mutate(Matches = purrr::map(GrobName,
            stringr::str_match, 
            stringr::regex("^(\\w*?)(?:-([trbl]))?(?:-(\\d+))?(?:-(\\d+))?$", ignore_case = TRUE))) %>%
        dplyr::mutate(Matches = purrr::map_chr(Matches, ~ paste(.x[1, -1], collapse = ":"))) %>%
        tidyr::separate(Matches, vctrs::vec_c("Type", "Side", "X", "Y"), sep = ":") %>%
        dplyr::mutate_at(dplyr::vars(X, Y), readr::parse_integer) %>%
        dplyr::mutate_at(dplyr::vars(X, Y), replace_na, 0L) %>%
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

get_height <- function(x, zero_if_null = TRUE) {
    if (grid::is.grob(x))
        return(grid::grobHeight(x))
    if (grid::is.unit(x))
        return(x)
    if (rlang::is_list(x))
        return(map(x, get_height))
    if (zero_if_null && rlang::is_null(x))
        return(u_(0 ~ null))

    rlang::abort("Unsupported input type", "sciplotr_invalid_arg")
}

get_width <- function(x, zero_if_null = TRUE) {
    if (grid::is.grob(x))
        return(grid::grobWidth(x))
    if (grid::is.unit(x))
        return(x)
    if (rlang::is_list(x))
        return(map(x, get_width))
    if (zero_if_null && rlang::is_null(x))
        return(u_(0 ~ null))

    rlang::abort("Unsupported input type", "sciplotr_invalid_arg")
}