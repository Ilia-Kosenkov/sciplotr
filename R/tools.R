log10_floor <- function(x) 10^floor(log10(x))

unique_f <- function(x, eps = 1L) {
  x <- vctrs::vec_cast(x, double())

  prod <- outer(x, x, primitiveR::are_equal_f, eps = eps)

  inds <- which(
    magrittr::equals(
      purrr::map_int(
        vctrs::vec_seq_along(x),
        ~ sum(prod[1:.x, .x])
      ),
      1L
    )
  )
  vctrs::vec_slice(x, inds)
}

outer_unique_which <- function(x, y, eps = 1L) {
  x <- vctrs::vec_cast(x, double())
  y <- vctrs::vec_cast(y, double())

  prod <- !outer(x, y, primitiveR::are_equal_f, eps = eps)
  list(
    x = which(apply(prod, 1, all)),
    y = which(apply(prod, 2, all))
  )
}

outer_unique <- function(x, y, eps = 1L) {
  ids <- outer_unique_which(x, y, eps)
  list(
    x = vctrs::vec_slice(x, ids$x),
    y = vctrs::vec_slice(y, ids$y)
  )
}

round_interval <- function(rng, by) {
  rng <- vctrs::vec_cast(rng, double())
  by <- vctrs::vec_assert(
    vctrs::vec_cast(by, double),
    size = 1L
  )

  by * vctrs::vec_c(floor(rng[1] / by), ceiling(rng[2] / by))
}

locate_inrange <- function(x, range) {
  test <-
    if (range[1] < range[2]) {
      function(x, l, r) {
        x >= l & x <= r
      }
    } else {
      function(x, l, r) {
        x <= l & x >= r
      }
    }

  tibble::tibble(
    l = range,
    r = dplyr::lead(range)
  ) %>%
    dplyr::mutate(
      id_l = 1L:(dplyr::n()),
      id_r = .data$id_l + 1L
    ) %>%
    dplyr::slice(-dplyr::n()) -> data

  purrr::map(
    x,
    ~ dplyr::filter(data, test(.x, l, r)) %>%
      dplyr::select(.data$id_l, .data$id_r) %>%
      dplyr::slice(1L) %>%
      purrr::flatten_int() %>%
      unname()
  )
}

df_grid <- function(rows, cols, margin = FALSE) {
  vars <- purrr::map(
    vctrs::vec_c(
      purrr::map(rows, unique),
      purrr::map(cols, unique)
    ),
    forcats::as_factor
  )

  if (margin) {
    vars <- purrr::map(vars, vctrs::vec_c, forcats::as_factor("(all)"))
  }

  tidyr::expand_grid(!!!vars)
}

get_id <- function(variables) {
  # `variables` is a `data.frame`
  lengths <- purrr::map_int(variables, vctrs::vec_size)
  vars <- dplyr::select(variables, which(lengths != 0L))
  vars_len <- vctrs::vec_size(vars)

  if (isTRUE(vars_len == 0L)) {
    n <- vctrs::vec_size(variables)
    return(structure(seq_len(n), n = n))
  }
  if (isTRUE(vars_len == 1L)) {
    return(get_id_var(vars[[1]]))
  }
  ids <- rev(purrr::map(variables, get_id_var))
  p <- vctrs::vec_size(ids)

  ndistinct <- purrr::map_int(ids, attr, "n")
  n <- prod(ndistinct)

  combs <- vctrs::vec_c(1, cumprod(vctrs::vec_slice(ndistinct, -p)))
  mat <- rlang::exec(cbind, !!!ids)
  res <- as.vector((mat - 1L) %*% combs + 1L)
  attr(res, "n") <- n

  get_id_var(res)
}

get_id_var <- function(x) {
  if (isTRUE(vctrs::vec_size(x) == 0)) {
    return(structure(integer(), n = 0L))
  }
  levels <- sort(unique(x), na.last = TRUE)
  id <- match(x, levels)
  n <- max(id)
  structure(id, n = n)
}

adjust_angle <- function(x, by = 180) {
  for (i in seq_len(length(x$children))) {
    x$children[[i]]$rot <- x$children[[i]]$rot + by
  }
  x
}

split_ex <- function(.data, col, name = NULL, keep = FALSE) {
  content <- dplyr::pull(.data, {{ col }})
  content <- vctrs::vec_recycle_common(!!!content)
  size <- vctrs::vec_size_common(!!!content)
  transposed <- purrr::map(seq_len(size), ~ purrr::map(content, .x))
  result <- purrr::map(transposed, ~ vctrs::vec_c(!!!.x))

  if (rlang::is_null(name) || rlang::is_empty(name)) {
    names <- paste0("Split_", seq_len(size))
  } else {
    names <- paste0(name, "_", seq_len(size))
  }

  result <- dplyr::bind_cols(rlang::set_names(result, names))

  if (!keep) {
    .data <- dplyr::select(.data, -{{ col }})
  }

  dplyr::bind_cols(.data, result)
}

#' @export
empty_labels <- function() {
  empty_seq
}

empty_seq <- function(x) {
  vctrs::vec_repeat(" ", vctrs::vec_size(x))
}

#' @export
lin_unit <- function(x0, x, y) {
  x0 <- vctrs::vec_cast(x0, double())
  x <- vctrs::vec_assert(
    vctrs::vec_cast(x, double()),
    size = 2L
  )
  assertthat::assert_that(isTRUE(length(y) == 2L))

  dx <- x[2] - x[1]
  dy <- y[2] - y[1]

  purrr::map(x0, ~ y[1] + dy / dx * (.x - x[1])) -> result
  if (grid::is.unit(y)) {
    rlang::exec(grid::unit.c, !!!result)
  } else {
    vctrs::vec_cast(result, vctrs::vec_ptype_common(!!!result))
  }
}
