#' @import rlang vctrs ggplot2 grid gtable
#' @importFrom tidyr      replace_na
#' @importFrom tibble     enframe
#' @importFrom scales     censor trans_new rescale
#' @importFrom stats      median
#' @importFrom dplyr      %>% inner_join mutate n pull filter
#' @importFrom purrr      imap map map_chr map2 map2_chr pluck detect_index
#' @importFrom primitiveR %==% %!=% are_equal_f are_same_all cc len %vin% lin
#' @importFrom primitiveR %===%
NULL

utils::globalVariables(c("."))