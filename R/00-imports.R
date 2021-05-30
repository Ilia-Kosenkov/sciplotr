#' @import                  rlang vctrs ggplot2 grid gtable
#' @importFrom tidyr        replace_na
#' @importFrom tibble       enframe
#' @importFrom scales       censor trans_new rescale rescale_max
#' @importFrom stats        median
#' @importFrom dplyr        %>% inner_join mutate n pull filter
#' @importFrom purrr        imap map map_chr map2 map2_chr pluck detect_index
#' @importFrom assertthat   assert_that
#' @importFrom primitiveR   %==% %!=% are_equal_f are_same_all cc len %vin% lin
#' @importFrom primitiveR   %===% %->%
NULL



utils::globalVariables(c("."))

.onLoad <- function(libname, pkgname) {
    register_theme_elements(
        sciplotr.axis.ticks.minor.length = unit(0, "pt"),
            sciplotr.axis.ticks.minor.length.x = NULL,
                sciplotr.axis.ticks.minor.length.x.bottom = NULL,
                sciplotr.axis.ticks.minor.length.x.top = NULL,
            sciplotr.axis.ticks.minor.length.y = NULL,
                sciplotr.axis.ticks.minor.length.y.left = NULL,
                sciplotr.axis.ticks.minor.length.y.right = NULL,

        element_tree = list(
            sciplotr.axis.ticks.minor.length = el_def("unit"),
                sciplotr.axis.ticks.minor.length.x = el_def("unit", "sciplotr.axis.ticks.minor.length"),
                    sciplotr.axis.ticks.minor.length.x.bottom = el_def("unit", "sciplotr.axis.ticks.minor.length.x"),
                    sciplotr.axis.ticks.minor.length.x.top = el_def("unit", "sciplotr.axis.ticks.minor.length.x"),
                sciplotr.axis.ticks.minor.length.y = el_def("unit", "sciplotr.axis.ticks.minor.length"),
                    sciplotr.axis.ticks.minor.length.y.left = el_def("unit", "sciplotr.axis.ticks.minor.length.y"),
                    sciplotr.axis.ticks.minor.length.y.right = el_def("unit", "sciplotr.axis.ticks.minor.length.y"))
    )

    register_theme_elements(
        sciplotr.facet.lab = element_text(),
        sciplotr.facet.lab.pos.x = unit(0.07, "npc"),
        sciplotr.facet.lab.pos.y = unit(0.93, "npc"),

        element_tree = list(
            sciplotr.facet.lab = el_def("element_text", "text"),
            sciplotr.facet.lab.pos.x = el_def("unit"),
            sciplotr.facet.lab.pos.y = el_def("unit"))
         )
}