
if (interactive()) {
    library(testthat)
    library(ggplot2)
    library(magrittr)
    is_null <- rlang::is_null
    testthat::test_dir(file.path("tests", "testthat"))
} else {
    #library(testthat)
    library(sciplotr)
    library(ggplot2)
    library(magrittr)

    testthat::test_check("sciplotr")
}
