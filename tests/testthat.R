
if (interactive()) {
    library(testthat)
    library(ggplot2)
    library(magrittr)
    test_dir(file.path("tests", "testthat"))
} else {
    library(testthat)
    library(sciplotr)
    library(ggplot2)
    library(magrittr)

    test_check("sciplotr")
}
