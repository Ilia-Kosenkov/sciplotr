
if (interactive()) {
    library(testthat)

    is_null <- rlang::is_null
    testthat::test_dir(file.path("tests", "testthat"))
} else {
    library(testthat)
    library(ggplot2)
    library(sciplotr)

    testthat::test_check("sciplotr", reporter = testthat::CheckReporter)
}
