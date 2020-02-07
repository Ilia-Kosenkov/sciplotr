testthat::context("Test no fail")

testthat::test_that("Plot is built without error", {
    mtcars %>%
        ggplot_sci(aes(hp, mpg, col = as_factor(gear))) +
        scale_x_sci(sec.axis = dup_axis_sci_weak()) +
        scale_y_sci(sec.axis = dup_axis_sci_weak()) +
        facet_sci(as ~ vm, scales = "free") -> plt

    print(plt)
})