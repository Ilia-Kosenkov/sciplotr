testthat::context("Test no fail")

#testthat::test_that("Plot is built without error", {
    mtcars %>%
        ggplot(aes(hp, mpg, col = as_factor(gear))) +
        geom_point() +
        theme_sci() +
        scale_x_sci(sec.axis = dup_axis_sci_weak()) +
        scale_y_sci(sec.axis = dup_axis_sci_weak()) +
        facet_grid(vs ~ am, scales = "free") -> plt

    print(plt)
#})