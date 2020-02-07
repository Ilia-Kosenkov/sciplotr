testthat::context("Test no fail")


testthat::setup({
    assign("tmp", tempfile(fileext = ".pdf"), envir = parent.frame())
    message(paste("Created temp file", tmp))
})

testthat::test_that("Plot is built without error", {
    mtcars %>%
        ggplot(aes(hp, mpg, col = as_factor(gear))) +
        coord_sci() +
        theme_sci() +
        geom_point() +
        scale_x_sci(sec.axis = dup_axis_sci()) +
        scale_y_sci(sec.axis = dup_axis_sci()) +
        facet_sci(vs ~ am, scales = "free",
            labeller = label_f(
                .f_left = ~paste("left", .x$rows[[1]]),
                .f_right = ~paste("right", .x$rows[[1]]),
                .f_bottom = ~paste("bottom", .x$cols[[1]]),
                .f_top = ~paste("top", .x$cols[[1]]))) -> plt
    message(paste("Using temp file", tmp))
    pdf(tmp)
    tryCatch({
        
        postprocess_axes(
            plt,
            strip_margin = 0.5 * mar_(1 ~ cm, 2 ~ cm, 3 ~ cm, 4 ~ cm),
            text_margin = 0.5 * mar_(4 ~ cm, 3 ~ cm, 2 ~ cm, 1 ~ cm),
            axes_margin = mar_(0.5 ~ cm, 0.5 ~ cm, 0.5 ~ cm, 0.5 ~ cm)) -> g
        grid::grid.newpage()
        grid::grid.draw(g)
        }, finally = dev.off())

    testthat::succeed("No errors were thrown.")
})

testthat::teardown({
    if (file.exists(tmp)) {
        file.remove(tmp)
        message(paste("Removed temp file", tmp))
    }
})