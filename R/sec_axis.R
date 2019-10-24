# https://github.com/tidyverse/ggplot2/blob/23e324197e0a5ddd764588d42838b0d96da9b68d/R/axis-secondary.R#L82
sec_axis_sci <- function(
    axis_trans = NULL,
    name = name_filler(),
    breaks = waiver(),
    labels = waiver(),
    breaks_trans = waiver()) {
    # sec_axis() historically accpeted two-sided formula, so be permissive.
    if (length(axis_trans) > 2) axis_trans <- axis_trans[c(1, 3)]

    axis_trans <- as_function(axis_trans)

    if (rlang::is_null(breaks_trans) || ggplot2:::is.waive(breaks_trans))
        breaks_trans <- identity_sci_trans()

    ggproto(NULL, AxisSecondarySci,
        #breaks_trans = breaks_trans,
        trans = axis_trans,
        name = name,
        breaks = breaks,
        labels = labels,
        guide = waiver(),
        temp_trans = breaks_trans
      )
}

# https://github.com/tidyverse/ggplot2/blob/fa000f786cb0b641600b6de68ae0f96e2ffc5e75/R/axis-secondary.R#L132
AxisSecondarySci <- ggproto("AxisSecondarySci", AxisSecondary,
    temp_trans = identity_sci_trans(),
    ###
    # Inherit settings from the primary axis/scale
    init = function(self, scale) {
        if (self$empty()) return()
        if (!is.function(self$trans)) stop("transformation for secondary axes must be a function", call. = FALSE)
        if (ggplot2:::is.derived(self$name) && !ggplot2:::is.waive(scale$name)) self$name <- scale$name
        if (ggplot2:::is.derived(self$breaks)) self$breaks <- scale$breaks
        if (ggplot2:::is.waive(self$breaks)) self$breaks <- scale$trans$breaks
        if (ggplot2:::is.derived(self$labels)) self$labels <- scale$labels
        if (ggplot2:::is.derived(self$guide)) self$guide <- scale$guide
    },

    ### ------------------ ###
    break_info = function(self, range, scale) {
        if (self$empty()) return()

        # Test for monotonicity on unexpanded range
        self$mono_test(scale)

        # Get scale's original range before transformation
        ## Original, equally spaced range
        ## `scale$trans` is `identity_sci`, appears to be inherited
        ## Both ranges are exactly equal for identity trans
        along_range <- seq(range[1], range[2], length.out = self$detail)
        old_range <- scale$trans$inverse(along_range)


        # Create mapping between primary and secondary range
        ## Applies transofrm to src range
        full_range <- self$transform_range(old_range)

        # Get break info for the secondary axis
        new_range <- range(full_range, na.rm = TRUE)
        # patch for date and datetime scales just to maintain functionality
        # works only for linear secondary transforms that respect the time or date transform
        if (scale$trans$name %in% c("date", "time")) {
            temp_scale <- self$create_scale(new_range, trans = scale$trans)
            range_info <- temp_scale$break_info()
            old_val_trans <- rescale(range_info$major, from = c(0, 1), to = range)
            old_val_minor_trans <- rescale(range_info$minor, from = c(0, 1), to = range)
        }
        else {

            temp_scale <- self$create_scale(new_range)
            ## Here the break info is obtained for the sec axis
            range_info <- temp_scale$break_info()

            # Map the break values back to their correct position on the primary scale
            old_val <- sapply(range_info$major_source, function(x) {
                which_less <- full_range < x
                index_lower <- which(which_less & (abs(full_range - x) == min(abs(full_range[which_less] - x))))

                which_greater <- full_range > x
                index_upper <- which(which_greater & abs(full_range - x) == min(abs(full_range[which_greater] - x)))

                index <- c(index_lower, index_upper)
                offset <- approx(full_range[index], c(0, 1), x)$y
                approx(c(0, 1), old_range[index], offset)$y
            })
            old_val_trans <- scale$trans$transform(old_val)


            old_val_minor <- sapply(range_info$minor_source, function(x) {
                which_less <- full_range < x
                index_lower <- which(which_less & (abs(full_range - x) == min(abs(full_range[which_less] - x))))

                which_greater <- full_range > x
                index_upper <- which(which_greater & abs(full_range - x) == min(abs(full_range[which_greater] - x)))

                index <- c(index_lower, index_upper)

                offset <- approx(full_range[index], c(0, 1), x)$y
                approx(c(0, 1), old_range[index], offset)$y
            })
            old_val_minor_trans <- scale$trans$transform(old_val_minor)

            ## Questionable rounding procedure
            # rescale values from 0 to 1
            range_info$major[] <- round(
                rescale(
                    scale$map(
                        old_val_trans,
                        range(old_val_trans)),
                        from = range),
                digits = 3)

            range_info$minor[] <- round(
                rescale(
                    scale$map(
                        old_val_minor_trans,
                        range(old_val_minor_trans)),
                        from = range),
                digits = 3)
        }

        # The _source values should be in (primary) scale_transformed space,
        # so that the coord doesn't have to know about the secondary scale transformation
        # when drawing the axis. The values in user space are useful for testing.
        range_info$major_source_user <- range_info$major_source
        range_info$minor_source_user <- range_info$minor_source
        range_info$major_source[] <- old_val_trans
        range_info$minor_source[] <- old_val_minor_trans

        names(range_info) <- paste0("sec.", names(range_info))
        range_info
    },

    ### ------------------ ###
    # Temporary scale for the purpose of calling break_info()
    ## The `trans` here is defaulted, so it can be controlled
    create_scale = function(self, range, trans = self$temp_trans) {
        ggproto_parent(AxisSecondary, self)$create_scale(range, trans)
    }
    ## `make_title` is trivial and is inherited
    ## `empry` is trivial and is inherited
)