sciplotr_global <- env()

theme_gen <- function(name, class, inherit = NULL, description = NULL) {
    name <- rlang::quo_name(rlang::ensym(name))
    class <- rlang::quo_name(rlang::ensym(class))
    inherit <- rlang::quo_name(rlang::enquo(inherit))

    rlang::set_names(list(list(class = class, inherit = inherit, description = description)), name)
}

# https://github.com/tidyverse/ggplot2/blob/f16f16fbc69347aecb8756c226ee3e4ef8db58f5/R/theme.r#L539
calc_element <- function(element, theme, verbose = FALSE) {
    if (verbose) message(element, " --> ", appendLF = FALSE)

    # If this is element_blank, don't inherit anything from parents
    if (inherits(theme[[element]], "element_blank")) {
        if (verbose) message("element_blank (no inheritance)")
        return(theme[[element]])
    }

    ## WATCH: Modified element tree
    element_tree <- append(ggplot2:::ggplot_global$element_tree, sciplotr_global$element_tree)

    # If the element is defined (and not just inherited), check that
    # it is of the class specified in .element_tree
    if (!is.null(theme[[element]]) &&
        !inherits(theme[[element]], element_tree[[element]]$class)) {
        stop(element, " should have class ", element_tree[[element]]$class)
    }

    # Get the names of parents from the inheritance tree
    pnames <- element_tree[[element]]$inherit

    # If no parents, this is a "root" node. Just return this element.
    if (is.null(pnames)) {
        # Check that all the properties of this element are non-NULL
        nullprops <- vapply(theme[[element]], is.null, logical(1))
        if (any(nullprops)) {
            stop("Theme element '", element, "' has NULL property: ",
        paste(names(nullprops)[nullprops], collapse = ", "))
        }

        if (verbose) message("nothing (top level)")
        return(theme[[element]])
    }

    # Calculate the parent objects' inheritance
    if (verbose) message(paste(pnames, collapse = ", "))
    parents <- lapply(pnames, calc_element, theme, verbose)

    # Combine the properties of this element with all parents
    Reduce(ggplot2:::combine_elements, parents, theme[[element]])
}

assign(
    "element_tree",
    purrr::flatten(list(
        theme_gen(axis.ticks.minor.length, unit),
        theme_gen(axis.ticks.minor.length.x, unit, axis.ticks.minor.length),
        theme_gen(axis.ticks.minor.length.x.bottom, unit, axis.ticks.minor.length.x),
        theme_gen(axis.ticks.minor.length.x.top, unit, axis.ticks.minor.length.x),
        theme_gen(axis.ticks.minor.length.y, unit, axis.ticks.minor.length),
        theme_gen(axis.ticks.minor.length.y.left, unit, axis.ticks.minor.length.y),
        theme_gen(axis.ticks.minor.length.y.right, unit, axis.ticks.minor.length.y))),
    envir = sciplotr_global)