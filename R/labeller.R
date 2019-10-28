debug_labeller <- `class<-`(function(labels) {
    print(labels)
    col_labs <- purrr::pmap(labels$cols, paste, sep = "; ")
    row_labs <- purrr::pmap(labels$rows, paste, sep = "; ")

    list(left = paste0(row_labs, "-left"),
         right = paste0(row_labs, "-right"),
         top = paste0(col_labs, "-top"),
         bottom = paste0(col_labs, "-bottom"))
}, "labeller")


label_f <- function(.f_left = NULL, .f_right = NULL, .f_top = NULL, .f_bottom = NULL) {
    .f_left <- if (rlang::is_null(.f_left)) rlang::as_function(~NULL) else rlang::as_function(.f_left)
    .f_right <- if (rlang::is_null(.f_right)) rlang::as_function(~NULL) else rlang::as_function(.f_right)
    .f_top <- if (rlang::is_null(.f_top)) rlang::as_function(~NULL) else rlang::as_function(.f_top)
    .f_bottom <- if (rlang::is_null(.f_bottom)) rlang::as_function(~NULL) else rlang::as_function(.f_bottom)

    `class<-`(function(labels) {
        list(left = .f_left(labels), right = .f_right(labels), top = .f_top(labels), bottom = .f_bottom(labels))
    }, "labeller")
}