sample_labeller <- `class<-`(function(labels) {
    col_labs <- purrr::pmap(labels$cols, paste, sep = "; ")
    row_labs <- purrr::pmap(labels$rows, paste, sep = "; ")

    list(left = paste0(row_labs, "-left"),
         right = paste0(row_labs, "-right"),
         top = paste0(col_labs, "-top"),
         bottom = paste0(col_labs, "-bottom"))
}, "Labeller")