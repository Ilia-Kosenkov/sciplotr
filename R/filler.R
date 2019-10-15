name_filler <- function() ""
labels_filler <- function() function(x) vctrs::vec_recycle("", vctrs::vec_size(x))