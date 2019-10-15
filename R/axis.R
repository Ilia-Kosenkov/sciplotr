dup_axis_sci <- function(
    trans = ~.,
    name = derive(),
    breaks = derive(),
    labels = derive(),
    breaks_trans = derive()) {

    sec_axis_sci(trans, name, breaks, labels, breaks_trans)
}

weak_dup_axis_sci <- function(
    trans = ~.,
    name = name_filler(),
    breaks = derive(),
    labels = labels_filler(),
    breaks_trans = derive()) {
    
    sec_axis_sci(trans, name, breaks, labels, breaks_trans)
}