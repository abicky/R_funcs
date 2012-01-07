#---------------------------------------------------------------------------
# Copyright (c) 2012 Takeshi Arabiki
# Licensed under the terms of the MIT License (see LICENSE.txt)
#---------------------------------------------------------------------------

sort.data.frame <- function(x, decreasing = FALSE, na.last = TRUE, order.by) {
    'Description:

     Sorting Data Frames

Usage:

     sort(x, decreasing = FALSE, na.last = TRUE, order.by)

Arguments:

       x: a data frame

decreasing: same as sort function (See also: base::sort)

 na.last: same as sort function (See also: base::sort)

order.by: expression, indicating columns to order a data frame by the columns

Examples:
     # sort in ascending order by all columns
     sort(iris)

     # sort in decreasing order by all columns
     sort(iris, decreasing = TRUE)

     # sort in ascending order by Species
     # and sort in decreasing order by Sepal.Length
     sort(iris, order.by = c(Species, -Sepal.Length)

'
    dec.index <- NULL
    if (missing(order.by)) {
        sort.fields <- names(x)
    } else {
        # cf. subset
        name.list <- as.list(seq_along(x))
        names(name.list) <- names(x)
        field.index <- eval(substitute(order.by), name.list, parent.frame())
        sort.fields <- names(name.list)[abs(field.index)]
        dec.index <- which(field.index < 0)
    }
    if (length(dec.index) > 0) {
        dec.fields <- sort.fields[dec.index]
        dec.fields <- sapply(dec.fields, function(field) {
            cmd <- sprintf("is.numeric(%s) || is.complex(%s)", field, field)
            if (!eval(parse(text = cmd), envir = x, enclos = NULL)) {
                field <- sprintf("xtfrm(%s)", field)
            }
            sprintf("-%s", field)
        })
        sort.fields[dec.index] <- dec.fields
    }
    cmd <- sprintf("order(%s, decreasing = %s)",
                   paste(sort.fields, collapse = ", "), as.character(decreasing))
    index <- eval(parse(text = cmd), envir = x, enclos = NULL)
    x[index, ]
}
