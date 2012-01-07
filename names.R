#---------------------------------------------------------------------------
# Copyright (c) 2012 Takeshi Arabiki
# Licensed under the terms of the MIT License (see LICENSE.txt)
#---------------------------------------------------------------------------

`names<-` <- function(x, value, name = NULL) {
    'Description:

     Set the names of an object.

Usage:

     names(x) <- value
     names(x, name) <- value

Arguments:

       x: an R object.

    name: a character vector of names to be updated

   value: a character vector of new names

'
    if (is.null(name)) {
        base::`names<-`(x, value)
    } else {
        match.index <- match(name, names(x))
        if (length(name) != length(value)) {
            value <- rep(value, length = length(name))
            warning("length of 'value' differs from length of 'name'")
        }
        value <- value[!is.na(match.index)]
        names(x)[match.index[!is.na(match.index)]] <- value
        x
    }
}
