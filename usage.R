#--------------------------------------------------------------------
# Copyright (c) 2011 Takeshi Arabiki
# Licensed under the terms of the MIT License (see LICENSE.txt)
#--------------------------------------------------------------------

#--------------------------------------------------------------------
# Description:
#    show and set usage
#
# Usage:
#    usage()
#    usage(func.name)
#    usage$func.name
#    usage$func.name <- "usage of func.name"
#
# Arguments:
#    func.name:  function or function name character
#
# Examples:
#    usage()  # show function names 'usage' has
#    NULL
#    usage(hoge)
#    No usage about 'hoge'
#    usage$hoge
#    No usage about 'hoge'
#    NULL
#    usage("hoge")
#    No usage about 'hoge'
#    x <- "hoge"; usage(x)
#    No usage about 'hoge'
#
#    usage$hoge <- '
#    Description:
#         Show message, "this is hoge!"
#    Usage:
#         hoge()
#    '
#    hoge <- function() {
#        cat("this is hoge!\n")
#    }
#
#    usage(hoge)
#
#    Description:
#         Show message, "this is hoge!"
#    Usage:
#         hoge()
#
#
#    usage$hoge
#
#    Description:
#         Show message, "this is hoge!"
#    Usage:
#         hoge()
#
#    NULL
#
#
#    usage()
#    [1] "hoge"
#
#--------------------------------------------------------------------
usage <- structure(function(func.name) {
    usage.list <- attr(usage, "usage.list")
    if (missing(func.name)) {
        # return function names 'usage' has
        names(usage.list)
    } else {
        # use func.name as-is if func.name is a character object,
        # but is.character(func.name) will fail if func.name object does not exist
        func.name <- as.character(match.call()[2])
        penv <- parent.frame()
        if (exists(func.name, envir = penv)) {
            tmp.func.name <- eval(parse(text = func.name), penv)
            if (is.character(tmp.func.name)) {
                func.name <- tmp.func.name
            }
        }
        if (func.name %in% names(usage.list)) {
            cat(usage.list[[func.name]], fill = TRUE)
        } else {
            cat(sprintf("No usage about '%s'\n", func.name))
        }
        invisible(NULL)
    }
},
                   class = "usage",
                   usage.list = list())

`$<-.usage` <- function(x, name, value) {
    usage.list <- attr(x, "usage.list")
    usage.list[[name]] <- value
    attr(x, "usage.list") <- usage.list
    x
}

`$.usage` <- function(x, name) {
    x(name)
}

print.usage <- function(x) {
    cat('Description:

     Show or Set Usage about the Specified Function

Usage:

     usage(func.name)
     usage$func.name
     usage$func.name <- "usage of func.name"

Arguments:

     func.name: function or function name character

')
}
