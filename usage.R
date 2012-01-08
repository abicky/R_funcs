#--------------------------------------------------------------------
# Copyright (c) 2011-2012 Takeshi Arabiki
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
usage <- structure(function(func.name, return.usage = FALSE) {
    usage.list <- attr(usage, "usage.list")
    if (missing(func.name)) {
        # return function names 'usage' has
        names(usage.list)
    } else {
        # use func.name as-is if func.name is a character object,
        # but is.character(func.name) will fail if func.name object does not exist
        func.name <- as.character(match.call()[2])
        penv <- parent.frame()
        default.msg <- NULL
        while (exists(func.name, envir = penv)) {
            # for functions like `foo<-`
            func.name <- sprintf("`%s`", func.name)
            tmp.func.name <- eval(parse(text = func.name), penv)
            if (is.character(tmp.func.name)) {
                func.name <- tmp.func.name
            } else if (is.function(tmp.func.name)) {
                src <- as.list(body(tmp.func.name))
                if (src[[1]] == "{" && length(src) > 2 && is.character(src[[2]])) {
                    default.msg <- src[[2]]
                }
                break
            } else {
                stop("func.name must be a function or function name character!")
            }
        }
        # strip backquotes
        func.name <- gsub("^`|`$", "", func.name)
        if (func.name %in% names(usage.list)) {
            msg <- usage.list[[func.name]]
        } else {
            if (is.null(default.msg)) {
                msg <- sprintf("No usage about '%s'", func.name)
            } else {
                msg <- default.msg
            }
        }
        if (return.usage) {
            ret <- msg
        } else {
            cat(msg, fill = TRUE)
            ret <- NULL
        }
        invisible(ret)
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

     usage()
     usage(func.name, return.usage = FALSE)
     usage$func.name
     usage$func.name <- "usage of func.name"

Arguments:

     func.name: function or function name character

  return.usage: logical, whether to return a usage as a characer value

Details:

     If the first line of a function is a character, the character is used
     as default usage of the function like R5 method.

')
}
