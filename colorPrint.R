#---------------------------------------------------------------------------
# URL https://github.com/abicky/R_funcs
# Copyright (c) 2012 Takeshi Arabiki
# Licensed under the terms of the MIT License (see LICENSE.txt)
#---------------------------------------------------------------------------
library(xtermStyle)

colorPrint <- function(df, condition, fields, style,
                       mode = c("ansi", "xterm-256color"), formats) {
    'Description:

     output data.frame with conditional formatting

Usage:

     colorPrint(df, condition, fields, style,
                mode = c("ansi", "xterm-256color"), formats)

Arguments:

      df: a data.frame

condition: logical expression indicating rows to apply conditional formatting.
            default is equal to rep(c(TRUE, FALSE), length = NROW(df))

  fields: not supported yet

   style: a named list of formatting values sent to xtermStyle::style.set.
          default is list(font.style = "inverse")

    mode: a character value sent to xtermStyle::style.mode

 formats: a list which contains lists which consit of "condition", "fields", and "style".
          use to specify multi-conditional formatting

Example:

     colorPrint(iris)

     colorPrint(iris, condition = Sepal.Length > 7,
                style = list(fg = "red"))

     colorPrint(iris, formats = list(
                        list(condition = Species == "setosa",
                             style     = list(font.style = "bold")),
                        list(condition = Species == "virginica",
                             style     = list(fg = "black", bg = "white")),
                        list(condition = TRUE,
                             style     = list(font.style = "underline"))))
'

    if (!is.data.frame(df)) {
        stop("'df' must be a data.frame!")
    }

    # initialization
    mode <- match.arg(mode)
    orig.mode <- style.mode(mode)
    if (is.null(orig.mode$style.mode)) {
        orig.mode$style.mode <- style.default.mode()
    }
    on.exit(style.mode(orig.mode))
    default.values <- list(targets = seq(length = NROW(df) / 2, by = 2),
                           fields  = names(df),
                           style   = list(font.style = "inverse"))

    # parse arguments
    if (missing(formats)) {
        tmp.format <- list()
        if (!missing(condition)) {
            e <- substitute(condition)
            r <- eval(e, df, parent.frame())
            if (!is.logical(r)) {
                stop("'condition' must evaluate to logical")
            }
            target <- which(r & !is.na(r))
            tmp.format$target <- target
        }
        if (!missing(fields)) {
            stop("'fields' is not supported yet")
        }
        if (!missing(style)) {
            tmp.format$style <- style
        }
        formats <- list(tmp.format)
    } else {
        formats <- .parseFormats(df, formats)
    }
    if (length(formats) == 0) {
        formats <- list(default.values)
    }

    # read data.frame
    out <- readDataFrame(df)

    # color data.frame
    for (cformat in formats) {
        style <- cformat$style
        if (is.null(style)) {
            style <- default.values$style
        }
        target <- cformat$target
        if (is.null(target)) {
            target <- default.values$target
        }
        out$body[target] <- style(out$body[target],
                                  fg = style$fg, bg = style$bg, font.style = style$font.style)
    }
    cat(paste(c(out$header, out$body), "\n"))
}

readDataFrame <- function(df) {
    tmpfile <- file()
    sink(tmpfile)
    print(df)
    sink()
    out <- list(header = readLines(tmpfile, 1),
                body   = readLines(tmpfile))
    close(tmpfile)
    out
}

.parseFormats <- function(df, formats) {
    # argument name which means formats must be 'formats'
    formats.sub <- substitute(formats, env = parent.frame())
    tmp.formats <- NULL

    format.names <- c("condition", "style", "fields")
    for (i in seq(length = length(formats.sub) - 1) + 1) {
        s <- .select(names(formats.sub[[i]]), format.names)
        # cf. subset.data.frame
        e <- formats.sub[[i]][[s["condition"]]]
        r <- eval(e, df, parent.frame(2))
        if (!is.logical(r) && !is.null(r)) {
            stop("'condition' must evaluate to logical")
        }
        if (is.null(r)) {
            target <- NULL
        } else {
            if (length(r) < NROW(df)) {
                r <- rep(r, length = NROW(df))
            }
            target <- which(r & !is.na(r))
        }
        style <- formats.sub[[i]][[s["style"]]]
        if (!is.null(formats.sub[[i]][[s["fields"]]])) {
            stop("'fields' is not supported yet")
        }
        tmp.formats <- c(tmp.formats, list(list(target = target, style = style)))
    }
    tmp.formats
}

# select longest match
.select <- function(x, targets) {
    matches <- pmatch(x, targets, dup = TRUE)
    x <- x[!is.na(matches)]
    select <- tapply(x, matches[!is.na(matches)], function(x) {
        x[which.max(nchar(x))]
    })
    lacks <- setdiff(seq_along(targets), as.numeric(names(select)))
    if (length(lacks) > 0) {
        names(select) <- targets[-lacks]
    } else {
        names(select) <- targets
    }
    # convert array to character
    c(select)
}
