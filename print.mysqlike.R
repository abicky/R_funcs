#---------------------------------------------------------------------------
# URL https://github.com/abicky/R_funcs
# Copyright (c) 2010-2012 Takeshi Arabiki
# Licensed under the terms of the MIT License (see LICENSE.txt)
#---------------------------------------------------------------------------


print.mysqlike <- function(x, index = TRUE, header = TRUE, row.names = TRUE, digits = NULL,
                           file = "", silent = FALSE, border = TRUE,
                           sep = " | ", lside = "| ", rside = " |") {
    'Description:

     output a vector, factor, matrix, list, or data.frame like MySQL

Usage:

     print.mysqlike(x, index = TRUE, header = TRUE, row.names = TRUE, digits = NULL,
                    file = "", silent = FALSE, border = TRUE,
                    sep = " | ", lside = "| ", rside = " |")

Arguments:

       x: a vector, factor, matrix, list, or data.frame.

   index: logical, whether to add row numbers.

  header: logical, whether to add the table header.

row.names: logical, whether to add row names

  digits: a numeric vector to specify the digits

    file: A connection, or a character string naming the file to print to.

  silent: logical, whether to output the mysqlike class object.

  border: logical, whether to output the border.

     sep: a character string to separate the columns.

   lside: a character string appended to the left side.

   rside: a character string appended to the right side.

'

    if (class(x) != "mysqlike" || is.null(attr(x, "formed"))) {
        rnames <- NULL
        if (is.matrix(x) && row.names) {
            rnames <- rownames(x)
        }
        x <- printMysqlikeFuncs$convertMatrix(x, digits)
        # add the column index if x doesn't have the column name
        if (is.null(colnames(x))) {
            colnames(x) <- 1:ncol(x)
        }
        # add the row number if index is TRUE and add the row names if row.names is TRUE
        if (index && !is.null(rnames)) {
            x <- cbind(1:nrow(x), name = rnames, x)
        } else if (index) {
            x <- cbind(1:nrow(x), x)
        } else if (!is.null(rnames)) {
            x <- cbind(name = rnames, x)
        }

        # calculate max width for each column
        if (header) {
            width <- apply(rbind(colnames(x), x), 2, function(strs) max(nchar(strs, type = "width")))
        } else {
            width <- apply(x, 2, function(strs) max(nchar(strs, type = "width")))
        }
        border.line <- sapply(width + 2, function(n) paste(rep("-", n), collapse = ""))
        border.line <- paste("+", paste(border.line, collapse = "+"), "+", sep = "")

        records <- apply(x, 1, concat,
                         width = width, sep = sep, lside = lside, rside = rside)

        if (border) {
            out <- paste(border.line, paste(records, collapse = "\n"), border.line, "", sep = "\n")
        } else {
            out <- paste(paste(records, collapse = "\n"), "", sep = "\n")
        }
        if (header) {
            header <- concat(colnames(x), width = width, sep = sep, lside = lside, rside = rside)
            if (border) {
                header <- paste(border.line, header, sep = "\n")
            }
            out <- paste(header, out, sep = "\n")
        }

        class(out) <- "mysqlike"
        attr(out, "formed") <- TRUE
    } else {
        out <- x
    }

    if (!silent) {
        cat(out, file = file)
    }

    invisible(out)
}


concat <- function(..., width = NULL, left, sep = "", side = "",
                   lside = side, rside = side) {
    'Description:

     concatenate R objects as characters

Usage:
     concat(..., width = NULL, left, sep = "", side = "",
            lside = side, rside = side)

Arguments:

     ...: a R objects. each object is converted to a character.

   width: a numeric vector to specify widths of each element

    left: a logical vector, whether to left-align.
          The elements regarded as numeric are right-aligned
          and the others are left-aligned by default.

     sep: a character string to separate the terms.

    side: a character string appended to either side.
          This value is ignored if "lside" or "rside" are specified.

   lside: a character string appended to the left side.
          "side" is used by default.

   rside: a character string added to the right side.
          "side" is used by default.

'
    strs <- unlist(list(...))
    if (missing(left)) {
        left <- !is.numexp(strs)
    }
    if (!is.null(width)) {
        strs <- pad(strs, width, left)
    }
    paste(lside, paste(strs, collapse = sep), rside, sep = "")
}


pad <- function(strs, width = NULL, left = TRUE) {
    'Description:

     fill spaces to justify the lengths of characters

Usage:

     pad(strs, width = NULL, left = TRUE)

Arguments:

    strs: a character vector.

   width: a numeric vector to specify widths of each element.
          default is max(nchar(strs)).
          nchar(strs) is used if nchar(strs) > width

    left: a logical vector, whether to left-align.

'

    if (mode(strs) %in% c("character", "numeric")) {
        strs <- as.character(strs)
    } else {
        stop("'strs' must be character or numeric!")
    }
    if (!is.logical(left)) {
        stop("'left' must be logical!")
    }

    charLen <- nchar(strs)
    if (is.null(width)) {
        width <- max(charLen)
    } else if (is.numeric(width)) {
        width <- printMysqlikeFuncs$checkArgLength(width, length(charLen))
        target <- which(width - charLen < 0)
        width[target] <- charLen[target]
    } else {
        stop("'width' must be numeric!")
    }

    n <- length(strs)
    left <- printMysqlikeFuncs$checkArgLength(left, n)
    if (length(unique(left)) == 1) {
        if (left[1]) {
            # append spaces to the right side
            strs <- paste(strs, sprintf("%*s", width - nchar(strs, type = "width"), ""), sep = "")
        } else {
            # append spaces to the left side
            strs <- paste(sprintf("%*s", width - nchar(strs, type = "width"), ""), strs, sep = "")
        }
    } else {
        left <- which(left)
        width <- printMysqlikeFuncs$checkArgLength(width, n)
        # append spaces to the right side
        strs[left] <- paste(strs[left], sprintf("%*s", width[left] - nchar(strs[left], type = "width"), ""), sep = "")
        # append spaces to the left side
        strs[-left] <- paste(sprintf("%*s", width[-left] - nchar(strs[-left], type = "width"), ""), strs[-left], sep = "")
    }

    strs
}


as.SN <- function(x, digits = NULL, round.digits = NULL) {
    'Description:
     make scientific notation

Usage:

     as.SN(x, digits = NULL, round.digits = NULL)

Arguments:

       x: a numeric vector.

  digits: a numeric vector indicating the number of significant digits

round.digits: a numeric vector indicating the number of digits after decimal places

Examples:

     x <- c(10.54389051435, 0.000000003429, 5483205992345, 0.002, 10)
     as.SN(x)
     "1.054389e+01" "3.429000e-09" "5.483206e+12" "2.000000e-03" "1.000000e+01"

     x <- c(10.5, 0.00000000342, 548, 0.002, 10)
     as.SN(x)
     "1.05e+01" "3.42e-09" "5.48e+02" "2.00e-03" "1.00e+01"

'

    if (is.character(x) && all(printMysqlikeFuncs$is.numericchar(x))) {
        x <- as.numeric(x)
    }

    if (!is.numeric(x)) {
        stop("x is not numeric!")
    }

    if (is.null(digits)) {
        if (!is.null(round.digits)) {
            if (is.numeric(round.digits)) {
                x <- round(x, round.digits)
            } else {
                stop("'round.digits' is must be numeric!")
            }
        }
        # strip non-numeric characters
        numbers <- gsub("^-?(?:0\\.0*|(\\d+)\\.(\\d*)(?:e.*)?)?", "\\1\\2", as.character(x))
        # the number of significant digits
        n <- 6
        digits <- min(n, max(nchar(numbers)) - 1)
    } else if (!is.numeric(digits)) {
        stop("'digits' is must be numeric!")
    } else if (!is.null(round.digits)) {
        warning("'round.digits' is ignored.")
    }

    # digits is 0 if the numeric vector consists of only single digit
    if (all(abs(x) %in% 0:9)) {
        digits <- 0
    }

    if (any(x < 0)) {
        sprintf("% 2.*e", digits, x)
    } else {
        sprintf("%.*e", digits, x)
    }
}


is.numexp <- function(strs) {
    'Description:
     detect numeric representations

Usage:

     is.numexp(strs)

Arguments:

    strs: a character vector

Examples:

     strs <- c("1234", "1234a", "1,234", "1,2,34")
     is.numexp(strs)
     TRUE FALSE  TRUE FALSE

'

    grepl("(?x)^\\s*[-+]?
           (?:
               # begin with number
               (?:
                  \\d+  # without commas
                  | (?: 0[1-9]{1,2} | 00[1-9] | [1-9]\\d{0,2} )(?: ,\\d{3} )+  # with commas
               )
               (?: \\.\\d* )?
               # begin with dot
               | \\.\\d+
           )
           (?i: e[-+]\\d* )?  # scientific notation
           \\s*$", strs, perl = TRUE)
}


# other functions
printMysqlikeFuncs <- list(
convertMatrix = function(x, digits = NULL) {
    'Description:
     convert a vector, factor, list, data.frame to a matrix

Usage:

     printMysqlikeFuncs$convertMatrix(x, digits = NULL)

Arguments:

       x: a vector, factor, matrix, list, or data.frame.

  digits: a numeric vector indicating the number of significant digits
'

    if (mode(x) == "list") {
        len <- sapply(x, length)
        numIndex <- which(sapply(x, is.numeric))

        # determine the number of significant digits on each column
        if (is.null(digits)) {
            x[numIndex] <- sapply(x[numIndex], printMysqlikeFuncs$num2printformat)
        } else {
            digits <- printMysqlikeFuncs$checkArgLength(digits, length(x))
            x[numIndex] <- mapply(printMysqlikeFuncs$num2printformat,
                                  x[numIndex], digits[numIndex], SIMPLIFY = FALSE)
        }

        # justify the vector lengths of each list element, and x become a matrix
        rnames <- rownames(x)
        x <- mapply(function(x, n) c(as.character(x), rep("", n)), x, max(len) - len)
        if (!is.matrix(x)) {
            x <- t(as.matrix(x))
        }
        rownames(x) <- rnames
    } else if (is.vector(x) || is.factor(x) || is.matrix(x)) {
        x <- as.matrix(x)
        # in case that original x is vector or matrix
        if (is.numeric(x)) {
            dnames <- dimnames(x)
            # determine the number of significant digits on each column
            if (is.null(digits)) {
                x <- apply(x, 2, printMysqlikeFuncs$num2printformat)
            } else {
                digits <- printMysqlikeFuncs$checkArgLength(digits, ncol(x))
                x <- mapply(printMysqlikeFuncs$num2printformat,
                            as.data.frame(x), digits, USE.NAMES = FALSE)
            }
            if (!is.matrix(x)) {
                x <- t(as.matrix(x))
            }
            dimnames(x) <- dnames
        }
    } else if (!is.matrix(x)) {
        stop("Invalid argument: 'x'!")
    }

    x
},

is.numericchar = function(str) {
    if (is.character(str)) {
        num <- suppressWarnings(as.numeric(str))
        !is.na(num)
    } else {
        rep(FALSE, length(str))
    }
},

num2printformat = function(x, digits = NULL) {
    'convert numeric vector to print format character vector print format
e.g. num2printformat(c(1, 1.2)) --> c("1.0", "1.2")
num2printformat(c(100000, 0.0001)) --> c("1e+05", "1e-04")'

    if (!is.numeric(x)) {
        stop("x is not numeric!")
    }

    if (!is.null(digits)) {
        x <- round(x, digits)
    }
    charX <- as.character(x)
    if (any(grepl("e", charX))) {
        ret <- as.SN(x, round.digits = digits)
    } else {
        splitNum <- strsplit(charX, c("\\."))
        charLen <- sapply(splitNum, function(x) {
            if (length(x) == 1) {
                c(nchar(x), 0)
            } else {
                nchar(x)
            }
        })
        maxLen <- apply(charLen, 1, max)
        if (sum(maxLen) > 11) {
            if (!is.null(digits) && maxLen[1] > digits) {
                digits <- 6
            }
            ret <- as.SN(x, digits)
        } else {
            if (is.null(digits)) {
                ret <- sprintf("%.*f", maxLen[2], x)
            } else {
                ret <- sprintf("%.*f", digits, x)
            }
        }
    }
    ret
},

checkArgLength = function(arg, n) {
    'check "arg" length and repeat it until its length is n'
    n <- suppressWarnings(as.integer(n))
    if (is.na(n) || n <= 0) {
        stop("'n' is invalid!")
    }

    len <- length(arg)
    if (n %% len != 0 || n < len) {
        warning(sprintf("'arg' length [%d] is not a multiple of the number of data [%d]", len, n))
    }

    rep(arg, length = n)
})
