#-------------------------------------------------------------------------------------#
# Copyright (c) 2010-2011 Takeshi Arabiki                                             #
# Licensed under the terms of the MIT License (see LICENSE.txt)                       #
#-------------------------------------------------------------------------------------#



#-------------------------------------------------------------------------------------#
# Description:                                                                        #
#    output a vector, factor, matrix, list, or data.frame like MySQL                  #
#                                                                                     #
# Usage:                                                                              #
#    print.mysqlike(texts, index = FALSE, silent = FALSE, header = TRUE),             #
#                   sep = " | ", leftside = "| ", rightside = " |")                   #
#                                                                                     #
# Arguments:                                                                          #
#    texts:      a vector, factor, matrix, list, or data.frame.                       #
#    index:      logical, whether to add row numbers.                                 #
#    silent:     logical, whether to output the mysqlike class object.                #
#    header:     logical, whether to add the table header.                            #
#    sep:        a character string to separate the terms.                            #
#    leftside:   a character string appended to the left side.                        #
#                'side' is used by default.                                           #
#    rightside:  a character string added to the right side.                          #
#                'leftside' is used by default.                                       #
#-------------------------------------------------------------------------------------#
print.mysqlike <- function(texts, index = FALSE, silent = FALSE, header = TRUE,
                           sep = " | ", leftside = "| ", rightside = " |") {
    if (class(texts) != "mysqlike" || is.null(attr(texts, "formed"))) {
        texts <- convertMatrix(texts, index)
        # calculate max width for each column
        if (header) {
            width <- apply(rbind(colnames(texts), texts), 2, function(strs) max(nchar(strs, type = "width")))
        } else {
            width <- apply(texts, 2, function(strs) max(nchar(strs, type = "width")))
        }
        border <- sapply(width + 2, function(n) paste(rep("-", n), collapse = ""))
        border <- paste("+", paste(border, collapse = "+"), "+", sep = "")

        records <- apply(texts, 1, concat, width = width, sep = sep, leftside = leftside, rightside = rightside)
        
        if (header) {
            header <- concat(colnames(texts), width = width, sep = sep, leftside = leftside, rightside = rightside)
            header <- paste(border, header, sep = "\n")
            out <- paste(header, border, paste(records, collapse = "\n"), border, "", sep = "\n")
        } else {
            out <- paste(border, paste(records, collapse = "\n"), border, "", sep = "\n")
        }
        
        class(out) <- "mysqlike"
        attr(out, "formed") <- TRUE
    } else {
        out <- texts
    }
    
    if (!silent) {
        cat(out)
    }
    
    invisible(out)
}



#-------------------------------------------------------------------------------------#
# Description:                                                                        #
#    convert a vector, factor, list, data.frame to a matrix                           #
#                                                                                     #
# Usage:                                                                              #
#    convertMatrix(texts, index = FALSE)                                              #
#                                                                                     #
# Arguments:                                                                          #
#    texts:      a vector, factor, matrix, list, or data.frame.                       #
#    index:      logical, whether to add row numbers.                                 #
#-------------------------------------------------------------------------------------#
convertMatrix <- function(texts, index = FALSE) {
    if (mode(texts) == "list") {
        len <- sapply(texts, length)
        numIndex <- which(sapply(texts, is.numeric))
        # determine the number of significant digits on on each column
        texts[numIndex] <- sapply(texts[numIndex], num2printformat)

        # justify the vector lengths of each list element, and texts become a matrix
        texts <- mapply(function(x, n) c(as.character(x), rep("", n)), texts, max(len) - len)
        if (class(texts) == "character") {
            texts <- matrix(texts, 1, dimnames = list(NULL, names(texts)))
        }
    } else if (is.vector(texts) || is.factor(texts)) {
        texts <- as.matrix(texts)
    } else {
        if (!is.matrix(texts)) {
            stop(sprintf("Invalid argument: '%s'!", as.character(substitute(texts))))
        }
    }

    if (is.numeric(texts)) {
        # determine the number of significant digits on each column
        texts <- apply(texts, 2, num2printformat)
    }

    # add the column index if texts doesn't have the column name
    if (is.null(colnames(texts))) {
        colnames(texts) <- 1:ncol(texts)
    }
    # add the row number if index is TRUE
    if (index) {
        texts <- cbind(1:nrow(texts), texts)
    }
    texts
}



#-------------------------------------------------------------------------------------#
# Description:                                                                        #
#    concatenate R objects as characters                                              #
#                                                                                     #
# Usage:                                                                              #
#    concat(..., width = NULL, sep = "", side = sep, leftside, rightside, left)       #
#                                                                                     #
# Arguments:                                                                          #
#    ...:        R objects. each object is converted to a character.                  #
#    width:      a numeric vector to specify widths of each element                   #
#    sep:        a character string to separate the terms.                            #
#    side:       a character string appended to either side.                          #
#                'sep' is used by default.                                            #
#                This value is ignored if 'leftside' or 'rightside' are specified.    #
#    leftside:   a character string appended to the left side.                        #
#                'side' is used by default.                                           #
#    rightside:  a character string added to the right side.                          #
#                'leftside' is used by default.                                       #
#    left:       a logical vector, whether to left-align.                             #
#                The elements regarded as numeric are right-aligned                   #
#                and the others are left-aligned by default.                          #
#-------------------------------------------------------------------------------------#
concat <- function(..., width = NULL, sep = "", side = sep, leftside, rightside, left) {
    strs <- unlist(list(...))
    if (missing(leftside)) {
        leftside = side
    }
    if (missing(rightside)) {
        rightside = leftside
    }
    if (missing(left)) {
        left <- !grepl("^-?\\d[\\d,]*\\.?\\d*$", strs, perl = TRUE)
    }
    if (!is.null(width)) {
        strs <- pad(strs, width, left)
    }
    paste(leftside, paste(strs, collapse = sep), rightside, sep = "")
}



#-------------------------------------------------------------------------------------#
# Description:                                                                        #
#    fill spaces to justify the lengths of characters                                 #
#                                                                                     #
# Usage:                                                                              #
#    pad(strs, width, left = TRUE)                                                    #
#                                                                                     #
# Arguments:                                                                          #
#    strs:       a character vector.                                                  #
#    width:      a vector to specify widths of each element.                          #
#    left:       a logical vector, whether to left-align.                             #
#-------------------------------------------------------------------------------------#
pad <- function(strs, width, left = TRUE) {
    left <- rep(left, len = length(strs))
    ifelse(left,
           # append spaces to the right side
           paste(strs, sprintf("%*s", width - nchar(strs, type = "width"), ""), sep = ""),
           # append spaces to the left side
           paste(sprintf("%*s", width - nchar(strs, type = "width"), ""), strs, sep = ""))
}




#-------------------------------------------------------------------------------------#
# Description:                                                                        #
#    make scientific notation                                                         #
#                                                                                     #
# Usage:                                                                              #
#    as.SN(x, digits = 7)                                                              #
#                                                                                     #
# Arguments:                                                                          #
#    x:          a numeric vector.                                                    #
#    digits:     integer indicating the number of significant digits                  #
#                                                                                     #
# Example                                                                             #
#    x <- c(10.54389051435, 0.000000003429, 5483205992345, 0.002, 10)                 #
#    as.SN(x)                                                                         #
#    "1.054389e+01" "3.429000e-09" "5.483206e+12" "2.000000e-03" "1.000000e+01"       #
#                                                                                     #
#    x <- c(10.5, 0.00000000342, 548, 0.002, 10)                                      #
#    as.SN(x)                                                                         #
#    "1.05e+01" "3.42e-09" "5.48e+02" "2.00e-03" "1.00e+01"                           #
#-------------------------------------------------------------------------------------#
as.SN <- function(x, digits = NULL) {
    if (!is.numeric(x)) {
        stop("x is not numeric!")
    }

    x <- signif(x, 7)
    charX <- as.character(x)
    if (is.null(digits)) {
        # the number of significant digits
        digits <- max(nchar(as.numeric(sub("^(\\d+)\\.?(\\d*).*", "\\1\\2", charX))))
    } else {
        if (digits < 1) {
            digits <- 1
        }
    }

    if (digits == 1) {
        # e.g. 1e+01
        charLen <- 5
    } else {
        # e.g. 1.0e+01 (digits = 2)
        charLen <- digits + 5
        # numeric like 1e+10 fail in the padding phase
        charX <- sub("^(\\d)e", "\\1.0e", charX)
    }

    # make charX into scientific notation
    nonSN <- which(!grepl("e", charX))
    if (length(nonSN) != 0) {
        splitNum <- strsplit(charX[nonSN], c("\\."))
        eNum <- sapply(splitNum, function(x) {
            if (x[1] == "0") {
                -(nchar(sub("(0*).*", "\\1", x[2])) + 1)
        } else {
            nchar(x[1]) - 1
        }
        })
        charX[nonSN] <- paste(sprintf("%.*f", max(charLen) - 6, x[nonSN] / 10^(eNum)), "e", sprintf("%+03d", eNum), sep = "")
    }
    
    # justify the number of significant digits
    padding <- sapply(max(charLen) - nchar(charX), function(times) paste(c(rep("0", times), "e"), collapse = ""))
    charX <- mapply(function(x, pad) sub("e", pad, x), charX, padding)
    names(charX) <- names(x)
    
    charX
}



#-------------------------------------------------------------------------------------#
# Description:                                                                        #
#    convert numeric vector to print format character vector print format             #
#                                                                                     #
# Usage:                                                                              #
#    num2printformat(x)                                                               #
#                                                                                     #
# Arguments:                                                                          #
#    x:          a numeric vector.                                                    #
#-------------------------------------------------------------------------------------#
num2printformat <- function(x) {
    if (!is.numeric(x)) {
        stop("x is not numeric!")
    }

    charX <- as.character(x)
    
    if (any(grepl("e", charX))) {
        as.SN(x)
    } else {
        splitNum <- strsplit(charX, c("\\."))
        charLen <- sapply(splitNum, function(x) {
            if (length(x) == 1) {
                c(nchar(x), 0)
            } else {
                nchar(x)
            }
        })
        if (sum(apply(charLen, 1, max)) > 11) {
            as.SN(x)
        } else {
            sprintf("%.*f", max(charLen[2,]), x)
        }
    }
}
