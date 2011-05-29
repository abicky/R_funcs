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
        width <- apply(rbind(colnames(texts), texts), 2, function(strs) max(nchar(strs, type = "width")))
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
        # justify the vector lengths of each list element, and texts become a matrix
        texts <- mapply(function(x, n) c(as.character(x), rep("", n)), texts, max(len) - len)
    } else if (is.vector(texts) || is.factor(texts)) {
        texts <- as.matrix(texts)
    } else {
        if (!is.matrix(texts)) {
            stop(sprintf("Invalid argument: '%s'!", as.character(substitute(texts))))
        }
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
