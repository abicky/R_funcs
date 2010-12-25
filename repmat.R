#-------------------------------------------------------------------------------------#
# Copyright (c) 2010 Takeshi Arabiki                                                  #
# Licensed under the terms of the MIT License (see LICENSE.txt)                       #
#-------------------------------------------------------------------------------------#



#-------------------------------------------------------------------------------------#
# Description:                                                                        #
#    Replicate and tile array                                                         #
#                                                                                     #
# Usage:                                                                              #
#    repmat(A, m, n = m)                                                              #
#                                                                                     #
# Arguments:                                                                          #
#    A:      a vector, factor, matrix, array, list, or data.frame.                    #
#    m:      a nonnegative integer or nonnegative integer vector                      #
#            to specify the number of replicatoin about each dimension.               #
#    n:      an integer ignored if the number of dimensions of m is larger than 1.    #
#-------------------------------------------------------------------------------------#
library(abind)
repmat <- function(A, m, n = m) {
    if (missing(m))
      stop("Requires at least 2 inputs.")
    
    if (is.vector(A))
      A <- t(matrix(A))
    
    m[2] <- ifelse(is.na(m[2]), n, m[2])
    if (!is.int(m) || any(m <= 0))
      stop("'m' should be a nonnegative integer or a nonnegative integer vector!")
    
    d <- dim(A)
    nr <- d[1L]
    nc <- d[2L]
    
    if (length(d) <= 2) {
        if (length(m) == 2) {
            # about the same as array(A, m) even if length(A) == 1
            # firstest!
            tmpA <- matrix(t(A), nrow = nr * m[1], ncol = nc, byrow = TRUE)
            matrix(tmpA, nrow = nr * m[1], ncol = nc * m[2])
            # slower if number of rows is large?
            # matrix(apply(A, 1, rep, m[1]), nrow = nr * m[1], ncol = nc * m[2], byrow = T)
            # slower if number of columns is large?
            # matrix(apply(A, 2, rep, m[2]), nrow = nr * m[1], ncol = nc * m[2])
        } else {
            tmpA <- matrix(t(A), nrow = nr * m[1], ncol = nc, byrow = TRUE)
            tmpA <- matrix(tmpA, nrow = nr * m[1], ncol = nc * m[2])
            array(tmpA, c(nr * m[1], nc * m[2], m[-(1:2)]))
        }
    } else {
        for (i in 1:length(m)) {
            A <- abind(rep(list(A), m[i]), along = i)
        }
        A
    }
}

is.int <- function(n) {
    if (!is.numeric(n)) {
        FALSE
    } else {
        all(n %% 1 == 0)
    }
}
