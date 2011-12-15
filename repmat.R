#-------------------------------------------------------------------------------------#
# Copyright (c) 2010-2011 Takeshi Arabiki                                             #
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
repmat <- function(A, m, n = m) {
    if (missing(m))
      stop("Requires at least 2 inputs.")

    if (is.vector(A))
      A <- matrix(A, 1)

    if (length(m) == 1)
      m[2] <- n

    if (any(as.integer(m) != m) || any(m <= 0)) {
        stop("'m' should be a nonnegative integer or a nonnegative integer vector!")
    }

    d <- dim(A)
    if (length(d) <= 2) {
        nr <- d[1L]
        nc <- d[2L]
        # kronecker(array(1, m), A) is slower
        tmpA <- matrix(t(A), nrow = nr * m[1], ncol = nc, byrow = TRUE)
        A <- matrix(tmpA, nrow = nr * m[1], ncol = nc * m[2])
        if (length(m) > 2) {
            A <- array(tmpA, c(nr * m[1], nc * m[2], m[-(1:2)]))
        }
    } else {
        A <- kronecker(array(1, m), A)
    }

    A
}
