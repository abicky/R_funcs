#---------------------------------------------------------------------------
# Copyright (c) 2012 Takeshi Arabiki
# Licensed under the terms of the MIT License (see LICENSE.txt)
#---------------------------------------------------------------------------

rank <- function(x, na.last = TRUE,
                 ties.method = c("average", "first", "random", "max", "min"),
                 decreasing = FALSE) {
    'Description:

     return the ranks of the values

Usage:

     rank(x, na.last = TRUE,
          ties.method = c("average", "first", "random", "max", "min"),
          decreasing = FALSE)

Arguments:

       x: same as rank function (See also: base::rank)

  na.last: same as rank function (See also: base::rank)

ties.method: same as rank function (See also: base::rank)

decreasing: logical, whether or not the values should be ranked in the decreasing order

'
    if (decreasing) {
        if (is.numeric(x) || is.complex(x)) {
            return(base::rank(-x, na.last, ties.method))
        } else {
            return(base::rank(-xtfrm(x), na.last, ties.method))
        }
    } else {
        return(base::rank(x, na.last, ties.method))
    }
}
