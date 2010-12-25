#-------------------------------------------------------------------------------------#
# Copyright (c) 2010 Takeshi Arabiki                                                  #
# Licensed under the terms of the MIT License (see LICENSE.txt)                       #
#-------------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------------#
# Description:                                                                        #
#    perform benchmark test                                                           #
#                                                                                     #
# Usage:                                                                              #
#    test(code, ic)                                                                   #
#                                                                                     #
# Arguments:                                                                          #
#    code:       R code for benchmark test                                            #
#    ic:        iteration count                                                       #
#                                                                                     #
# Examples:                                                                           #
#    test(sapply(1:100, rep, 1000))                                                   #
#                                                                                     #
#    test({                                                                           #
#        ret <- c()                                                                   #
#        for(i in 1:100) {                                                            #
#            ret <- cbind(rep(i, 1000))                                               #
#        }                                                                            #
#    })                                                                               #
#                                                                                     #
#    test({                                                                           #
#        ret <- c()                                                                   #
#        for(i in 1:100) {                                                            #
#            ret <- rbind(rep(i, 1000))                                               #
#        }                                                                            #
#        t(ret)                                                                       #
#    })                                                                               #
#                                                                                     #
#-------------------------------------------------------------------------------------#
test <- function(code, ic = 1000) {
    expr <- substitute(code)
    print(system.time(for (i in 1:ic) {
        eval(expr)
    }))   
}
