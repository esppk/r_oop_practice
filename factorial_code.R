# factorial function
#loop version
factorial_loop <- function(x){
    stopifnot(x>0)
    ans <- 1
    for(i in 2:x){
        ans <- ans*i
    }
    ans
}
#reduc version
factorial_reduce <- function(x){
    stopifnot(x>0)
    require(purrr)
    reduce(c(1:x), function(x,y) x*y)
}
#recursive version
factorial_func <- function(x){
    stopifnot(x>0)
    if(x==1){
        x
    }else{
        x*factorial_func(x-1)
    }
}
#memorization version
factorial_mem <- function(n){
    ans <- c(1, rep(NA, n-1))
    mem_fac <- function(x){
        if(x == 1) return(1)
        if(!is.na(ans[x])){
            return(ans[x])
        }
        ans[x] <<- x*mem_fac(x-1)
        ans[x]
    }
    mem_fac(n)
}

library(purrr)
library(microbenchmark)
result <- map(c(5,10,50,100),function(x) microbenchmark(factorial_loop(x), factorial_func(x),factorial_reduce(x),factorial_mem(x)))
#write to disc
write.csv(map(result, summary), file = "result.csv")
