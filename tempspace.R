cube <- function(x,n) {
        x^3
}

x <- 1:10

if (x > 5) {
        x <- 0
}

x <- 5
y <- if (x < 3) {
        NA
} else {
        10
        
}





h <- function(x, y = NULL, d = 3L) {
        z <- cbind(x, d)
        print(z)
        print(x)
        print(y)
        print(d)
        if(!is.null(y))
                z <- z + y
        else
                z <- z + f
        g <- x + y / z
        if(d == 3L)
                return(g)
        g <- g + 10
        g
}


#
#  Scoping
#

a <- "Global"
f <- function() {
        
        g <- function(x) cat( x,a,"\n")
        g("called in f before assn of a:")
        a <- "in f"
        g("called in f after assn of a:")
        h <- function(x){
                a <- "defined in h"
                g(x)
        }
        h( "h calling g from in f:")
        a <- "redefined in f"
        h("h calling g in f after redefining a")
        rm(a)
        h("h calling g after removing a")
}

f()

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

makeVector <- function(x = numeric()) {
        m <- NULL
        print(environment())
        evn <- environment()
        print(parent.env(evn))
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        getevn<- function() environment()
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean,
             getevn = getevn)
}


a <- 5; b <- 1; c <- 4
f <- function (n) for (i in 1:n) d <- 1/{a*{b+c}}
g <- function (n) for (i in 1:n) d <- 1/(a*(b+c))
system.time(f(100 00 00))

x <- 1:4
