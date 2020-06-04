knitr::opts_chunk$set(echo = TRUE, results = "asis")

#Programming assignment 1: Caching the mean 


makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}



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


#Programming assignment 2: Caching the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        set <- function(y) {
                x <<- y 
                inv <<- NULL 
        }

        get<- function() x 
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inv 
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
} 

cacheSolve <- function(x, ...){

        inv <- x$getInverse()
        if(!is.null(inv)){
                message("Getting cached data")
                return(inv)
        }

        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}