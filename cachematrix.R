## Thaabit Nacerodien
## cacheMatrix.R
## Assignment 2
## R Programming Course

## This code was written to calculate the inverse of a matrix for the potential scenario where the computation of said
## matrix's inverse could be repeated often. As such, the value of the inverse is cached the first time it is calculated,
## Thereafter a check is performed to retrieve the value from the cache, instead of recalculating the same value. 

## The makeCacheMatrix function takes a matrix (invertible) as an argument and return a list of functions to set and get
## both the matrix and the inverse of the matrix. When the set functions are called, the arguments are stored outside
## the current environment; they are cached. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve function takes the list created by the makeCacheMatrix function as an argument. It first checks if the
## value of the inverse is stored in the cached variable. If the value is NULL, it will calculate the inverse using the 
## solve function and therafter store that value into the cached variable. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
