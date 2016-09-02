## M. Hall submission for Programming Assignment #2 (9/1/2016)
##
## These functions are intended to cache the inverse of a matrix
## This is for Programming Assignment #2 "Lexical Scoping" in R Programming

## The makeCacheMatrix function will enable setting & getting the value of the
## matrix, and setting & getting the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(sol) inv <<- sol
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve function will calculate the inverse of the matrix from the 
## above function, or will return the cached version if it already exists
## and input is unchanged

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
