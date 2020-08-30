## Put comments here that give an overall description of what your
## functions do

## Comments: The following functions compute the inverse of the matrix 
##          and cash the data so that the computation is not repeated.

## Write a short comment describing this function
## Description: makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    INV <- NULL
    set <- function(y) {
        x <<- y
        INV <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) INV <<- inverse
    getinverse <- function() INV
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## Description: 
##      cacheSolve function computes the inverse of the special "matrix" returned 
##      by makeCacheMatrix above. If the inverse has already been calculated 
##      (and the matrix has not changed), then the cachesolve should retrieve 
##      the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    INV <- x$getinverse()
    if(!is.null(INV)) {
        message("getting cached data")
        return(INV)
    }
    data <- x$get()
    INV <- solve(data, ...)
    x$setinverse(INV)
    INV
}

}


#Example
exmatrix <- matrix( c(5, 1, 0,
                      3,-1, 2,
                      4, 0,-1), nrow=3, byrow=TRUE)

cacheSolve(makeCacheMatrix(exmatrix))