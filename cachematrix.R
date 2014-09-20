## There are two functions in this file.
## makeCacheMatrix creates a special matrix object that can cache its inverse.
## cacheSolve calculates the inverse of a matrix.

## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.
makeCacheMatrix <- function(x = matrix()) {
    inverseX <- NULL
    set <- function(y) {
        x <<- y
        inverseX <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inverseX <<-inverse
    getinverse <- function() inverseX
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
 

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache, else it will calculate the inverse,
## cache it and then return it.

cacheSolve <- function(x, ...) {
    ## Return the inverse matrix of x
    inverseX <- x$getinverse()
    if (!is.null(inverseX)) {
        message(“Retrieving cached inverse matrix")
        return(inverseX)
    } else {
        inverseX <- solve(x$get())
        x$setinverse(inverseX)
        return(inverseX)  
    }
}