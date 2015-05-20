## These functions are used to:
## - Make a special "matrix"
## - Calculate and cache the inverse of this "matrix"

## This function is used to make a special "matrix", which is indeed a list containing four functions to handle the matrix
##
## Example:
##
## > m <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
##
makeCacheMatrix <- function(x = matrix()) {
    ## Local variable that indeed caches the data
    s <-  NULL

    ## Local set function, used to set the matrix
    set <- function (y) {
        x <<- y
        s <<- NULL
    }

    ## Local get function, used to get the matrix
    get <- function() x

    ## Local setinverse function, used to set the inverse matrix
    setinverse <- function(inverse) s <<- inverse

    ## Local getinverse function, used to get the inverse matrix
    getinverse <- function() s

    ## function return, a list with the four functions above
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" created with the "makeCacheMatrix" function.
## However, it first checks to see if the inverse has already been calculated.
##
## Example:
##
## > m <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
## > cacheSolve(m)
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getinverse()
    
    ## Check if the value is cached. If so, it:
    ##  - Gets the inverse matrix from the cache;
    ##  - Skips the calculation;
    ##  - Show a message indicating this case.
    if (!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    ## Otherwise, it:
    ##  - Show a message indicating that the cache is empty;
    ##  - Gets the matrix and calculates its inverse;
    ##  - Sets the value in the cache.
    message("cache is empty. calculating...")
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    
    ## Return the inverse matrix
    s
}
