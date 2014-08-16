## Put comments here that give an overall description of what your
## functions do
#
# Example 3x3 invertible matrix
#
# m <- matrix(c(2,3,2,1,2,1,1,1,2), nrow=3,ncol=3)
# solve(m) gives inverse of matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    matData <- x$get()
    inv <- solve(matData)
    x$setInverse(inv)
    inv
}
