## Put comments here that give an overall description of what your
## functions do
#
# Example 3x3 invertible matrix
#
# m <- matrix(c(2,3,2,1,2,1,1,1,2), nrow=3,ncol=3)
# solve(m) gives inverse of matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # Initialize variable storing the inverse
    inv <- NULL
    #
    # Define function to set the matrix data in special cached matrix object
    #
    set <- function(y) {
        x <<- y
        #
        # Reset the matrix inverse when matrix data is updated.
        # This step will ensure that the matrix inverse will be recomputed.
        #
        inv <<- NULL
    }
    
    #
    # Define get function to return the matrix data
    #
    get <- function() x
    #
    # Define function to set the matrix inverse in the cached variable "inv" defined
    # in the parent environment.
    #
    setInverse <- function(inverse) inv <<- inverse
    #
    # Define function to return the matrix inverse
    #
    getInverse <- function() inv
    #
    # Special matrix object created using this function will return a list of functions
    # that can be used to manipulate the specail matrix object.
    #
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    #
    # Call the "getInverse" function on the special matrix object "x" passed as function arg
    #
    inv <- x$getInverse()
    #
    # Check if cached inverse value is returned by special matrix object
    #
    if (!is.null(inv)) {
        message("getting cached inverse")
        #
        # Return cached inverse value of matrix
        #
        return(inv)
    }
    #
    # Fetch the matrix data from the special matrix object "x"
    #
    matData <- x$get()
    #
    # Compute the matrix inverse
    #
    inv <- solve(matData)
    #
    # Set the matrix inverse value in the spcial matrix object
    #
    x$setInverse(inv)
    #
    # Return the matrix inverse
    #
    inv
}
