## Caching the Inverse of a Matrix

## makeCacheMatrix first check whether the passed matrix is invertible. If so,
## then creates a special list containing:
## 1. a function to set the value of the matrix, and set its inverse to be NULL
## 2. a function to get the value of the matrix
## 3. a function to set the inverse 
## 4. a function to get the inverse 
## NOTE: makeCacheMatrix won't do the calculation of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    if(det(x)==0){
        message("the matrix is not invertible")
    } else {
        invM <- NULL
        set <- function(y){
            x <<- y
            invM <<- NULL
        }
        get <- function() x
        setInvM <- function(solveM) invM <<-solveM
        getInvM <- function() invM
        list(set = set, get = get,
             setInvM = setInvM,
             getInvM = getInvM)
    }
}


## cacheSolve returns the inverse matrix either retrieved from cache or
## calculated within.

cacheSolve <- function(x, ...) {
    invM <- x$getInvM()
    if(!is.null(invM)) {
        message("getting cached data")
        return(invM)
    }
    data <- x$get()
    invM <- solve(data, ...)
    x$setInvM(invM)
    invM
}
