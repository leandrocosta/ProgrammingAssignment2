## The following functions provide a way to store a matrix and its inverse in
## an object. The inverse is calculated only at the first call to cacheSolve().
## The matrix and its inverse are stored in the object created by makeCacheMatrix().

## This function creates an object that provides methods to store a matrix and
## its inverse. Each time the matrix is reset the inverse is set to NULL

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function receives an object x (created by the previous functon) that
## is used to store a matrix and its inverse. At the first time this function
## is called the inverse is calculated and stored in x. After that, each time
## the function is called it just returns the inverse matrix already calculated
## and stored in x.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
