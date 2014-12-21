## The functions store the value of the matrix and of its inverse
## They will check if the inversion has

## This function stores the value of the x matrix and of its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inv <<- solve(x) 
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## This function tests for the existence of a cached matrix
## and returns the inverse of x

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data) 
    x$setinv(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
