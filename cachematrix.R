## Put comments here that give an overall description of what your
## functions do

## makeCacheMatirx creates a special vector, which is a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, setmean = setmean, getmean = getmean)
}


## cacheSolve calculates the mean of the special "vector" created above.
## it first checks if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it solves for the inverse of the matrix and
## sets the inverse of the matrix in the cache via the setinv function.

cacheSolve <- function(x, ...){
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solv(data, ...)
    x$setinv(inv)
    inv
}
