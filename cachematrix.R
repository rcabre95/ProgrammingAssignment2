## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# the "makeCacheMatrix" function creates a kind of matrix that sets the matrix as the inverse of the matrix instead

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# the function "cacheSolve" finds the inverse of the vector that "makeCacheMatrix" generated. it checks if an inverse already exists. if there is already an inverse, then it is skipped.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}