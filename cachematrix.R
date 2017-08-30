##we are going to create two functions, one will make special matrix using 
## get and set functions and the other will calculate it's inverse if already not calculated.
##invertible matrix is assumed
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i<<-inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
    return(i)
}
        ## Return a matrix that is the inverse of 'x'
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
}
