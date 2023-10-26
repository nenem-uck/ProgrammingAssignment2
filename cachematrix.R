## Put comments here that give an overall description of what your
## functions do

## function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## store the inverse value
    i <- NULL
    ## set the original matrix and reset inverse
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## get the original matrix
    get <- function() x
    ## set the inverse
    setinv <- function(inv) i <<- inv
    ## get the inverse
    getinv <- function() i
    
    ## return a list of 4 functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
    ## check if an inverse already calculated in cache, if yes, return it
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## if no, solve it
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
        ## Return a matrix that is the inverse of 'x'
}
