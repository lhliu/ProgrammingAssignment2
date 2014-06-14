## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # declare matric as m
    m <- NULL   
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setMatric <- function(matric) {
        m <- matric  
    } 
    getMatric <- function() m
    list(set = set, get = get,
         setMatric = setMatric,
         getMatric = getMatric)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getMatric()
    if(!is.null(m)) {
        message("getting cached matric data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setMatric(m)
    m
}

matricSample <- function(matricRow, sampleMean, sampleSD) {
    v <- round(rnorm(matricRow*matricRow, sampleMean, sampleSD), digits=0)
    om <- matrix(v,nrow=matricRow)
    om
}