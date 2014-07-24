## This functions together calculate the inverse matrix of a given matrix,
## caches the inverse matrix and return it.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
    invMat <- NULL
    set <- function(y) {
        x <<- y
        invMat <<- NULL
    }
  
    get <- function() x
    setinv <- function(solve) invMat <<- solve
    getinv <- function() invMat
  
    list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function computes the inverse matrix (invMat) of the 
## matrix (x) returned by makeCacheMatrix and returns the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    invMat <- x$getinv()
    if(!is.null(invMat)) {
        message("getting cached data")
        return(invMat)
    }
    
    data <- x$get()
    invMat <- solve(data, ...)
    x$setinv(invMat)
    return(invMat)
  
}
