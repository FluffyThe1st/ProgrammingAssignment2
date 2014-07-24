## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
