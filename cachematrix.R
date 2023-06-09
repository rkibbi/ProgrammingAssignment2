##The purpose of this experiment is to create a pair of functions, 
##named "makeCacheMatrix" and "cacheSolve," 
##with the intention of creating a special object that stores a matrix 
##and also caches the inverse of that matrix

##The function makeCacheMatrix generates a unique "matrix" object,
##this function possesses the capability to store its inverse for the provided input
##which should be a square matrix with invertible properties

makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  set <-function(y) {
    x <<- y
    inve <<- NULL
  }
  get <- function () x
    setinve <- function(inverse) inve <<- inverse
      getinve <- function () inve
        list (set = set, get = get, setinve = setinve, getinve = getinve)
}

## The function cacheSolve calculates the inverse of the special matrix
##returned by the makeCacheMatrix function. 
## If the inverse has already been calculated and the matrix remains the same,
## then the cacheSolve should fetch the inverse of the cache. 

cacheSolve <- function(x, ...) {
  ##Return a matrix that is the inverse of 'x'
  inve <- x$getinve ()
    if(!is.null(inve)) {
        message("getting cached result")
          return (inve)
    }
      data <- x$get()
        inve <- solve(data,...)
        x$setinve (inve)
        inve
}
