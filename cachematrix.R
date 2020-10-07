## There are two functions described below. They are responsible for caching the inverse of a matrix, which is a potentially time-consuming operation.
## To cache the inverse of a matrix is to avoid repeated costly matrix inversions, in the event where re-computations become necessary (for example, while conducting loops).In such a scenario, the inverse can be looked up in the cache, rather than be recomputed.


## The first function is called 'makeCacheMatrix'. It is a function that creates a "special matrix" that can cache it's inverse in the following steps:
## (1) It sets the value of the matrix
## (2) It gets the value of the matrix
## (3) It sets the value of the inverse
## (4) It gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {     ##Here, x is a square invertible matrix
  inv <- NULL               
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The second function is called 'cacheSolve'.This functions calculates the inverse of the "special matrix" returned by the previous function. It checks to see whether the inverse has already been calculated (and the matrix has not changed), in which case it retrieves the inverse from the cache to avoid re-computation. Otherwise, it calculates the inverse and sets the value in the cache using setinverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {                ##If inverse has already been calculated
    message("getting cached data")   ##Get it from the cache and skip computation
    return(inv)
  }
  data <- x$get()                     ##otherwise, calculate inverse using the 'solve' function
  inv <- solve (data, ...)
  x$setinverse(inv)                   ##this newly calculated value of inverse is set in the cache using 'setinverse'
  inv
}
