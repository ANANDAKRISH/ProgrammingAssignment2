## Describing a pair of functions that cache the inverse of a matrix

## Creates special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y){
    x <<- y
    a <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) a <<- inverse
  getInverse <- function() a 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  a <- x$getInverse()
  if(!is.null(a)){
    message("getting cached data")
    return(a)
  }
  mat <- x$get()
  a <- solve(mat,...)
  x$setInverse(a)
  a
}