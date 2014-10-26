## makeCacheMatrix function creates a special matrix which
## calculates and caches its inverse if not already cached

## 

makeCacheMatrix <- function(x = matrix()) {
  
  inverseX <- NULL
  
  set <- function(y) {
    x <<- y
    inverseX <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(anInverse) inverseX <<- anInverse
  
  getInverse <- function() inverseX
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Calculates and caches the inverse of given matrix x if not already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseX <- x$getInverse()
  
  if(!is.null(inverseX)) {
    message("getting cached inverse...")
    return(inverseX)
  }
  aMatrix <- x$get()
  inverseX <- solve(aMatrix)
  x$setInverse(inverseX)
  inverseX
}
