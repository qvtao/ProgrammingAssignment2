## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inversemx <- NULL
  set <- function(y) {
    x <<- y
    inversemx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inversemx <<- inverse
  getinverse <- function() inversemx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x,...) {
  inversemx <- x$getinverse()
  
  if(!is.null(inversemx)) {
    message("getting cached inverse matrix")
    return(inversemx)
  }
  mx <- x$get()
  inversemx <- solve(mx,...)
  x$setinverse(inversemx)
  inversemx  
}
