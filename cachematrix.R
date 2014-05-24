## Put comments here that give an overall description of what your
## functions do


## creates a default "matrix" object and cache both it and its inverse.
## return a list contains four functions, which can be used to set and get data
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


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
## return the inverse matrix
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
