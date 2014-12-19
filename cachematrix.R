makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(matrix){
    x <<- matrix
    i <<- NULL
  }
  
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
} 


cacheSolve <- function(x, ...) {
  
  i <- x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  message("calculating inverse matrix")
  data <- x$get()
  i <- solve(data,...)
  x$setinv(i)
  return(i)
}