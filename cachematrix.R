## Put comments here that give an overall description of what your
## functions do

## Function to cache the inverse of a matrix. Takes an invertable matrix as an argument.
## Contains set, get, setInv, and getInv functions.

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(z) inv <<- z
  
  getInv <- function() inv
  list(set=set,get=get,setInv = setInv, getInv = getInv)
}


## Takes an argument x which should be an invertable matrix and first checks
## if its inverse has been cached already, otherwise calculates the inverse
## and caches it. 

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
