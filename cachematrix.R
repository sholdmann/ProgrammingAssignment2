# Put comments here that give an overall description of what your
## functions do

# The first function makeCacheMatrix() create an R object that stores a matrix and its invers. 
# The second function cacheSolve() implements the cache-property for the invers of the R object. 
# The first time you access the invers of the R object, the value is calculated. With every 
# further access, the function returns the value from the cache.


## Write a short comment describing this function

#
# makeCacheMatrix() creates an R object that stores a matrix and its invers
#

makeCacheMatrix <- function(x = matrix()) {
  
  inversmatrix <- NULL
  
  set <- function(y){
    x <<- y
    inversmatrix <<- NULL
  }
  
  get <- function() x
  
  setinvers <- function(parainvers) inversmatrix <<- parainvers
  
  getinvers <- function() inversmatrix
  
  
  list(set = set, get = get, setinvers = setinvers, getinvers = getinvers)

}


## Write a short comment describing this function

# 
# The function cacheSolve() requires an argument that is returned by makeCacheMatrix()
# in order to retrieve the invers from the cached value that is stored in 
# the makeCacheMatrix() object's environment. The first time you access the invers, the value is calculated.
#

cacheSolve <- function(x) {
  inversmatrix <- x$getinvers()
  if(!is.null(inversmatrix)) {
    message("getting cached data")
    return(inversmatrix)
  }
  data <- x$get()
  inversmatrix <- solve(data)
  x$setinvers(inversmatrix)
  inversmatrix
}


