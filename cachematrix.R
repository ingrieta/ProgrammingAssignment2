## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
## assuming that x is an invertible matrix
  
  i = NULL  # i contains the  inverse matrix
  set = function(y) {
    # `<<-`  assigns a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    i <<- NULL
  }
  get = function() x
  setinv = function(inverse) i <<- inverse 
  getinv = function() i
#this list is the input for cacheSolve function
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Returns the inverse of the original matrix entered in makeCacheMatrx
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i = x$getinv()
  
  # if the inverse was already calculated
  if (!is.null(i)){
    message("getting cache data")
    return(i) 
  }
  # else the inverse is calculated with the solve function
  matriz = x$get()
  i = solve(matriz, ...)
  # set the inverse in the cache using the setinv function.
  x$setinv(i)
  
  return(i)    
}
