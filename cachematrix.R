## Francisco Souza

## makeCacheMatrix: Function to create an special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverseMatrix <- function(inverse) inv <<- inverse
  getInverseMatrix <- function() inv
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  
}


#cacheSolve: Function to compute the inverse of the special "matrix" returned by makeCacheMatrix funcion
#If the inverse has already been calculated (matrix has not changed) then the cachesolve should 
#retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv<- x$getinverse()
  if(!is.null(inv)) {
    message("Getting Cached Data!")
    return(inv)
  }
  mat<-x$get()
  inv<- solve(mat, ...)
  x$setinverse(inv)
  inv
}
