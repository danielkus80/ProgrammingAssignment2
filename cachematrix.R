## R functions to create special matrix and cache matrix inversion computation. 


## makeCacheMatrix() function
## To create a special matrix, which is really a list containing functions to
## set/get the value of the matrix and set/get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  
  get <- function() x
  
  setsolve <- function(solve) im <<- solve
  
  getsolve <- function() im
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve() function 
## To return a matrix (either by computation or from cache) 
## that is the inverse of matrix 'x'
cacheSolve <- function(x, ...) {
  im <- x$getsolve()
  
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  
  data <- x$get()
  im <- solve(data, ...)
  
  x$setsolve(im)
  im
}