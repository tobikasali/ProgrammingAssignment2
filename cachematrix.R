## There are two functions, makeCacheMatrix which caches the inverse of a matrix and the cacheSolve function 
## which uses the solve function to get the inverse of a matrix.
## 

## This function takes a matrix as input and contains a list of functions that 
## set the value of the matrix (set()), get the value of matrix (get()), cache the value 
## of the inverse Matrix (setinv()) and gets the cached inverse of the matrix (getinv()).

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    
  }
  
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv= getinv)
  
}


## This function takes a matrix as input, checks to see if an inverse already exists and if not, computes
## the inverse and uses the setinv() function to cache the computed inverse.

cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  ## Return a matrix that is the inverse of 'x'
}