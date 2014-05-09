## This function creates a special "matrix" object that can cache its inverse
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function below computes the inverse of the function returned by the makeCacheMatrix above
## If the inverse has been calculated then it should retrieve inverse from cache
## It will returns a matrix that is the inverse of 'x'
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
  }
