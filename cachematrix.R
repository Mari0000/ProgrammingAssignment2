# makeCacheMatrix is function that take matrix and return list of 4 functions
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  # set the value of matrix to x
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  # get the value of matrix
  get <- function() x
  ## <<- this operator used to assign a value to an object in an environment that is different from the current environment
  ## set inverse to inverse object 
  setinverse <- function(inverse) inverse <<- inverse
  ## return the inverse
  getinverse <- function() inverse
  ## return list of 4 functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
# cacheSolve is function that take x as function 
# x return list of function so x can access any function by $
cacheSolve <- function(x, ...) {
  # access the getinverse function by x and then assign to m object
  m <- x$getinverse()
  # check if m is not null
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # get the matrix and assign it to data
  data <- x$get()
  # get matrix-inverse by using Solve function and passing data that contain that matrix and assign result to m
  m <- solve(data, ...)
  # passing m that contain matrix-inverse to function setinverse 
  x$setinverse(m)
  # return m
  m
}