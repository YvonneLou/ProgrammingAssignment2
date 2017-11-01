## This function creates a special "matrix" object that can cache its inverse
## 

## makeCacheMatrix function create a list of 4 methods that you can set and get
## the matrix and also it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinver <- function(i) inver <<- i
  getinver <- function() inver
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}


## cacheSove function checks if any invertiable matrix exists, create one if not.

cacheSolve <- function(x, ...) {
        
  inver <- x$getinver()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinver(inver)
  inver
  
}
