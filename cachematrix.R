## The main idea of both functions is to be able to create a special matrix object
## and assign it into the cache. The function cacheSolve takes the previously created
## matrix and calculates its inverse

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
    }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
 inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setinverse(inver)
  inver
}

# Peng (2014) offered an example from a vector of how the code may be written,
# the student took this example and aided from R help sites such as stackoverflow
# was able to wrote the needed code to acomplish the assignment.
