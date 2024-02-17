## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function 
## set the value of the vector, 
## get the value of the vector, 
## set the value of the inverse matrix and 
## get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##cache Solve gets the inverse matrix value and checks if it is null 
## if it is null, it gets the passed matrix and create inverse of it 
## assign it to the inverse variable, and
## set the value to the cache using setInverse function

cacheSolve <- function(x) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
