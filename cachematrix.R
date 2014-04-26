## Below makeCacheMatrix function takes a matrix and caches its inverse.
## Last command in the function returns a list containing the functions

makeCacheMatrix <- function(x = matrix()) {
  # initialize inverse value to NULL
  inv <- NULL
  # set matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  get <- function() x
  # set and get inverse 
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  # return list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Below function calculates the inverse fo matrix 
## which is created ib makeCacheMatrix function. 

cacheSolve <- function(x, ...) {
  # get from cache if its already cached
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  # calculate inverse
  inverse <- solve(data, ...)
  # cache inverse of matrix
  x$setinv(inverse)
  # returning inverse
  inverse
}
