## Put comments here that give an overall description of what your
## functions do

## Usage for testing in the console:
## 1) Create a matrix, m <- matrix(c(1:4),nrow=2,ncol=2)
## 2) Cache the inverse object, x <- makeCacheMatrix(m)
## 3) Find and solve inverse, y <- cacheSolve(x)

## This function takes a matrix as input and caches it
## Thus it is a constructor function including sets & gets
makeCacheMatrix <- function(x = matrix()) {
  # Invalidate m
  mtx <- NULL
  # Create setter function for te matrix
  set <- function(y)
  {
    x <<- y
    mtx <<- NULL
  }
  # Create getter function matrix
  get <- function() x
  # Create getter/setter functions for the inverse function
  setinverse <- function(inverse) mtx <<- inverse
  getinverse <- function() mtx
  # Return a list with setters/getters
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## If the inverse matrix has been cached, this function pulls 
## the inverse matrix from cache and returns it.
## If cache is not present, then it executes solve and sets the matrix.
cacheSolve <- function(x, ...) {
  # Pull inverse matrix from cache
  mtx <- x$getinverse()
  # If inverse matrix is in the cashe, return it
  if(!is.null(mtx)) {
    message("getting cached data")
    return(mtx)
  }
  # Else get the matrix
  data <- x$get()
  # Compute the inverse
  mtx <- solve(data, ...)
  # Put the inverse into cache
  x$setinverse(mtx)
  # Return the inverse
  mtx
}

#Run functions automatically
# m <- matrix(c(1:4),nrow=2,ncol=2)
# x <- makeCacheMatrix(m)
# y <- cacheSolve(x)
# m
# y