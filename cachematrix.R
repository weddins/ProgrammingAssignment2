## Put comments here that give an overall description of what your
## functions do

## Usage for testing in the console:
## 1) Create a matrix, m <- matrix(c(1:4),nrow=2,ncol=2)
## 2) Cache the inverse object, x <- makeCacheMatrix(m)
## 3) Find and solve inverse, y <- cacheSolve(x)

## This function takes a matrix as input and caches it using <<-
## Thus it is a form of constructor function including sets & gets
makeCacheMatrix <- function(x = matrix()) {
  # Invalidate m
  m <- NULL
  # Create setter function for te matrix
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  # Create getter function matrix
  get <- function() x
  # Create getter/setter functions for the inverse function
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  # Return a list with setters/getters
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## If the inverse matrix has been cached, this function pulls the inverse matrix from cache and returns it.
## If cache is not present, then it executes solve and sets the matrix.
cacheSolve <- function(x, ...) {
  # Pull inverse matrix from cache
  m <- x$getinverse()
  # If inverse matrix is in the cashe, return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Else get the matrix
  data <- x$get()
  # Compute the inverse
  m <- solve(data, ...)
  # Put the inverse into cache
  x$setinverse(m)
  # Return the inverse
  m
}

#Run functions automatically
# m <- matrix(c(1:4),nrow=2,ncol=2)
# x <- makeCacheMatrix(m)
# y <- cacheSolve(x)
# m
# y