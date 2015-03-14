## Put comments here that give an overall description of what your
## functions do

## Usage for testing in the console:
## 1) Create a matrix, m <- matrix(c(1:4),nrow=2,ncol=2)
## 2) Cache the inverse object, x <- makeCacheMatrix(m)
## 3) Find and solve inverse, y <- cacheSolve(x)

## This function takes a matrix as input and caches it using <<-
## Thus it is a form of constructor function including sets & gets
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## If the inverse matrix has been cached, this function pulls the inverse matrix from cache and returns it.
## If cache is not present, then it executes solve and sets the matrix.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m        ## Return a matrix that is the inverse of 'x'

}

#Run functions automatically
# m <- matrix(c(1:4),nrow=2,ncol=2)
# x <- makeCacheMatrix(m)
# y <- cacheSolve(x)
# m
# y