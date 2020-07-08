## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly. 
## Here I write a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #set the value of the matrix
  set <- function(matrix) {
    inv <<- NULL 
    m <<- matrix
  }
  #get the value of the vector
  get <- function(){ 
    m
  }
  #set the inverse of the matrix
  setinverse <- function(inverse){ 
    inv <<- inverse
  }
  # get the inverse of the matrix
  getinverse <- function(){
    inv
  }
  #Construct the special object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  ## return the inverse if its already set
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  ## Get the matrix from our object
  data <- x$get()
  ## Calculate the inverse using matrix multiplication
  inv <- solve(data)
  x$setmean(inv)
  inv
}

