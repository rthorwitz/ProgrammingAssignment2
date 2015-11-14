##Defines the function as a matrix and sets up the inverse dataframe
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  
  ##Defines a function to assign a value to an object in an environment different from the current environment.
  set = function(y) {  
    x <<- y
    inv <<- NULL
  }
  
  ##Sets and gets the value of the inverse of the original matrix
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set,get=get,
       setinverse = setinverse,
       getinverse=getinverse)
}

##Function to calculate the inverse of the original matrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  ##Checks to see if value is already stored  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ##Calculates value
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  x$setinverse(inv)
  return(inv)
}