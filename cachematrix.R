## makeCacheMatrix creates a list which contains function for 
##setting the matrix, getting the matrix , setting the inverse of matrix and getting the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(z) i<<-z
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve first check if the inverse of matrix is available and if available gets from cache.
##If not, it calculates the inverse of matrix and sets the value of inverse in the cache

cacheSolve <- function(x, ...) {
        i<-x$getinverse()
  if(!is.null(i)){
    message("Getting catched data")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinverse(i)
  i
}
