## A pair of functions that cache the inverse of a matrix
## makeCacheMatrix:creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL                                         ## Initialize the inverse property m<-NULL
  set<-function(y){                               ## Method to set the matrix 
    x<<-y
    m<<-NULL
  }
  get <- function() {                             ## Method the get the matrix
    x                                             ## Return the matrix
  }
  setInverse <- function(inverse) {               ## Method to set the inverse of the matrix
    i <<- inverse
  }
  getInverse <- function() {                      ## Method to get the inverse of the matrix
    m                                             ## Return the inverse property
  }
  list(set = set, get = get,                      ## Return a list of the methods
       setInverse = setInverse,
       getInverse = getInverse)

}

## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
   m <- x$getInverse()                            ## Return a matrix that is the inverse of 'x'        
          if(!is.null(m)) {                       ## Return the inverse if its already set        
          message("fetching the cached data")
          return(m)
        }
   data <- x$get()                                ## Get the matrix from our object
   m <- solve(data) %*% data                      ## Calculate the inverse using matrix multiplication
   x$setInverse(m)                                ## Set the inverse to the object
   m                                              ## Return the matrix
}
