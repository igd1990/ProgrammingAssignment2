## ##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { ##defines a function to set the matrix, x, to a new matrix, y, 
    ##and resets the inverse, inv, to NULL
    x <<- y
    inv <<- NULL
  }
  get <- function() x  ##returns the matrix, x
  setinverse <- function(solve) inv <<- solve
  ## solve function used to find the inverse of a matrix
  getinverse <- function() inv      ##m <<- sets the inverse, inv, to inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  ## returns the 'special vector' containing all of the functions just defined
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) { 
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

## Test Matrix :A
A = matrix(c(2, 4, 3, 1),nrow=2,ncol=2,byrow = TRUE)        # fill matrix by rows
m1 <- makeCacheMatrix(A)
cacheSolve(m1)


## References 
## http://stackoverflow.com/questions/24904683/caching-the-mean-of-a-vector-in-r
## http://www.statmethods.net/advstats/matrix.html