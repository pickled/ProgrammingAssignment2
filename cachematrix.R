## My function will not work properly. but I'll try to explain my intend
## as like given example, these functions have two parts
## first one is cache object. so if store matrix and if fetch something, then return is
## second one is calculate part. if some matrix was given, this function
## try to find that result from cached object. 
## if couldn't found target, then calculate invert matrix and save it to cache

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setivt <- function(ivt) m <<- ivt
  getivt <- function() m
  list(set = set, get = get,
       setivt = setivt,
       getivt = getivt)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getivt()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- data %*% solve(...)
  x$setivt(m)
  m
}