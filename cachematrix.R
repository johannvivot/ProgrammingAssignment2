# The function below creates a special matrix that can cache its inverse.
# This special matrix contains a functions to:
# set the value of the matrix
# get the value of the matrix
# set the value of the matrix inverse
# get the value of the matdix inverse


makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
  
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
  
    # getter
    get <- function() x
  
    # inverse setter
    setinverse <- function(inv) invx <<- inv
  
    # inverse getter
    getinverse <- function() invx
  
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function store the inverse of the special matrix returned by 
# makeCacheMatrix. 
# If the inverse has already been calculated, then the cachesolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    invx <- x$getinverse()
    if(!is.null(invx)) {
        message("getting cached data")
        return(invx)
    }
    data <- x$get()
    invx <- solve(data, ...)
    x$setinverse(invx)
    invx
}