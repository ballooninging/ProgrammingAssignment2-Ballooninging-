##makeCacheMatrix creates a list that allows for caching of inverse matrices to a given matrix x
##See comments within function for a more detailed explanation of the code

makeCacheMatrix <- function (x = numeric()) {
  #sets the cache for inverseMatrix to NULL to clear the cache
  inverseMatrixCache <- NULL
  #creates a nested function set, which takes the argument (y)
  set <- function (y) {
    #the argument y is set as (x), which is then used for the function in the parent environment "makeCacheMatrix"
    x <<- y
    inverseMatrixCache <- NULL
  }
  #simply returns x, as set by makeCacheMatrix
  get <- function() x
  #solves inverseMatrixCache
  setsolve <- function(solve) inverseMatrixCache <<- solve
  #gets the solved inverseMatrixCache
  getsolve <- function() inverseMatrixCache
  #creates a list that defines and stores values derived from makeCacheMatrix
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


##cacheSolve creates the inverse of a given matrix x, and stores it in a cache "inverseMatrixCache"
##See comments within function for a more detailed explanation of the code

  cacheSolve <- function (x, ...) {
    #stores the solution of x, a matrix with a valid inverse matrix, here
    inverseMatrixCache <- x$getsolve()
    #gets cached data if there is already a value in inverseMatrixCache
    if(!is.null(inverseMatrixCache)) {
      print("Getting Cached Data - Please Hold!")
      return(inverseMatrixCache)
    }
    #if there is no data in inverseMatrixCache, we have to solve x itself - thus the code below	
    data <- x$get()
    inverseMatrixCache <- solve(data, ...)
    x$setsolve(inverseMatrixCache)
    inverseMatrixCache
  }
