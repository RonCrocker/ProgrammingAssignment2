## A collection of functions that create and operate on a special
## kind of matrix that enables caching the results of "expensive"
## operations - those operations that take a lot of time or memory
## (or both).
##
## Currently there are two functions:
## - makeCacheMatrix(x) - Creates a matrix-like object with caching abilities.
##
## - cacheSolve(x, ...) - Returns the result of running solve() on the
##                        matrix-like object returned from makeCacheMatrix()
##                        above. If the result has already been calculated,
##                        the function returns the previous result; if it
##                        has not yet been caclulated, the function calculates
##                        it and saves the result to be returned in subsequent
##                        calls.

## makeCacheMatrix()
##
## Create the matrix-like object with cacheing abilities.
##
## Parameters:
##  - x: The matrix to wrap with caching abilities. If not provided, an empty
##       matrix will be used.
## Result:
##  A special kind of matrix object with caching abilities (represented by a list in R)

makeCacheMatrix <- function(x = matrix()) {
  setMatrix <- function(y) {
    x <<- y
    clearCache()
  }

  getMatrix <- function() x

  ## ---
  ## Cache-related operations
  ## ---

  solve.cache <- NULL # This is the cached inverse via solve()

  clearCache <- function() {
    solve.cache <<- NULL
  }

  setCache <- function(cache) {
    solve.cache <<- cache
  }

  getCache <- function() {
    solve.cache
  }

  # Test if the cache is null
  cacheIsNull <- function() is.null(solve.cache)

  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       clearCache = clearCache,
       setCache = setCache,
       getCache = getCache,
       cacheIsNull = cacheIsNull)
}

## cacheSolve(x, ...)
##
## Returns the result of running solve() on the matrix-like object
## returned from makeCacheMatrix() above. If the result has already
## been calculated, the function returns the previous result; if it
## has not yet been caclulated, the function calculates it and saves
## the result to be returned in subsequent calls.
##
## Parameters:
##  - x: A special matrix, as created by makeCacheMatrix().
##  - ... any other parameters; turns out they're not really used, but that's ok.
##
## Result:
##  the result of running solve() on the matrix provided when the x object was created.

cacheSolve <- function(x, ...) {
  # If the cache isn't populated, populate it now
  if (x$cacheIsNull()) {
    x$setCache(solve(x$getMatrix()))
  }
  # return the value from the cache
  x$getCache()
}
