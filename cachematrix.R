## Calculates the Inverse of a Matrix and caches it

## Handles the cache of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()
    x
  setmtrx <- function(mtrx)
    m <<- mtrx
  getmtrx <- function()
    m
  list(
    set = set,
    get = get,
    setmtrx = setmtrx,
    getmtrx = getmtrx
  )
}


## Returns the inverted matrix. Uses a cache as needed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmtrx()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmtrx(m)
  m
}
