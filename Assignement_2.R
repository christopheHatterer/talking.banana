makeMatrix <- function(x = numeric()) {
      cached.inv <- NULL
      set <- function(y) {
            x <<- y
            cached.inv <<- NULL
      }
      get <- function() x
      setinv <- function(matinv) cached.inv <<- matinv
      getinv <- function() cached.inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

cacheinv <- function(x, ...) {
      cached.inv <- x$getinv()
      if(!is.null(cached.inv)) {
            message("getting cached data")
            return(cached.inv)
      }
      data <- x$get()
      cached.inv <- solve(data, ...)
      x$setinv(cached.inv)
      cached.inv
}



mm <- makeMatrix()
mm$set(matrix(c(6, 2, -2, 1, 3, 1, 5, 2, 1), 3, 3))
mm$get()
cacheinv(mm)
mm$get() %*% inv
class(inv)
class(mm)
mm
