#makeCacheMatrix function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { #function takes argument x that is invertible matrix
  m <- NULL 
  set <- function(y) { #set value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() {x} #get value of the matrix
  setInverseM <- function(inverse) {m <<- inverse} #set value of the inverse matrix
  getInverseM <- function() {m} #get value of the inverse matrix
  list(set = set, get = get, setInverseM = setInverseM,getInverseM = getInverseM) #create list containing function
}

#cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getInverseM() #assign to m returned inverted matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #compute, set and return inverse matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setInverseM(m)
  m
}