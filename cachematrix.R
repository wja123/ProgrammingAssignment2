## makeCacheMatrix creates a special Matrix object and caches its inverse
## 

## makeCacheMatrix creates a special Matrix object and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<- function(y){
    x <<-y
    m<-NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}


## checks if the objects inverse has already been cached, and if so returns the inverse matrix, or else calculates its inverse and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

