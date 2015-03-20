## Function that write and retrieve a matrix value, putting the value in cache when necessary

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                    #inicialize m with null value
  set <- function(y) {   #
    x <<- y
    m <<- NULL                                 #when setting x store m value as null in cache
  }
  get <- function() x                           #retrieve x value
  setinverse <- function(inverse) m <<- inverse # put in "m" the value of attribute inverse. Store "m" in cache
  getinverse <- function() m                    # retriev m value 
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}

## Calculate de matrix inverse for a given object

cacheSolve <- function(x, ...) {
  m <- x$getinverse()                  #get the matrix inverse value
  if(!is.null(m)) {                    #verify if the value has been assigned 
    message("getting cached data")     # if not Null then retrieve the inverse value of m in cache  
    return(m)  
  } else {
  data <- x$get()                      #assing to data de matrix value
  m <- solve(data, ...)                #calculate de matrix inverse
  x$setinverse(m)                      #call set inverse to store the value of the inverse in cache
  m                                    ## Return a matrix that is the inverse of 'x'
  }
}
