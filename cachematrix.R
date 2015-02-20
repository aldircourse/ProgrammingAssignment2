## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inverseMatrix<-NULL
      set <- function(y) {
            ## assign external matrix to inner object
            x <<- y
            ## as we have a new matrix, old inverse matrix is not valid
            inverseMatrix <<- NULL
      }
      
      get <- function(){
            ##return value of x
            x
      }
      
      setInverse <- function(invMatrix){ 
            inverseMatrix<<- invMatrix
      }
      
      getInverse <- function(){
            inverseMatrix
      } 
      
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      iMatrix <- x$getInverse()
      if(!is.null(iMatrix)) {
            message("getting cached data")
            return(iMatrix)
      }
      data <- x$get()
      iMatrix <- solve(data, ...)
      x$setInverse(iMatrix)
      iMatrix
}

useInverse<-function(matrix, ...){
      initTime<-Sys.time()
      v<-makeCacheMatrix(matrix)
      m1<-cacheSolve(v)
      midTime<-Sys.time()
      m2<-cacheSolve(v)
      endTime<-Sys.time()
      
      #perfomance statistics
      noCacheTime<-diff.POSIXt(c(initTime,midTime))
      cacheTime<-diff.POSIXt(c(midTime,endTime))
      
      print(paste("noCache: ",noCacheTime))
      print(paste("cacheTime: ",cacheTime))
}

