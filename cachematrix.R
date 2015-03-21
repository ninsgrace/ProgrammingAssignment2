##The functions below creates a special matrix object through makeCacheMatrix
##And return its inverse thru cacheSolve function which uses the solve function

##This function creates a special matrix object that contains a list of function to 
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of the matrix
##get the value ot the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL ##holds the inverse of the matrix
    
    ##sets the value of the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    ##gets the value of the matrix
    get <- function() x
    
    ##setting the inverse of the matrix 
    setinverse <- function(inv) inverse <<- inv
    
    ##getting the inverse of the matrix
    getinverse <- function() inverse
    
    ##list containg above functions so we can use makeCacheMatrix object as
    ##x <- makeCacheMatrix(sampleMatrix) 
    ##x$set(newMatrix) --setting new matrix
    ##x$get() --to get the matrix
    ##x$setinverse() -- to set the inverse 
    ##x$getinvers() -- to get the inverse
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}

##This function computes the inverse of the special
##"matrix" returned by `makeCacheMatrix` above
cacheSolve <- function(x, ...) {## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse() ##getting the inverse of x using x$getinverse if already cached
  
  if(!is.null(inverse)) { ##if there is already an inverse data, get the cached inverse
    message("getting cached data")
    return(inverse)
  }
  
  ##if no inverse value yet, get the matrix using the get function "x$get()"
  data <- x$get() 
  
  ##get the inverse of the matrix stored in data using solve function
  inverse <- solve(data, ...)
  
  ##cache the inverse value derived from above
  x$setinverse(inverse)
  
  ##return the inverse of the matrix
  inverse
}
