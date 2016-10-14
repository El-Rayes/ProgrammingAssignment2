
makeCacheMatrix <- function(x = matrix()) {
             ## x a square matrix       
       
	 i <- NULL
        
	## set the matrix
	 
	set <- function(y) {
                
                x <<- y
                i <<- NULL
        }

       ## get the matrix  
	 get <- function() x
        
	  ## set the inverse
	  setinverse <- function(inverse) i <<- inverse 
        
	  ##  get the inverse
        getinverse <- function() i
        
	  ## list used to input to cacheSolve()
	  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
        ## x output from makeCacheMatrix()
        ## the function return the inverse of the matrix in makeCacheMatrix()
        
        i <- x$getinverse()
        
        # to check if the inverse is calculated
        if (!is.null(i)){
                
                message("getting cached data")
                return(i)
        }
        
        # if not then it calculates the inverse 
        data <- x$get()
        i <- solve(data, ...)
        
        # set the value of the inverse in the cache
        x$setinverse(i)
        
        return(i)
}

