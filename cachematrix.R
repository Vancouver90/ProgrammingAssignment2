
#    The first function, makeCacheMatrix, creates a special "matrix":
#    sets and gets the value of the matrix
#    sets and gets the value of the inverse of the matrix
#
#	 The second function, CacheSolve, provides the inverse of the "matrix",
#	 either by retrieving it from cache or computing it otherwise.

#This function creates a special "matrix" object which can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

	  m <- NULL
		#Next line sets the matrix and initializes m to null
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
		#Next line names the 4 set and get functions so that they can be called by them
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)	
	  
}


## CacheSolve Function:
## If the inverse has already been calculated then cache.solve retrieves the inverse that has been cached
## If the inverse has not already been calculated then it calculates the inverse using solve function and
## stores the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {

		m <-x$getinverse() #Looks for stored inverted matrix in cache and sets m to it
		#If m is NOT null then inverted matrix is in cache and it returns message that it is getting the cached data
       if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()			#gets the matrix x
        m <- solve(data, ...) 	#solve function computes the inverse of the matrix 
        x$setinverse(m)			#stores the value of the inverse in the cache
        m
	}
	
