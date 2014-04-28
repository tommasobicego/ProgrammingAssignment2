#create the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve #defines the method that sets the inverse matrix
        getinverse <- function() m #defines the method that retrieves the inverse matrix
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )
}

#caching the inverse matrix, if it's already in cache it retrieves it and it returns it
cacheSolve <- function(x, ...) {
        m <- x$getinverse() #tries to get the inverse matrix
        if(!is.null(m)) { #tries to see if the inverse matrix already exists
                message("getting cached data")
                return(m) #returning the cached inverse matrix
        }
        data <- x$get()
	  m <- solve(data) #returns the inverted matrix
        x$setinverse(m) #cache the inverse matrix
        m
}

