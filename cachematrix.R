## Functions returning objects to allow memoization of a matrix inverse  

## Returns object storing an invertible matrix and its inverse.

makeCacheMatrix <- function(myMatrix = matrix()) {
	    myMatrixInverse <- NULL
            set <- function(mat) {
	    	myMatrix <<- mat
	    	myMatrixInverse <<- NULL
	            }
	    get <- function() myMatrix
	    setInverse <- function(inverse) myMatrixInverse <<- inverse
	    getInverse <- function() myMatrixInverse
	    
	    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Takes a matrix object X and computes inverse storing it in X's inverse cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	    inverse <- x$getInverse()
            if(!is.null(inverse)) {
            	message("getting cached data")
	        return(inverse)  
	    }
	    mat <- x$get()
	    inverse <- solve(mat)
	    x$setInverse(inverse)
            inverse
}

makeCacheMatrixImproved <- function(myMatrix = matrix()) {
	    myMatrixInverse <- NULL
            set <- function(mat) {
		                    myMatrix <<- mat
	                    myMatrixInverse <<- NULL
			                        }
	                get <- function() myMatrix
	                getInverse <- function() {
				                if (is.null(myMatrixInverse)) {
							                    myMatrixInverse <<- solve(myMatrix)
			                }
			                myMatrixInverse
					            }
			            list(set = set, get = get, getInverse = getInverse)


}

