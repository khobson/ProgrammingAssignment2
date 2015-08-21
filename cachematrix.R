## Kristian Hobson. r Programming Assignment 2, 21/08/2015
##
## Included are 2 functions for the purposes of this assignment. 
##			makeCacheMatrix
##			cacheSolve
##
##makeCacheMatrix - 	This function commits a value to an object in an environment that is different to the environment we are 
##			currently in. In this case a matrix.
##
##cacheSolve -		This function inverts a matrix and returns the inverted from that submitted to the function
##
##
##

makeCacheMatrix <- function(x = matrix()) {
	## Set the variable to null.
        invert = NULL
        set = function(y) {
                ## use `<<-` to assign a value  
                x <<- y
		## reset the invert variable to null.
                invert <<- NULL
        }
        get = function() x
        setinv = function(inverse) invert <<- inverse 
        getinv = function() invert
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## This function inverts the input variable, as a matrix
        
	## Establish the variable with the data passed in
        invert = x$getinv()
        
        ## if we have already done it, do not do it again
        if (!is.null(invert)){
                ## Send a message back to the console. 
                message("getting cached data")

		## Return out of the function without doing it again
                return(invert)
        }
        
        ## If we have not done it, as above, prepare using the solve function as advised
        mat.data = x$get()
        invert = solve(mat.data, ...)
        
        ## Invert the data using the setinv function.
        x$setinv(invert)
        
	## Finally return the data.
        return(invert)
}
