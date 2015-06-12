## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        t <- NULL
        #this function is to reset the value of matrix directly
        set <- function(y){
                x <<- y
                t <<- NULL
        }
        
        #this function is to get the matrix values
        
        get <- function() x
        
#setmatrix and getmatrix are functions very similar to set and get. 
#They don't inverse the matrix, they simply store the value of the input 
#in a variable t into the main function makeCacheMatrix (setmatrix) and return it (getmatrix).
        setmatrix <- function(matrix) t <<- matrix
        getmatrix <- function() t
        
        list(set=set,get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
##The first thing cacheSolve does is to verify the value t, stored previously with getmatirx, 
##exists and is not NULL. If it exists in memory, it simply returns a message and the value t 
#that is supposed to be the matrix, but not necessarily.        
        
        t<-x$getmatrix()
        if(!is.null(t)) {
                message("getting cached matrix")
                return(t)
        }
        
#If it was the case, "return(t)" would have ended the function. So everything 
#that follows this if() is a sort of else {}. data gets the vector stored 
#with makeCacheMatrix, t inverses the matrix and x$setmatrix(t) stores it in the 
#object generated assigned with makeCacheMatrix.
        data <- x$get()
        t <- solve(data)
        x$setmatrix(t)
        t
        
}

