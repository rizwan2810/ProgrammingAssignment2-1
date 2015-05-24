## Create matrix that takes input values and create an Inverse values in the matrix
## Function creates the a function that caches the inverse values. 

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) 
{
 inv <- NULL ## Initialize the variable inverse
    set <- function(y) {
        x <<- y ## Setting the values in the function
        inv <<- NULL
    }
    get <- function() x ## gets the value from the function
    setinverse <- function(inverse) inv <<- inverse ##Creates an Inverse function 
    getinverse <- function() inv # Get the inverse value
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ##List the values
}

## This Function returns the inverse of the matrix. It checks for the presence of the inverse. 
##If present take the values and gets the result else, computes the inverse, sets the value in the cache using setfunction.

##This function has an assumption that matrix is always invertible.
cacheSolve <- function(x, ...) 
{
 inv <- x$getinverse() #Get Inverse Value
    if(!is.null(inv)) #Check for NULL
	
	{
        message("Retrieving Cached Data")
        return(inv) #Return inverse values
    }
    data <- x$get() #Get Cached Data
    inv <- solve(data) #Solve the Cached values in the matrix
    x$setinverse(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
