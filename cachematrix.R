## Date:		14th October 2014
## Description	Initialises a matrix 
## Version:		0.1
## Change History
## Date			Change				Version
## 14/10/2014	Initial function	0.1

makeCacheMatrix <- function(x = matrix()) 
{
	## Initialise the matrix
	m<-NULL
	set<-function(y)
	{
		s<<-y
		i<<-NULL
	}
	
	## Get value of the matrix
	get<-function() x
	
	## Initialise the inverse of the matrix
	setinverse<-function(solve) i<<-solve
	getinverse<-function() i
	
	## Get the inverse of the matrix
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Date:		14th October 2014
## Description	Returns a matrix that is the inverse of x 
## Version:		0.1
## Change History
## Date			Change				Version
## 14/10/2014	Initial function	0.1

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse
		
		## If exists
		if(!is.null(i))
		{
			message("Retrieving cached data...")
			return(i)
		}
		## else: 
		data <- x$get()
		i <- solve(data, ...)
		
		## set the inverse of the matrix
		x$setinverse(i)
		i
}
