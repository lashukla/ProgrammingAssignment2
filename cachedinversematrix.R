makeCacheMatrix <- function( x = matrix())
{
	inverseOfMatrix <- NULL
	set <- function(y)
	{
		x <<- y
		inverseOfMatrix <<- NULL
	}
	get <- function()
	{
		x
	}
	setInverse <- function(inverseVal) 
	{
		inverseOfMatrix <<- inverseVal
	}
	getInverse <- function()
	{
		inverseOfMatrix 
	}
	list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}

cacheSolve <- function(x,...)
{
	inverseOfMatrix <- x$getInverse()
	if(!is.null(inverseOfMatrix))
	{
		print("getting inverse from cache")
		return(inverseOfMatrix)
	}
	data <- x$get()
	inverseOfMatrix <- solve(data)
	x$setInverse(inverseOfMatrix)
	inverseOfMatrix
}
