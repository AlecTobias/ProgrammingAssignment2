makeCacheMatrix <- function(x = matrix()) { 
## ^ A function tasked to create a matrix to cache the inverse

    zz <- NULL ##sets up the inverse as a NULL


    set <- function(y) {

      x <<- y

      zz <<- NULL

    }

  get <- function() x ##needed to make matrix of x

  setInverse <- function() zz <<- solve(x) ##This part calculates the inverse
  getInverse <- function() zz

  list(set = set, get = get,

       setInverse = setInverse,

       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    ##We inverse the matrix of x

    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("obtaining the cached matrix")
        return(inv)
    }

    mat <- x$get()

    inv <- solve(mat, ...)

    x$setInverse(zz)

    zz
}

 newMatrix <- makeCacheMatrix()  ##newMatrix is simply the name of the matrix

 newMatrix$ set(matrix(9:16,3))   ##Creates new matrix
 newMatrix$ get()                 ##Here we show the created matrix

 newMatrix$ setInverse()         ##Calculates inverse of created matrix
 newMatrix$ getInverse()         ##Here we show the inversed matrix



