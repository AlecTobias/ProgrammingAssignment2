## Put comments here that give an overall description of what your
## functions do

## We have the functions of makeCacheMatrix and library(MASS)
## makeCacheMatrix consists of set,get,setinv, and getinv
##library(MASS) is used to calculate the inverse for non squared and square matrices


library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL            #initializing inverse as NULL
  set<-function(y){
                  x<<-y
                  inv<<-NULL
                   }
  get<-function()x             #function to get matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){ 
                    inver<-ginv(x)
                    inver%*%x           #function to obtain inverse of the matrix
                    }
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}

## Write a short comment describing this function
##This is used to get the cache data

cacheSolve <- function(x, ...) ##gets cache data      
  {
  inv<-x$getinv()                  
  if(!is.null(inv)){                 #checking whether inverse is NUll 
                     message("getting cached data!")
                     return(inv)                       #returns inverse value
  }
  data<-x$get()
  inv<-solve(data,...)              #calculates inverse value
  x$setinv(inv)
  inv   ## Return a matrix that is the inverse of 'x'
}
f<-makeCacheMatrix(matrix(2:9,4,6))
f$get()
f$getinv()
cacheSolve(f)

##Peer-Graded Assessment
##Sample Usage
> f<-makeCacheMatrix(matrix(2:9,4,6))
> f$get()
     [,1] [,2] [,3] [,4] [,5] [,6]
[1,]    2    6    2    6    2    6
[2,]    3    7    3    7    3    7
[3,]    4    8    4    8    4    8
[4,]    5    9    5    9    5    9
> f$getinv()
             [,1]         [,2]         [,3]         [,4]         [,5]
[1,] 3.333333e-01 2.220446e-16 3.333333e-01 2.220446e-16 3.333333e-01
[2,] 5.551115e-17 3.333333e-01 5.551115e-17 3.333333e-01 5.551115e-17
[3,] 3.333333e-01 2.220446e-16 3.333333e-01 2.220446e-16 3.333333e-01
[4,] 5.551115e-17 3.333333e-01 5.551115e-17 3.333333e-01 5.551115e-17
[5,] 3.333333e-01 2.220446e-16 3.333333e-01 2.220446e-16 3.333333e-01
[6,] 5.551115e-17 3.333333e-01 5.551115e-17 3.333333e-01 5.551115e-17
             [,6]
[1,] 2.220446e-16
[2,] 3.333333e-01
[3,] 2.220446e-16
[4,] 3.333333e-01
[5,] 2.220446e-16
[6,] 3.333333e-01
> cacheSolve(f)
getting cached data!
             [,1]         [,2]         [,3]         [,4]         [,5]
[1,] 3.333333e-01 2.220446e-16 3.333333e-01 2.220446e-16 3.333333e-01
[2,] 5.551115e-17 3.333333e-01 5.551115e-17 3.333333e-01 5.551115e-17
[3,] 3.333333e-01 2.220446e-16 3.333333e-01 2.220446e-16 3.333333e-01
[4,] 5.551115e-17 3.333333e-01 5.551115e-17 3.333333e-01 5.551115e-17
[5,] 3.333333e-01 2.220446e-16 3.333333e-01 2.220446e-16 3.333333e-01
[6,] 5.551115e-17 3.333333e-01 5.551115e-17 3.333333e-01 5.551115e-17
             [,6]
[1,] 2.220446e-16
[2,] 3.333333e-01
[3,] 2.220446e-16
[4,] 3.333333e-01
[5,] 2.220446e-16
[6,] 3.333333e-01
