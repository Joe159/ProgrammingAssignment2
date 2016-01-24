## The functions below are used to create a special list which acts as a matrix
## The list is able to store the inverse of the matrix so that it does not need 
## to be re-calculated after it has been called 

## The makeCacheMatrix function creates a list which contains the functions that
## create and retrive the values of the  matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
  INV<<-NULL
  SetMatrix<-function(y){
    x<<-y
    INV<<- NULL
  } 
  GetMatrix<-function() x
  SetInv<-function(solve) INV<<-solve
  GetInv<-function() INV
  list(SetMatrix=SetMatrix, GetMatrix=GetMatrix,SetInv=SetInv,GetInv=GetInv) 

}


## cacheSolve checks if the inverse has previously been calculated. If it has 
## it returns the previous value, otherwise it will calculate the inverse and
## store it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  INV<<-x$GetInv()
  if(!is.null(INV)) {
    message('getting cached Inverse')
    return(INV)
  }
  data<-x$GetMatrix()
  INV<-solve(data,...)
  x$SetInv(INV)
  INV
}
