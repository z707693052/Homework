my_f <- function(x){
  if (0 <= x && x< 0.5){
    y = 4*x/3
  }
  else if (0.5 <= x && x< 1){
    y = (4/3)*(1 - x)
  }
  else if (1 <= x && x< 1.5){
    y = (8.0/3)*(x - 1)
  }
  else if (1.5 <= x && x<= 2){
    y = (2.0/3)*(8-4*x)
  }
  else 
  {y = 0}
  y
}