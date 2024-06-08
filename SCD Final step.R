#Compute the SCD between two complexes X and Y
#last step to find the max after finding directed distances

scd(X,Y)<- function(X,Y){
  max(sqd_complex_complex(X,Y), sqd_complex_complex(Y,X)
    }