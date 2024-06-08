plot_rep<- function(cycle){ 

      for (i in 1:dim(cycle)[1]) {
    lines(cycle[i,,])
  }

}

plotlines <- function(data,cyclerep,one_line_at_a_time=FALSE,same_col=TRUE,ptitle=NULL){
  #plot the lines of a cycle rep all at once or one at a time 
  #par(bg = 'black', fg = 'white')
  plot(data,type = 'p')#,col='white')
  title(main = list(ptitle, cex = 1.5,font=3))#, col = "white", font = 3))
  points(cyclerep,pch=16)
  
  cols<-colorRampPalette(c('red','blue'))(nrow(cyclerep)/2)
  
  if( same_col == TRUE ){cols<- rep('black',nrow(cyclerep)/2)}#rep('red',nrow(cyclerep)/2)}
  
  for (point in seq(from=2, to=nrow(cyclerep),by=2)){
    lines(cyclerep[(point-1):point,],col=cols[point/2])
    if (one_line_at_a_time==TRUE){readline(prompt="Press enter to continue")}
  }

}

plot_reps<-function(data,cyclereps,start=1,end=NULL){
  #plot cycle reps one at a time
  if(is.null(end)==TRUE){end<- length(cyclereps)}
  
  cyclerep<-start
  press<-''
  
  while (press!='e' & cyclerep != (end + 1) & cyclerep != 0) {
    
    plotlines(data,cyclereps[[cyclerep]],ptitle=cyclerep)
    
    press<- readline('Press <enter> to continue or <b + enter> to go back or <e + enter> to exit.')
    
    if( press == 'b'){
      cyclerep<-cyclerep - 1
      } else if ( press == 'e'){
      break
      } else {
      cyclerep<- cyclerep + 1
    }
  
  }
  
}



