library(dplyr)
#In rugby, a penalty or drop goal is worth 3 points,  ("pd" in the code)
#an unconverted try is worth 5 points,                ("ut" in the code)
#a converted try is worth 7 points.                   ("ct" in the code)
#Any rugby score is a composition of these elements.
#
#Let's set these up as the start of our validscores data frame.
validscores <- as.data.frame(
  rbind(cbind(sc = 0,pd = 0, ut = 0,ct = 0),
        cbind(3,1,0,0),
        cbind(5,0,1,0),
        cbind(6,2,0,0),    #note that we include 6 for convenience: it's the result of 2 penalties or drop goals
        cbind(7,0,0,1)))

for(i in 8:150){          #cover plausible scores (highest score in pro rugby since 2012: NZ women 121 vs HK women 0)
  
  compositions_for_i <- NULL  #let's store the valid compositions for score i in this object
  
  #the score of interest could only be obtained from by adding 3, 5 or 7 to a lower valid score
  #if such previous scores exist, let's find them
  which_was_prev_sc_after_pd   <- which(i - validscores[,1] == 3)
  which_was_prev_sc_after_ut   <- which(i - validscores[,1] == 5)
  which_was_prev_sc_after_ct   <- which(i - validscores[,1] == 7)
  
  #where the previous score exists:
  if(length(which_was_prev_sc_after_pd)>0)    {temp1 <- validscores[which_was_prev_sc_after_pd,]
  temp1[,2] <- temp1[,2]+1 #let's copy the previous score's composition(s) and increment the appropriate score method
  temp1[,1] <- i
  compositions_for_i <- rbind(compositions_for_i,temp1)} #let's add this to the compositions df
  if(length(which_was_prev_sc_after_ut)>0)  {temp1 <- validscores[which_was_prev_sc_after_ut,]
  temp1[,3] <- temp1[,3]+1
  temp1[,1] <- i
  compositions_for_i <- rbind(compositions_for_i,temp1)}
  if(length(which_was_prev_sc_after_ct) > 0) {temp1 <- validscores[which_was_prev_sc_after_ct,]
  temp1[,4] <- temp1[,4]+1
  temp1[,1] <- i
  compositions_for_i <- rbind(compositions_for_i,temp1)}
  
  #sometimes we can end up with duplicates as order of scoring doesn't matter, so let's remove those
  if(!is.null(compositions_for_i)) compositions_for_i <- distinct(compositions_for_i)
  
  #we add these to our master validscores df and repeat for all scores, i
  validscores <- rbind(validscores,compositions_for_i)
}
