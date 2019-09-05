####################################################################################
### Monte Carlo simulation of Rugby World Cup 2019
####################################################################################
### Author: statsonthe.cloud
####################################################################################
### Purpose: to provide simulation output as a df for further analysis
####################################################################################
### Revision A
####################################################################################
# development list:
# need to add tie-breaker logic
# could re-compute fluid rating as tournament progresses
# recompute try distn
# done:
# random additive factor can be added to team rating before tournament starts

library(dplyr)

####################################################################################
### 1) Team Ratings Baseline
####################################################################################

### Rugbypass index baseline
#source: index.rugbypass.com
#date: 2019-08-11, following the completion of the Rugby Championship

#2019-09-04
ratings <- data.frame( 
           team         = c("NZL","WAL","ENG","RSA","IRE","AUS","FRA","SCO","ARG","FIJ","JPN","USA","SAM","GEO","RUS","ITA","TON","CAN","NAM","URU"),
           rpi_20190904 = c(89,79,85,83,85,73,66,69,66,61,60,56,51,49,46,46,27,13,13,13),
           wr_20190904  = c(89.40,87.92,88.13,86.83,88.86,84.05,79.72,81.00,76.29,77.43,77.21,71.93,69.08,73.29,64.81,72.04,71.04,61.36,61.01,65.18),
           rpi_20190811 = c(88,85,83,82,81,75,70,70,67,61,60,56,51,48,45,43,27,13,13,13),
           wr_20190805  = c(91.54,89.96,86.27,86.30,88.69,81.91,79.42,80.17,76.81,76.98,76.62,72.53,69.08,74.42,64.81,72.04,71.49,61.36,61.01,65.18),
           stringsAsFactors = F
)
ratings$consensus <- (ratings$rpi_20190904+ratings$wr_20190904+
                      ratings$rpi_20190811+ratings$wr_20190805)/4
ratings <- transform(ratings, SD=apply(ratings[,2:5],1, sd, na.rm = TRUE))
ratings <- ratings[order(ratings$consensus),]

plot(ratings$rpi_20190904,ratings$wr_20190904,xlim=c(0,100),ylim=c(0,100),
     xlab="RPI 2019-09-04",ylab="WR 2019-09-04",
     main="World Rugby vs RugbyPass ratings")
abline(0,1)

barplot(ratings$consensus,names=ratings$team,ylim=c(0,100),col="darkgrey",
        main="RugbyPass/World Rugby August/September 2019 mean consensus ratings",
        ylab="mean consensus rating")

boxplot(t(ratings[,2:5]),names=ratings$team,
        main="Distribution of Ratings by Team",
        sub="RugbyPass/World Rugby August/September 2019 ratings")

mean(ratings$SD)

ratings[ratings$team=="JPN","consensus"] <- ratings[ratings$team=="JPN","consensus"]+1.5 #give JPN some home advantage


####################################################################################
### 2) Scoring Distributions
####################################################################################

#distributions below taken from Pro 14 2017/18 season analysis
#refer to github repo for further documentation
#tries distribution
plot(seq(0,10,1),evd::dgumbel(seq(0,10,1),loc = 1.99, scale = 1.49),ylab="probability of occurrence",xlab="tries scored",main="A distribution for number of tries scored",sub="Gumbel distribution fitted to 2017/18 season Pro 14 data")
lines(seq(0,10,0.01),evd::dgumbel(seq(0,10,0.01),loc = 1.99, scale = 1.49),col="grey")

#conversion distribution
plot(c(0,1),c(1/3,2/3),ylim=c(0,1),
     ylab="probability of success, given a try has been scored",
     xlab="conversion outcome",
     main="A distribution for conversion success",
     sub="Distribution fitted to 2017/18 season Pro 14 data")

#penalties distribution
plot(seq(0,10,1),evd::dgumbel(seq(0,10,1),loc = 0.89, scale = 0.98),ylab="probability of occurrence",xlab="penalties scored",main="A distribution for number of penalties scored",sub="Gumbel distribution fitted to 2017/18 season Pro 14 data")
lines(seq(0,10,0.01),evd::dgumbel(seq(0,10,0.01),loc = 0.89, scale = 0.98),col="grey") 

####################################################################################
### 3) Define Match Simulation Functions
####################################################################################

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#calculate the probability of winning for two teams
pwin <- function(rating1,rating2,homeadv=5){
                  rpass_diff <- rating1-rating2
                  home <- 50 + rpass_diff + homeadv
                  home <- min(home,98)
                  home <- max(home,2)
                  away <- 100 - home
                  c(home,away)*0.01
                  }
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#simulate match result
rwc_match_sim <- function(teamA,teamB,rtgs=ratings){
        teamA <- as.character(teamA)
        teamB <- as.character(teamB)
        teamA_rating <- rtgs[rtgs$team==teamA,'consensus']
        teamB_rating <- rtgs[rtgs$team==teamB,'consensus']
        teamA_pwin   <- pwin(teamA_rating,teamB_rating,0)[1]  #the rpi pwin, with 0 home advantage
        teamB_pwin   <- pwin(teamA_rating,teamB_rating,0)[2]
        teamA_relpwin <- teamA_pwin * 2   #scale pwin so 50% chance of winning gets you a 1* score multiplier in the simulation, etc.
        teamB_relpwin <- teamB_pwin * 2
    #take a random sample from the distributions and scale it by the pwin
        teamA_tries <- pmax(round(evd::rgumbel(1,loc = 1.99 * teamA_relpwin,scale = 1.49* teamA_relpwin),0),0) #number of tries scored
        teamA_convs <- sum(round(runif(teamA_tries,0,1)*3/2,0)) #assumed to the dependent on num_tries but not on overall odds
        teamA_pens  <- pmax(round(evd::rgumbel(1,loc = 0.89 * teamA_relpwin,scale = 0.98 * teamA_relpwin),0),0) #number of penalties scored
        teamA_score <- teamA_tries * 5 + teamA_convs * 2 + teamA_pens * 3
        teamB_tries <- pmax(round(evd::rgumbel(1,loc = 1.99 * teamB_relpwin,scale = 1.49* teamB_relpwin),0),0) #number of tries scored
        teamB_convs <- sum(round(runif(teamB_tries,0,1)*3/2,0)) #assumed to the dependent on num_tries but not on overall odds
        teamB_pens  <- pmax(round(evd::rgumbel(1,loc = 0.89 * teamB_relpwin,scale = 0.98 * teamB_relpwin),0),0) #number of penalties scored
        teamB_score <- teamB_tries * 5 + teamB_convs * 2 + teamB_pens * 3
    #work out who won, lost or drew
        if(teamA_score > teamB_score) {teamA_win_flag <- 1; teamB_win_flag <- 0}
        if(teamA_score == teamB_score) {teamA_win_flag <- 0.5; teamB_win_flag <- 0.5}
        if(teamA_score < teamB_score) {teamA_win_flag <- 0; teamB_win_flag <- 1}
    #did the loser get a losing bonus point?
        if(teamA_win_flag == 0 & teamA_score-teamB_score > -7) {teamA_losingbp_flag <- 1} else {teamA_losingbp_flag <- 0}
        if(teamB_win_flag == 0 & teamB_score-teamA_score > -7) {teamB_losingbp_flag <- 1} else {teamB_losingbp_flag <- 0}
    #did the team, either winner or loser, get a 4 try bonus point?
        if(teamA_tries >= 4) {teamA_trybp_flag <- 1} else {teamA_trybp_flag <- 0}
        if(teamB_tries >= 4) {teamB_trybp_flag <- 1} else {teamB_trybp_flag <- 0}
    #calculate the league points awarded given the match result
        teamA_pt <- teamA_win_flag * 4 + teamA_losingbp_flag * 1 + teamA_trybp_flag * 1  #note that a draw is denoted by win_flag 0.5
        teamB_pt <- teamB_win_flag * 4 + teamB_losingbp_flag * 1 + teamB_trybp_flag * 1
    #make a result data frame
        teamAB_result <- rbind(
                                c(teamA,teamA_win_flag-teamA_win_flag %% 1,(teamA_win_flag %% 1)*2,teamA_tries,teamA_score,teamB_score,teamA_pt),
                                c(teamB,teamB_win_flag-teamB_win_flag %% 1,(teamB_win_flag %% 1)*2,teamB_tries,teamB_score,teamA_score,teamB_pt)
                                )
        colnames(teamAB_result) <- c("team_name","wins","draws","tries","pts_for","pts_ag","table_pts")
    #make a match result data frame (to be used in event of a tiebreaker)
        teamAB_tiebreaker_record <- rbind(
                                c(paste0(teamA,teamB),teamA_win_flag,teamB_win_flag),
                                c(paste0(teamB,teamA),teamB_win_flag,teamA_win_flag)
                                )
        colnames(teamAB_tiebreaker_record) <- c("teams","teamA_win","teamB_win")
    #construct the output df
        output <- NULL
        output$teamAB_result <- teamAB_result
        output$teamAB_tiebreaker_record <- teamAB_tiebreaker_record
        return(output)
}
#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*


####################################################################################
### 4) Define Tournament Simulation Function
####################################################################################

#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*#*
#Simulate one instance of the tournament
simulate_rwc <- function(rtgs = ratings, rating_sensitivity = c(0,0)){

  fluidrtgs <- rtgs
  fluidrtgs$consensus <- fluidrtgs$consensus+runif(20,rating_sensitivity[1],rating_sensitivity[2])
  
      ### 4a) Group Stages Simulation
      pool_A_matches <- list(
                            c("JPN","RUS"),
                            c("IRE","SCO"),
                            c("RUS","SAM"),
                            c("JPN","IRE"),
                            c("SCO","SAM"),
                            c("IRE","RUS"),
                            c("JPN","SAM"),
                            c("SCO","RUS"),
                            c("IRE","SAM"),
                            c("JPN","SCO")
                            )
      pool_B_matches <- list(
                            c("NZL","RSA"),
                            c("ITA","NAM"),
                            c("ITA","CAN"),
                            c("RSA","NAM"),
                            c("NZL","CAN"),
                            c("RSA","ITA"),
                            c("NZL","NAM"),
                            c("RSA","CAN"),
                            c("NZL","ITA"),
                            c("NAM","CAN")
                          )
      pool_C_matches <- list(
                            c("FRA","ARG"),
                            c("ENG","TON"),
                            c("ENG","USA"),
                            c("ARG","TON"),
                            c("FRA","USA"),
                            c("ENG","ARG"),
                            c("FRA","TON"),
                            c("ARG","USA"),
                            c("ENG","FRA"),
                            c("USA","TON")
                            )
      pool_D_matches <- list(
                            c("AUS","FIJ"),
                            c("WAL","GEO"),
                            c("FIJ","URU"),
                            c("GEO","URU"),
                            c("AUS","WAL"),
                            c("GEO","FIJ"),
                            c("AUS","URU"),
                            c("WAL","FIJ"),
                            c("AUS","GEO"),
                            c("WAL","URU")
                            )
      
      {
      pool_A_results <- NULL
      pool_A_tiebreaker_records <- NULL
      for(mtch in pool_A_matches){
        #mtch        <- pool_matches[[1]]
        mtch_result <-rwc_match_sim(mtch[1],mtch[2],fluidrtgs)
        pool_A_results <- rbind(pool_A_results,
                                mtch_result$teamAB_result)
        pool_A_tiebreaker_records <- rbind(pool_A_tiebreaker_records,
                                           mtch_result$teamAB_tiebreaker_record)
      }
      
      pool_A_results <- data.frame(pool_A_results,stringsAsFactors = F)
      pool_A_results[-1] <- lapply(pool_A_results[-1], as.numeric)
      pool_A_results <- pool_A_results %>% group_by(team_name) %>% summarise_all(funs(sum))
      pool_A_results <- pool_A_results %>% arrange(-table_pts)
      pool_A_results
      pool_A_winner <- pool_A_results[1,"team_name"]
      pool_A_second <- pool_A_results[2,"team_name"] 
      }
      {
        pool_B_results <- NULL
        pool_B_tiebreaker_records <- NULL
        for(mtch in pool_B_matches){
          #mtch        <- pool_matches[[1]]
          mtch_result <-rwc_match_sim(mtch[1],mtch[2],fluidrtgs)
          pool_B_results <- rbind(pool_B_results,
                                  mtch_result$teamAB_result)
          pool_B_tiebreaker_records <- rbind(pool_B_tiebreaker_records,
                                             mtch_result$teamAB_tiebreaker_record)
        }
        
        pool_B_results <- data.frame(pool_B_results,stringsAsFactors = F)
        pool_B_results[-1] <- lapply(pool_B_results[-1], as.numeric)
        pool_B_results <- pool_B_results %>% group_by(team_name) %>% summarise_all(funs(sum))
        pool_B_results <- pool_B_results %>% arrange(-table_pts)
        pool_B_results
        pool_B_winner <- pool_B_results[1,"team_name"]
        pool_B_second <- pool_B_results[2,"team_name"] 
      }
      {
        pool_C_results <- NULL
        pool_C_tiebreaker_records <- NULL
        for(mtch in pool_C_matches){
          #mtch        <- pool_matches[[1]]
          mtch_result <-rwc_match_sim(mtch[1],mtch[2],fluidrtgs)
          pool_C_results <- rbind(pool_C_results,
                                  mtch_result$teamAB_result)
          pool_C_tiebreaker_records <- rbind(pool_C_tiebreaker_records,
                                             mtch_result$teamAB_tiebreaker_record)
        }
        
        pool_C_results <- data.frame(pool_C_results,stringsAsFactors = F)
        pool_C_results[-1] <- lapply(pool_C_results[-1], as.numeric)
        pool_C_results <- pool_C_results %>% group_by(team_name) %>% summarise_all(funs(sum))
        pool_C_results <- pool_C_results %>% arrange(-table_pts)
        pool_C_results
        pool_C_winner <- pool_C_results[1,"team_name"]
        pool_C_second <- pool_C_results[2,"team_name"] 
      }
      {
        pool_D_results <- NULL
        pool_D_tiebreaker_records <- NULL
        for(mtch in pool_D_matches){
          #mtch        <- pool_matches[[1]]
          mtch_result <-rwc_match_sim(mtch[1],mtch[2],fluidrtgs)
          pool_D_results <- rbind(pool_D_results,
                                  mtch_result$teamAB_result)
          pool_D_tiebreaker_records <- rbind(pool_D_tiebreaker_records,
                                             mtch_result$teamAB_tiebreaker_record)
        }
        
        pool_D_results <- data.frame(pool_D_results,stringsAsFactors = F)
        pool_D_results[-1] <- lapply(pool_D_results[-1], as.numeric)
        pool_D_results <- pool_D_results %>% group_by(team_name) %>% summarise_all(funs(sum))
        pool_D_results <- pool_D_results %>% arrange(-table_pts)
        pool_D_results
        pool_D_winner <- pool_D_results[1,"team_name"]
        pool_D_second <- pool_D_results[2,"team_name"] 
      }
      
      ### 4b) Knock-out Stages Simulation
      
      #QF1: WINNER POOL C VS RUNNER UP POOL D
      qf1_draw_flag <- T
      while(qf1_draw_flag == T){    #simulate match until a no-draw result is given
      qf1_result <- rwc_match_sim(pool_C_winner,pool_D_second,fluidrtgs)
      qf1_draw_flag   <- qf1_result$teamAB_result[1,"draws"] == 1
      }
      qf1_winner <- qf1_result$teamAB_result[qf1_result$teamAB_result[,"wins"] == 1,"team_name"]
      qf1_loser <- qf1_result$teamAB_result[qf1_result$teamAB_result[,"wins"] == 0,"team_name"]
      
      #QF2: WINNER POOL B VS RUNNER UP POOL A
      qf2_draw_flag <- T
      while(qf2_draw_flag == T){    #simulate match until a no-draw result is given
        qf2_result <- rwc_match_sim(pool_B_winner,pool_A_second,fluidrtgs)
        qf2_draw_flag   <- qf2_result$teamAB_result[1,"draws"] == 1
      }
      qf2_winner <- qf2_result$teamAB_result[qf2_result$teamAB_result[,"wins"] == 1,"team_name"]
      qf2_loser <- qf2_result$teamAB_result[qf2_result$teamAB_result[,"wins"] == 0,"team_name"]
      
      #QF3: WINNER POOL D VS RUNNER UP POOL C
      qf3_draw_flag <- T
      while(qf3_draw_flag == T){    #simulate match until a no-draw result is given
        qf3_result <- rwc_match_sim(pool_D_winner,pool_C_second,fluidrtgs)
        qf3_draw_flag   <- qf3_result$teamAB_result[1,"draws"] == 1
      }
      qf3_winner <- qf3_result$teamAB_result[qf3_result$teamAB_result[,"wins"] == 1,"team_name"]
      qf3_loser <- qf3_result$teamAB_result[qf3_result$teamAB_result[,"wins"] == 0,"team_name"]
      
      #QF4: WINNER POOL A VS RUNNER UP POOL B
      qf4_draw_flag <- T
      while(qf4_draw_flag == T){    #simulate match until a no-draw result is given
        qf4_result <- rwc_match_sim(pool_A_winner,pool_B_second,fluidrtgs)
        qf4_draw_flag   <- qf4_result$teamAB_result[1,"draws"] == 1
      }
      qf4_winner <- qf4_result$teamAB_result[qf4_result$teamAB_result[,"wins"] == 1,"team_name"]
      qf4_loser <- qf4_result$teamAB_result[qf4_result$teamAB_result[,"wins"] == 0,"team_name"]
      
      #SF1: WINNER QF1 VS WINNER QF2
      sf1_draw_flag <- T
      while(sf1_draw_flag == T){    #simulate match until a no-draw result is given
        sf1_result <- rwc_match_sim(qf1_winner,qf2_winner,fluidrtgs)
        sf1_draw_flag   <- sf1_result$teamAB_result[1,"draws"] == 1
      }
      sf1_winner <- sf1_result$teamAB_result[sf1_result$teamAB_result[,"wins"] == 1,"team_name"]
      sf1_loser <- sf1_result$teamAB_result[sf1_result$teamAB_result[,"wins"] == 0,"team_name"]
      
      #SF2: WINNER QF3 VS WINNER QF4
      sf2_draw_flag <- T
      while(sf2_draw_flag == T){    #simulate match until a no-draw result is given
        sf2_result <- rwc_match_sim(qf3_winner,qf4_winner,fluidrtgs)
        sf2_draw_flag   <- sf2_result$teamAB_result[1,"draws"] == 1
      }
      sf2_winner <- sf2_result$teamAB_result[sf2_result$teamAB_result[,"wins"] == 1,"team_name"]
      sf2_loser <- sf2_result$teamAB_result[sf2_result$teamAB_result[,"wins"] == 0,"team_name"]
      
      #BRONZE FINAL: LOSER SF1 VS LOSER SF2
      bronze_draw_flag <- T
      while(bronze_draw_flag == T){    #simulate match until a no-draw result is given
        bronze_result <- rwc_match_sim(sf1_loser,sf2_loser,fluidrtgs)
        bronze_draw_flag   <- bronze_result$teamAB_result[1,"draws"] == 1
      }
      bronze_winner <- bronze_result$teamAB_result[bronze_result$teamAB_result[,"wins"] == 1,"team_name"]
      bronze_loser <- bronze_result$teamAB_result[bronze_result$teamAB_result[,"wins"] == 0,"team_name"]
      
      #FINAL: WINNER SF1 VS WINNER SF2
      rwcfinal_draw_flag <- T
      while(rwcfinal_draw_flag == T){    #simulate match until a no-draw result is given
        rwcfinal_result <- rwc_match_sim(sf1_winner,sf2_winner,fluidrtgs)
        rwcfinal_draw_flag   <- rwcfinal_result$teamAB_result[1,"draws"] == 1
      }
      rwcfinal_winner <- rwcfinal_result$teamAB_result[rwcfinal_result$teamAB_result[,"wins"] == 1,"team_name"]
      rwcfinal_loser <- rwcfinal_result$teamAB_result[rwcfinal_result$teamAB_result[,"wins"] == 0,"team_name"]
      
      rwc_simulation_result <- rbind(
            rwcfinal_winner,
            rwcfinal_loser,
            bronze_winner,
            bronze_loser,
            qf1_loser,
            qf2_loser,
            qf3_loser,
            qf4_loser,
            pool_A_results[1,"team_name"],
            pool_A_results[2,"team_name"],
            pool_A_results[3,"team_name"],
            pool_A_results[4,"team_name"],
            pool_A_results[5,"team_name"],
            pool_B_results[1,"team_name"],
            pool_B_results[2,"team_name"],
            pool_B_results[3,"team_name"],
            pool_B_results[4,"team_name"],
            pool_B_results[5,"team_name"],
            pool_C_results[1,"team_name"],
            pool_C_results[2,"team_name"],
            pool_C_results[3,"team_name"],
            pool_C_results[4,"team_name"],
            pool_C_results[5,"team_name"],
            pool_D_results[1,"team_name"],
            pool_D_results[2,"team_name"],
            pool_D_results[3,"team_name"],
            pool_D_results[4,"team_name"],
            pool_D_results[5,"team_name"]
      )
      
      return(as.data.frame(rwc_simulation_result))
}

####################################################################################
### 5) Monte Carlo Simulation of Multiple Tournaments
####################################################################################

rwc_simulations_results <- list()
for(i in 1:10000){
rwc_simulations_results[[i]] <- simulate_rwc(rtgs=ratings,
                                             rating_sensitivity=c(-5,5))
if(i %% 50 == 0) print(i)
}
rwc_simulations_results <- data.frame(rwc_simulations_results,stringsAsFactors = F)
rownames(rwc_simulations_results) <- c("rwcfinal_winner","rwcfinal_loser",
                                       "bronze_winner","bronze_loser",
                                       "qf1_loser","qf2_loser","qf3_loser","qf4_loser",
                                       "pool_A_1","pool_A_2","pool_A_3","pool_A_4","pool_A_5",
                                       "pool_B_1","pool_B_2","pool_B_3","pool_B_4","pool_B_5",
                                       "pool_C_1","pool_C_2","pool_C_3","pool_C_4","pool_C_5",
                                       "pool_D_1","pool_D_2","pool_D_3","pool_D_4","pool_D_5")

table(as.character(rwc_simulations_results[1,]))
#write.csv(rwc_simulations_results,"rwc_simulations_results.csv")

