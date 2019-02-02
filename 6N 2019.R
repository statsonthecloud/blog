library(evd)

#define the Rugby Pass Index ratings as of 31 Jan 2019, a day before tournament start
sixnations <- data.frame(team = c("IRE","ENG","SCO","WAL","FRA","ITA"),rpass = c(86,83,74,82,69,38),stringsAsFactors = FALSE)

#calculate the pwin for a given pair of teams
calculate_pair_pwin <- function(team1,team2){
rpass_diff <- sixnations[sixnations$team == team1,"rpass"]-sixnations[sixnations$team == team2,"rpass"]
home <- 50 + sign(rpass_diff) * abs(rpass_diff) + 5
home <- min(home,98)
home <- max(home,2)
away <- 100 - home
c(home,away)*0.01
}

list_of_matches <- list(c("FRA","WAL"),
                        c("SCO","ITA"),
                        c("IRE","ENG"),
                        c("SCO","IRE"),
                        c("ITA","WAL"),
                        c("ENG","FRA"),
                        c("FRA","SCO"),
                        c("WAL","ENG"),
                        c("ITA","IRE"),
                        c("SCO","WAL"),
                        c("ENG","ITA"),
                        c("IRE","FRA"),
                        c("ITA","FRA"),
                        c("WAL","IRE"),
                        c("ENG","SCO"))

#simulate 10,000 tournaments, j
for(j in 1:10000){
if(j==1) {results_df_collated <- data.frame(team = c("IRE","ENG","SCO","WAL","FRA","ITA","IREw","ENGw","SCOw","WALw","FRAw","ITAw","IREp","ENGp","SCOp","WALp","FRAp","ITAp"))}

#simulate each of the 15 matches, i
for(i in 1:15){
if(i == 1) {results_df <- data.frame(team = c("IRE","ENG","SCO","WAL","FRA","ITA","IREw","ENGw","SCOw","WALw","FRAw","ITAw","IREp","ENGp","SCOp","WALp","FRAp","ITAp"), pt = 0)}

a_teamname <- list_of_matches[i][[1]][1]
b_teamname <- list_of_matches[i][[1]][2]

ab_pwin <- calculate_pair_pwin(a_teamname,b_teamname)
a_relative_odds <- ab_pwin[1]*2
b_relative_odds <- ab_pwin[2]*2

#take a random sample from distribution fitted from Pro 14 data 2017/18 season
#scale it by the pwin
a_num_tries <- pmax(round(evd::rgumbel(1,loc = 1.99 * a_relative_odds,scale = 1.49*a_relative_odds),0),0) #number of tries scored
a_num_convs <- sum(round(runif(a_num_tries,0,1)*3/2,0)) #assumed to the dependent on num_tries but not on overall odds
a_num_pens <- pmax(round(evd::rgumbel(1,loc = 0.89 * a_relative_odds,scale = 0.98 * a_relative_odds),0),0) #number of penalties scored

b_num_tries <- pmax(round(evd::rgumbel(1,loc = 1.99 * b_relative_odds,scale = 1.49*b_relative_odds),0),0) #number of tries scored
b_num_convs <- sum(round(runif(b_num_tries,0,1)*3/2,0)) #assumed to the dependent on num_tries but not on overall odds
b_num_pens <- pmax(round(evd::rgumbel(1,loc = 0.89 * b_relative_odds,scale = 0.98 * b_relative_odds),0),0) #number of penalties scored

a_score <- a_num_tries * 5 + a_num_convs * 2 + a_num_pens * 3
b_score <- b_num_tries * 5 + b_num_convs * 2 + b_num_pens * 3

#work out who won, lost or drew
if(a_score > b_score) {a_win_flag <- 1; b_win_flag <- 0}
if(a_score == b_score) {a_win_flag <- 0.5; b_win_flag <- 0.5}
if(a_score < b_score) {a_win_flag <- 0; b_win_flag <- 1}

#did the loser get a losing bonus point?
if(a_win_flag == 0 & a_score-b_score > -7) {a_losingbp_flag <- 1} else {a_losingbp_flag <- 0}
if(b_win_flag == 0 & b_score-a_score > -7) {b_losingbp_flag <- 1} else {b_losingbp_flag <- 0}

#did the team, either winner or loser, get a 4 try bonus point?
if(a_num_tries >= 4) {a_trybp_flag <- 1} else {b_trybp_flag <- 1}
if(b_num_tries >= 4) {b_trybp_flag <- 1} else {a_trybp_flag <- 1}

##calculate the league points awarded given the match result##
a_pt <- a_win_flag * 4 + a_losingbp_flag * 1 + a_trybp_flag * 1
b_pt <- b_win_flag * 4 + b_losingbp_flag * 1 + b_trybp_flag * 1

#bring all of the results together into one column of a dataframe, ready for collation across all tournaments
#results show three things: number of tournament points, number of wins (to enable calculation of grand slams), and tournament ranking
#pt
results_df[results_df$team == a_teamname,"pt"] <- results_df[results_df$team == a_teamname,"pt"] + a_pt
results_df[results_df$team == b_teamname,"pt"] <- results_df[results_df$team == b_teamname,"pt"] + b_pt
#wins
results_df[results_df$team == paste0(a_teamname,"w"),"pt"] <- results_df[results_df$team == paste0(a_teamname,"w"),"pt"] + floor(a_win_flag)
results_df[results_df$team == paste0(b_teamname,"w"),"pt"] <- results_df[results_df$team == paste0(b_teamname,"w"),"pt"] + floor(b_win_flag)
#if a team achieved a grand slam, then they receive 3pt extra at the end
if(results_df[results_df$team == paste0(a_teamname,"w"),"pt"] == 5) {results_df[results_df$team == a_teamname,"pt"] <- results_df[results_df$team == a_teamname,"pt"] + 3}
if(results_df[results_df$team == paste0(b_teamname,"w"),"pt"] == 5) {results_df[results_df$team == b_teamname,"pt"] <- results_df[results_df$team == b_teamname,"pt"] + 3}
#ranking
if(i == 15){
rankings <- cbind(results_df[1:6,][order(results_df[1:6,'pt'],decreasing = TRUE),],pt=1:6)[,-2]
rankings$team <- paste0(rankings$team,"p")
results_df[results_df$team == "IREp",2] <- rankings[rankings$team == "IREp",2]
results_df[results_df$team == "ENGp",2] <- rankings[rankings$team == "ENGp",2]
results_df[results_df$team == "SCOp",2] <- rankings[rankings$team == "SCOp",2]
results_df[results_df$team == "WALp",2] <- rankings[rankings$team == "WALp",2]
results_df[results_df$team == "FRAp",2] <- rankings[rankings$team == "FRAp",2]
results_df[results_df$team == "ITAp",2] <- rankings[rankings$team == "ITAp",2]
}
}

#add to one tournament dataframe
results_df_collated <- cbind(results_df_collated,results_df[,2])
#progress counter
if(j %% 50 == 0) {print(j)}
}

##### analyse the results
#yet more dataframe manipulation (all of the data frame manipulation could do with a major tidy up!!)
results_working <- t(results_df_collated)
results_working <- results_working[-1,]
colnames(results_working) <- c("IRE","ENG","SCO","WAL","FRA","ITA","IREw","ENGw","SCOw","WALw","FRAw","ITAw","IREp","ENGp","SCOp","WALp","FRAp","ITAp")
rownames(results_working) <- 1:dim(results_working)[1]

#boxplot of league pts
png(filename="6N_2019_pts_boxplot_1.png",width=600,height=600)
boxplot(list(as.numeric(results_working[,1]),
          as.numeric(results_working[,2]),
          as.numeric(results_working[,4]),
          as.numeric(results_working[,3]),
          as.numeric(results_working[,5]),
          as.numeric(results_working[,6])),
        names = c("IRE","ENG","WAL","SCO","FRA","ITA"),
        main = "Distribution of league points",
        sub = "Six Nations 2019 simulation results, n = 10,000",
        ylab = "final league points",
        ylim = c(0,30))
dev.off()

#boxplot of standings
png(filename="6N_2019_std_boxplot_1.png",width=600,height=600)
boxplot(list(as.numeric(results_working[,13]),
             as.numeric(results_working[,14]),
             as.numeric(results_working[,16]),
             as.numeric(results_working[,15]),
             as.numeric(results_working[,17]),
             as.numeric(results_working[,18])),
        names = c("IRE","ENG","WAL","SCO","FRA","ITA"),
        main = "Distribution of standings",
        sub = "Six Nations 2019 simulation results, n = 10,000",
        ylab = "final standing (1=winner, 6=bottom)",
        ylim = c(6,1))
dev.off()

#barplot of Grand Slam occurrences
png(filename="6N_2019_GS_barplot_1.png",width=600,height=600)
grandslam_occurrences <- c((10000 - sum(results_working[,7:12] == " 5"))/100,
  sum(results_working[,7] == " 5")/100,
  sum(results_working[,8] == " 5")/100,
  sum(results_working[,10] == " 5")/100,
  sum(results_working[,9] == " 5")/100,
  sum(results_working[,11] == " 5")/100,
  sum(results_working[,12] == " 5")/100)

barplot(grandslam_occurrences,
names = c("None","IRE","ENG","WAL","SCO","FRA","ITA"),
main = "How often did a team win the Grand Slam in the simulation?",
ylab = "% of Grand Slam wins",
sub = "Six Nations 2019 simulation results, n = 10,000",
ylim=c(0,70)
)
text(0.75,grandslam_occurrences[1]+3,paste0(round(grandslam_occurrences[1],1),"%"),adj=c(0.5,0.5))
text(0.75+1.2*1,grandslam_occurrences[2]+3,paste0(round(grandslam_occurrences[2],1),"%"),adj=c(0.5,0.5))
text(0.75+1.2*2,grandslam_occurrences[3]+3,paste0(round(grandslam_occurrences[3],1),"%"),adj=c(0.5,0.5))
text(0.75+1.2*3,grandslam_occurrences[4]+3,paste0(round(grandslam_occurrences[4],1),"%"),adj=c(0.5,0.5))
text(0.75+1.2*4,grandslam_occurrences[5]+3,paste0(round(grandslam_occurrences[5],1),"%"),adj=c(0.5,0.5))
text(0.75+1.2*5,grandslam_occurrences[6]+3,paste0(round(grandslam_occurrences[6],1),"%"),adj=c(0.5,0.5))
text(0.75+1.2*6,grandslam_occurrences[7]+3,paste0(round(grandslam_occurrences[7],1),"%"),adj=c(0.5,0.5))
dev.off()

#barplot of victories
png(filename="6N_2019_VIC_barplot_1.png",width=600,height=600)
victory_occurrences <- c( sum(results_working[,13] == " 1")/100,
                           sum(results_working[,14] == " 1")/100,
                           sum(results_working[,16] == " 1")/100,
                           sum(results_working[,15] == " 1")/100,
                           sum(results_working[,17] == " 1")/100,
                           sum(results_working[,18] == " 1")/100)

barplot(victory_occurrences,
        names = c("IRE","ENG","WAL","SCO","FRA","ITA"),
        main = "How often did a team win the Six Nations 2019 in the simulation?",
        ylab = "% of tournament wins",
        sub = "Six Nations 2019 simulation results, n = 10,000",
        ylim=c(0,70)
)
text(0.75,victory_occurrences[1]+3,paste0(round(victory_occurrences[1],1),"%"),adj=c(0.5,0.5))
text(0.75+1.2*1,victory_occurrences[2]+3,paste0(round(victory_occurrences[2],1),"%"),adj=c(0.5,0.5))
text(0.75+1.2*2,victory_occurrences[3]+3,paste0(round(victory_occurrences[3],1),"%"),adj=c(0.5,0.5))
text(0.75+1.2*3,victory_occurrences[4]+3,paste0(round(victory_occurrences[4],1),"%"),adj=c(0.5,0.5))
text(0.75+1.2*4,victory_occurrences[5]+3,paste0(round(victory_occurrences[5],1),"%"),adj=c(0.5,0.5))
text(0.75+1.2*5,victory_occurrences[6]+3,paste0(round(victory_occurrences[6],1),"%"),adj=c(0.5,0.5))
text(0.75+1.2*6,victory_occurrences[7]+3,paste0(round(victory_occurrences[7],1),"%"),adj=c(0.5,0.5))
dev.off()


#Rugby Pass rating
png(filename="6N_2019_rpassratings_2.png",width=600,height=600)
barplot(sixnations[c(1,2,4,3,5,6),'rpass'],ylim=c(0,100),
names = c("IRE","ENG","WAL","SCO","FRA","ITA"),
main = "Rugby Pass Index Ratings",
ylab = "Rating",
sub = "Ratings obtained from index.rugbypass.com on 31 Jan 2019")
text(0.75+1.2*0,sixnations[c(1,2,4,3,5,6),'rpass'][1]+3,sixnations[c(1,2,4,3,5,6),'rpass'][1],adj=c(0.5,0.5))
text(0.75+1.2*1,sixnations[c(1,2,4,3,5,6),'rpass'][2]+3,sixnations[c(1,2,4,3,5,6),'rpass'][2],adj=c(0.5,0.5))
text(0.75+1.2*2,sixnations[c(1,2,4,3,5,6),'rpass'][3]+3,sixnations[c(1,2,4,3,5,6),'rpass'][3],adj=c(0.5,0.5))
text(0.75+1.2*3,sixnations[c(1,2,4,3,5,6),'rpass'][4]+3,sixnations[c(1,2,4,3,5,6),'rpass'][4],adj=c(0.5,0.5))
text(0.75+1.2*4,sixnations[c(1,2,4,3,5,6),'rpass'][5]+3,sixnations[c(1,2,4,3,5,6),'rpass'][5],adj=c(0.5,0.5))
text(0.75+1.2*5,sixnations[c(1,2,4,3,5,6),'rpass'][6]+3,sixnations[c(1,2,4,3,5,6),'rpass'][6],adj=c(0.5,0.5))
dev.off()

#tries distribution
png(filename="PRO14_201718_triesdistn_1.png",width=600,height=600)
plot(seq(0,10,1),evd::dgumbel(seq(0,10,1),loc = 1.99, scale = 1.49),ylab="probability of occurrence",xlab="tries scored",main="A distribution for number of tries scored",sub="Gumbel distribution fitted to 2017/18 season Pro 14 data")
lines(seq(0,10,0.01),evd::dgumbel(seq(0,10,0.01),loc = 1.99, scale = 1.49),col="grey")       #number of tries scored
dev.off()

#penalties distribution
png(filename="PRO14_201718_pensdistn_1.png",width=600,height=600)
plot(seq(0,10,1),evd::dgumbel(seq(0,10,1),loc = 0.89, scale = 0.98),ylab="probability of occurrence",xlab="penalties scored",main="A distribution for number of penalties scored",sub="Gumbel distribution fitted to 2017/18 season Pro 14 data")
lines(seq(0,10,0.01),evd::dgumbel(seq(0,10,0.01),loc = 0.89, scale = 0.98),col="grey") 
dev.off()


