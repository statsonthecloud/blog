####################################################################################
### Analysis of Monte Carlo simulation of Rugby World Cup 2019
####################################################################################
### Author: statsonthe.cloud
####################################################################################
### Purpose: to analyse the simulation results!
####################################################################################
### Revision A
####################################################################################

library(dplyr)
library(tidyr)
library(rowr)
library(ggplot2)

####################################################################################
### 1) Aggregate the results
####################################################################################

table(as.character(rwc_simulations_results[1,]))

simagg <- NA
for(i in 1:20){
    team <- ratings$team[i]
    teamres <- NULL
      for(j in 1:28){
      teamres <- c(teamres,sum(rwc_simulations_results[j,]==team))
      }
    teamres <- as.data.frame(teamres)
    rownames(teamres) <- rownames(rwc_simulations_results)
    colnames(teamres) <- team
    simagg <- cbind.fill(teamres,simagg)
}
simagg <- simagg[,-21]
rownames(simagg) <- rownames(rwc_simulations_results)
simagg

simagg <- rbind(simagg,qf_loser = colSums(simagg[5:8,]))
simagg <- rbind(simagg,made_qf_or_higher = colSums(simagg[1:8,]))
simagg <- rbind(simagg,made_sf_or_higher = colSums(simagg[1:4,]))
simagg <- rbind(simagg,made_f_or_higher = colSums(simagg[1:2,]))

####################################################################################
### 2a) finals plot
####################################################################################

simagg_norm <- simagg/rowSums(simagg)
simagg_finals <- gather(simagg_norm[c(30:32,1),])
simagg_finals$valueactual <- gather(simagg[c(30:32,1),])[,2]
simagg_finals$stage <- rep(rownames(simagg_norm)[c(30:32,1)],20)
simagg_finals$stage <- factor(simagg_finals$stage,rownames(simagg)[c(30:32,1)],ordered = T)
simagg_finals$key <- factor(simagg_finals$key,teams,ordered = T)

ggplot(simagg_finals, aes(key,stage)) + 
  geom_tile(aes(fill = value),color="grey") + 
  geom_text(aes(label = paste0(round(valueactual/100,1),"%"))) +
  #scale_fill_gradient2(low = "white",mid = "orangered",high="hotpink",midpoint = 0.2) +
  scale_fill_gradient2(low = "white",high="hotpink") +
  scale_y_discrete(labels=c("Made the QFs","Made the SFs","Made the Final","Won RWC")) +
  xlab("Team") +
  ylab("How far the team reached (cumulative probability)") +
  ggtitle("How far did each team progress?") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(lineheight = 1.5))

####################################################################################
### 2b) pools plot
####################################################################################

plots_midpoint <- 35*100

{
poolAagg <- simagg[13:9,]
poolAagg <- poolAagg[,colSums(poolAagg) !=0]
pooltemp <- gather(poolAagg)
pooltemp$stage <- rep(rownames(poolAagg),5)
pooltemp$stage <- factor(pooltemp$stage,rownames(poolAagg),ordered = T)
pooltemp$key <- factor(pooltemp$key,teams,ordered = T)

ggplot(pooltemp, aes(key,stage)) + 
  geom_tile(aes(fill = value),color="grey") + 
  geom_text(aes(label = paste0(round(value/100,1),"%"))) +
  scale_fill_gradient2(low = "white",mid="hotpink",high="hotpink",midpoint=plots_midpoint) +
  scale_y_discrete(labels=rev(c("Pool Winner","Pool Runner-up","Third Place","Fourth Place","Fifth Place"))) +
  xlab("Team") +
  ylab("Standing after all pool matches played") +
  ggtitle("Pool A: How did teams place?") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(lineheight = 1.5))
}
{
poolBagg <- simagg[18:14,]
poolBagg <- poolBagg[,colSums(poolBagg) !=0]
pooltemp <- gather(poolBagg)
pooltemp$stage <- rep(rownames(poolBagg),5)
pooltemp$stage <- factor(pooltemp$stage,rownames(poolBagg),ordered = T)
pooltemp$key <- factor(pooltemp$key,teams,ordered = T)

ggplot(pooltemp, aes(key,stage)) + 
  geom_tile(aes(fill = value),color="grey") + 
  geom_text(aes(label = paste0(round(value/100,1),"%"))) +
  scale_fill_gradient2(low = "white",mid="hotpink",high="hotpink",midpoint=plots_midpoint) +
  scale_y_discrete(labels=rev(c("Pool Winner","Pool Runner-up","Third Place","Fourth Place","Fifth Place"))) +
  xlab("Team") +
  ylab("Standing after all pool matches played") +
  ggtitle("Pool B: How did teams place?") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(lineheight = 1.5))
}
{
poolCagg <- simagg[23:19,]
poolCagg <- poolCagg[,colSums(poolCagg) !=0]
pooltemp <- gather(poolCagg)
pooltemp$stage <- rep(rownames(poolCagg),5)
pooltemp$stage <- factor(pooltemp$stage,rownames(poolCagg),ordered = T)
pooltemp$key <- factor(pooltemp$key,teams,ordered = T)

ggplot(pooltemp, aes(key,stage)) + 
  geom_tile(aes(fill = value),color="grey") + 
  geom_text(aes(label = paste0(round(value/100,1),"%"))) +
  scale_fill_gradient2(low = "white",mid="hotpink",high="hotpink",midpoint=plots_midpoint) +
  scale_y_discrete(labels=rev(c("Pool Winner","Pool Runner-up","Third Place","Fourth Place","Fifth Place"))) +
  xlab("Team") +
  ylab("Standing after all pool matches played") +
  ggtitle("Pool C: How did teams place?") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(lineheight = 1.5))
}
{
poolDagg <- simagg[28:24,]
poolDagg <- poolDagg[,colSums(poolDagg) !=0]
pooltemp <- gather(poolDagg)
pooltemp$stage <- rep(rownames(poolDagg),5)
pooltemp$stage <- factor(pooltemp$stage,rownames(poolDagg),ordered = T)
pooltemp$key <- factor(pooltemp$key,teams,ordered = T)

ggplot(pooltemp, aes(key,stage)) + 
  geom_tile(aes(fill = value),color="grey") + 
  geom_text(aes(label = paste0(round(value/100,1),"%"))) +
  scale_fill_gradient2(low = "white",mid="hotpink",high="hotpink",midpoint=plots_midpoint) +
  scale_y_discrete(labels=rev(c("Pool Winner","Pool Runner-up","Third Place","Fourth Place","Fifth Place"))) +
  xlab("Team") +
  ylab("Standing after all pool matches played") +
  ggtitle("Pool D: How did teams place?") +
  theme_minimal() +
  theme(legend.position="none") +
  theme(plot.title = element_text(lineheight = 1.5))
}

