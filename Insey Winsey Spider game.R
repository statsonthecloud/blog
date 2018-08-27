#RETURNS RESULT OF ROLL OF A 6 SIDED DIE
six_sided_number_die_roll <- function()
  return(sample(x=c(1:6),size = 1,replace = TRUE))

#RETURNS RESULT OF ROLL OF SPECIAL SHAPE DIE
six_sided_shape_die_roll <- function() {
  rv <- sample(x=c(1:6),size = 1, replace = TRUE)
  shapes <- c('diamond','circle','semi-circle','triangle','rectangle','square')
  return(shapes[rv])
  }

#RETURNS RESULT OF SPIN OF SPECIAL WEATHER SPINNER
weather_spin <- function()
  return(sample(x=c('raining','sunshine'),size=1,replace = TRUE, prob = c(0.3,0.7)))

#SIMULATES ONE FULL GAME OF 'INSEY WINSEY SPIDER', NUMBER VERSION 
spider_game_number_run <- function() {
  spider_position = 0
  i = 0
    while (spider_position <= 10)
    {
      spider_position <- spider_position +six_sided_number_die_roll()
      if(weather_spin() == 'raining') spider_position <- 0
      i = i + 1
    }
return(i)
}

#SIMULATES k FULL GAMES OF 'INSEY WINSEY SPIDER', NUMBER VERSION
#51s on Lenovo laptop to run 100,000 iterations
spider_game_number_sim <- function(k) {
j = 1
agg <- NULL

while(j < k + 1)
{agg[j] <- spider_game_number_run()
       j = j + 1}
#return the number of turns it took to finish
return(agg)
}

n=200000
start_time <- Sys.time()
a <- spider_game_number_sim(n)
elapsed_time <- Sys.time() - start_time
a_mode <- getmode(a)
a_mean <- mean(a)
hist(a, freq = FALSE, breaks = seq(1:max(a)),
     main = paste("Insey Winsey Spider Game (number version),\n", format(n,scientific = FALSE, big.mark = ","),"simulation results"),
     xlab = paste("x, number of turns to complete game\nE(x)=",round(a_mean,2)," Mode=",a_mode))

quantile(a, 0.95)*15/60
quantile(a, 0.95^(1/2))*15/60*2
quantile(a, 0.05^(1/2))*15/60*2
     