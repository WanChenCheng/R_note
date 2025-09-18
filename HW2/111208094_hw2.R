#1.
#寫function!!
simple_shooting_simulation <- function(num_shots, hit_rate){
  total_hits <-  rbinom(1, size = num_shots, prob = hit_rate)
  actual_hit_rate <- total_hits/num_shots
  hittt <- list(total_hits=total_hits,actual_hit_rate=actual_hit_rate)
  return(hittt)
}
simple_shooting_simulation(987654321,0.66)

#檢查
set.seed(123)
results <- simple_shooting_simulation(10000, 0.75)
cat(paste0("命中的總次數:",results[1],"\n","實際命中率:",results[2]))

#2.
#寫function!!
basketball_series <- function(team1_name = "Team 1", team2_name = "Team 2") {
  
  team1_wins <- 0  
  team2_wins <- 0 
  
  scores <- data.frame(Game = 1:7, 
                       Team1_Score = c(0, 0, 0, 0, 0, 0, 0), 
                       Team2_Score = c(0, 0, 0, 0, 0, 0, 0))
  #迴圈開始
  for (i in 1:7) {
    team1_score <- sample(80:120, 1)  
    team2_score <- sample(80:120, 1)  
    
    scores[i, "Team1_Score"] <- team1_score
    scores[i, "Team2_Score"] <- team2_score  
    
    
    if (team1_score > team2_score) {
      team1_wins <- team1_wins + 1
    } else if (team2_score > team1_score) {
      team2_wins <- team2_wins + 1
    } 
    
    if (team1_wins == 4 || team2_wins == 4) {break}
    
  } #迴圈結束
  
  if (team1_wins > team2_wins) {
    cat("最終結果:Team1獲勝")
  } else if (team2_wins > team1_wins) {
    cat("最終結果:Team2獲勝")
  } else {
    cat("平手")
  }
  return(scores)
}

# 測試
set.seed(123) 
basketball_series("Team 1", "Team 2")



