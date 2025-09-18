###作業二參考解答###

##第一小題：籃球投籃命中率模擬
#方法一：用sample產生資料
simple_shooting_simulation <- function(num_shots,hit_rate){
  hit_result <- sample(0:1,size=num_shots,replace=TRUE,prob=c(1-hit_rate, hit_rate))
  total_hits <- sum(hit_result)
  actual_hit_rate <- total_hits/num_shots
  
  return(list(命中的總次數=total_hits,實際命中率=actual_hit_rate))
  
}
set.seed(123)
results <- simple_shooting_simulation(10000, 0.75)
cat(paste0("命中的總次數: ", results[1], "\n", "實際命中率: ", results[2]))

#方法二：用rbinom產生資料
simple_shooting_simulation <- function(num_shots, hit_rate){
  total_hits <- rbinom(1, size = num_shots, prob = hit_rate)
  actual_hit_rate <- total_hits/num_shots
  return(c(total_hits, actual_hit_rate))
}
set.seed(123)
results <- simple_shooting_simulation(10000, 0.75)
cat(paste0("命中的總次數: ", results[1], "\n", "實際命中率: ", results[2]))

##第二小題模擬籃球系列賽
#方法一
basketball_series <- function(team1_name, team2_name){
  team1_wins <- 0
  team2_wins <- 0
  i=1
  
  while(team1_wins < 4 && team2_wins < 4){
    team1_score <- sample(80:120,1,replace=TRUE)
    team2_score <- sample(80:120,1,replace=TRUE)
    
    if (team1_score > team2_score){
      team1_wins <- team1_wins + 1
      cat(paste("第",i,"場比賽","\n",team1_name,":",team1_score,"vs",team2_name,":",team2_score,"\n"))
      i <- i+1
    }
    else{
      team2_wins <- team2_wins + 1
      cat(paste("第",i,"場比賽","\n",team1_name,":",team1_score,"vs",team2_name,":",team2_score,"\n"))
      i <- i+1
    }
  }  
  
  if (team1_wins == 4){
    cat(paste("最終結果:",team1_name,"獲勝"))
  }
  else {
    cat(paste("最終結果:",team2_name,"獲勝"))
  }
  
  
}
set.seed(123)
basketball_series("Team1", "Team2")

#方法二
basketball_series <- function(team1_name, team2_name){
  i=0
  team1=0
  team2=0
  while(max(team1, team2) != 4){
    i=i+1
    points=sample(80:120, 2, replace = FALSE)
    cat(paste0("第", i, "場:", "\n", team1_name, ":", points[1], " v.s ", team2_name, ":", points[2], "\n"))
    if(points[1]>points[2]) team1=team1+1 else team2=team2+1
  }
  if(team1==4) cat(paste0("\n", "最終結果: ", team1_name, " 獲勝")) else cat(paste0("\n", "最終結果: ", team2_name, " 獲勝"))
}

set.seed(123)
basketball_series("Team1", "Team2")


###決策樹###
library(rpart)
library(rpart.plot) 

data <- data.frame(House=factor(c('yes','no','no','yes','no','no','yes','no','no','no')),
                   Marriage=factor(c('single','married','single','married','divorced','married','divorced','single','married','single')),
                   Income=c(125,100,70,120,95,60,220,85,75,90),
                   In_Debt=factor(c('0','0','0','0','1','0','0','1','0','1')))
#先放Marriage
cart_model_1 <- rpart(In_Debt ~ House + Marriage + Income, data = data, method = "class",control =rpart.control(minsplit =1,minbucket=1, cp=0))
rpart.plot(cart_model_1,digits = 2)
rpart.rules(cart_model_1,cover=T)

#先放Income
cart_model_2 <- rpart(In_Debt ~Income+House + Marriage, data = data, method = "class",control =rpart.control(minsplit =1,minbucket=1, cp=0))
rpart.plot(cart_model_2,digits = 2)
rpart.rules(cart_model_2,cover=T)

