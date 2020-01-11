corrects_results = vector(length = 100)
for(j in 1:100){
  
  results = vector(length = 10000)
  
  for(i in 1:10000){
    v = rnorm(3)
    first_door = rnorm(3)
    first_door = which(first_door == max(first_door))
    
    max_v = which(v == max(v))
    
    empty_door = which(v != v[max_v] & v != v[first_door])
    if(length(empty_door) == 2){
      random = rnorm(2)
      random = which(random == max(random))
      empty_door = empty_door[random] 
    } 
    
    #cambia de puerta? si
    
    change_door = which(v != v[empty_door] & v != v[first_door])
    
    
    
    if(v[change_door] == max(v)){
      results[i] = 1
    } else {
      results[i] = 0 
    }
    
  }
  
  corrects = length(which(results == 1))
  
  
  corrects_results[j] = corrects
  
  #print(paste("Correctos: ", as.character(correctos)))
  

}

porcentaje_acertado = mean(corrects_results)*100/10000

print(paste("Porcentaje acertado: ", as.character(round(porcentaje_acertado,digits = 2)), "%", sep = ""))
