mh_per = function(n,m){
  corrects_results = vector(length = n)
  for(j in 1:n){
    
    results = vector(length = m)
    
    for(i in 1:m){
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
  
  porcentaje_acertado = mean(corrects_results)*100/m
  
  return(print(paste("Porcentaje acertado: ", as.character(round(porcentaje_acertado,digits = 2)), "%", sep = "")))
  
  
  
} 

monty_hall = function(){
  
  first_door = as.numeric(readline("Elija una puerta [1] [2] [3]: "))
  
  while(first_door > 3){
    first_door  = as.numeric(readline("Elija una puerta [1] [2] [3]: "))
  }
  
  v = rnorm(3)
  
  max_v = which(v == max(v))
  
  empty_door = which(v != v[max_v] & v != v[first_door])
  if(length(empty_door) == 2){
    random = rnorm(2)
    random = which(random == max(random))
    empty_door = empty_door[random] 
  }
  
  option_door = which(v != v[empty_door] & v != v[first_door])
  
  change = readline(paste("En la puerta ", as.character(empty_door), " no hay nada.", " Quiere cambiar su elección a la puerta ", as.character(option_door), "?: ", sep = ""))
  while(change != "si" & change != "no"){
    change = readline(paste("En la puerta ", as.character(empty_door), " no hay nada.", " Quiere cambiar su elección a la puerta ", as.character(option_door), "?: ", sep = ""))
  }
  if(change == "si"){
    change_door = which(v != v[empty_door] & v != v[first_door])
  } else {
    change_door = first_door
    }
  
  if(v[change_door] == v[max_v]){
    print("correcto")
  } else {
    print("incorrecto")
  }
}
