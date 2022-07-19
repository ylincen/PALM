shortline_to_pixel_points = function(shortlines, precision){
  shortline_vertical = shortlines[,shortlines[5,]==1, drop = F]
  shortline_horizontal = shortlines[,shortlines[5,]==2, drop = F]
  
  pixels = NULL
  
  vec = apply(shortline_vertical,2,function(l){
          return(seq(l[2],l[4],by = precision))
        })
  
  # apply function sometimes will return a matrix, stupid feature
  if(!is.list(vec)){
    vec = split(vec, 1:ncol(vec))
  }
  
  for(i in 1:length(vec)){
    pixels = rbind(pixels, cbind(shortline_vertical[1,i],vec[[i]]))
  }
  
  vec = apply(shortline_horizontal,2,function(l){
    return(seq(l[1],l[3],by = precision))
  })
  
  if(!is.list(vec)){
    vec = split(vec, 1:ncol(vec))
  }
  
  for(i in 1:length(vec)){
    pixels = rbind(pixels, cbind(vec[[i]],shortline_horizontal[2,i]))
  }
  
  
  
  return(pixels)
}