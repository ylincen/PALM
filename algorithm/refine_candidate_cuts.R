# This function is for removing cut points with no data points nearby
# input:
# x: 1d data
# candi_cuts: candidate_cuts, the left boundary EXCLUDED,
#              the right boundary INCLUDE, 
#              and candi_cuts already rounded and corrected for floating points error

# output: 
# refined candidated cuts
# BIPRODUCT: counts!
refine_candidate_cuts = function(x, candi_cuts, left_excluded=T, debug_flag=F){
  
  # remove the left boundary if it was not removed
  if(!left_excluded){
    candi_cuts = candi_cuts[-1]
  }
  
  # If the original candidate cuts has length 2, and now length 1
  # with the left boundary EXCLUDED
  if(length(candi_cuts) < 2){
    #return(candi_cuts)
    return(list(refined_candidate_cuts = candi_cuts,
         counts = length(x)))
  }
  
  x = sort(x) # complexity = n*log2(n) 
  
  # BIPRODUCT: get counts
  counts = rep(0, length(candi_cuts))

  i = 1
  exclude = rep(FALSE, length(candi_cuts))
  done = FALSE
  for(j in 1:length(candi_cuts)){ # \O(length(candi_cuts)) complexity
    cut = candi_cuts[j]
    
    if(done){
      break
    }

    if(isTRUE((cut <= x[i]) == T | (cut <= x[i]) == F)){
      # pass
    } else{
      browser()
    }
    if(debug_flag ==T){
      print(i)
    }
    
    if(debug_flag ==T & i ==194){
      browser()
    }
    
    if(cut <= x[i]){ 
      exclude[j] = TRUE
      if(j==1){
        counts[j] = 0
      } else{
        counts[j] = counts[j-1]
      }
    } else{
      exclude[j - 1] = FALSE
      i = i + 1
      
      if(j == 1){
        counts[j] = 1
      } else{
        counts[j] = counts[j-1] + 1
      }
      if(i > length(x)){
        if(j+1 < length(exclude)){# remove all empty bins from the cut point right to max(x) and max(candi_cuts) 
          exclude[(j+1):(length(exclude)-1)] = TRUE
        }
        counts[(j+1):length(exclude)] = counts[j]
        break
      }
      
      # make sure that the next x we check is larger than the current candidate cut
      while(x[i] < cut){ 
        i = i + 1
        counts[j] = counts[j] + 1
        if(i > length(x)){
          if(j+1 < length(exclude)){ # remove all empty bins from the cut point right to max(x) and max(candi_cuts) 
            exclude[(j+1):(length(exclude)-1)] = TRUE
          }
          done=TRUE
          counts[(j+1):length(exclude)] = counts[j]
          break
        }
      }
    }
  }
  
  refined_candidate_cuts = candi_cuts[!exclude]
  counts = counts[!exclude]
  return(list(refined_candidate_cuts = refined_candidate_cuts,
              counts = counts))
}

# Doc: (TO BE POLISHED)
# Proof that the algorithm is correct. That is, to prove:
# 1. If some cut point c[j] is T, then it must be within an empty bin.
# 2. If some cut point is F, then it must has a data point 
# near it to the left or right, or it is the left or right boundary. 

# Proof: 
# a) As we search from left to right, the first candidate point that 
# is larger than x[1] is F, and the candidate point with one index
# smaller is also F, but all candidate points further left (if any)
# are T. We mark these two 'F' cut points as c_t1 and c_t2, or c[t1], 
# c[t2]. 
# As all T cut points are smaller than c[t1], they are within the 
# empty bin [C_til[1], c[t1]), where C_til[1] is excluded from the 
# input. The two F cut points are near x[1].

# b) Now we move to x[i] such that x[i-1] < c_t2 and x[i] >= c_t2,
# where i >= 2. 
# b.i) If i > length(x), then i - 1 = length(x), i.e., only c_t2 
# and the right boundary (if not c_t2) are F, any other candidate
# cut points between them (if they exist) are T. 
# b.ii) If i <= length(x), then we check c[t2 + 1], ..., until 
# we find some j such that c[j] > x[i] but c[j-1] <= x[i], we mark 
# c[j] and c[j-1] as F, but all c[t2+1],..., c[j-2] are False. 
# Note that the extreme case is that c[t2 + 1] > x[i] and 
# c[t2] <= x[i], then c[t2] and c[t2 + 1] are both FALSE. 

# c) We keep doing this, until we hit x[length(x)]. Assume some k
# such that c[k] > x[length(x)] and c[k-1] <= x[length(x)], then
# c[k + 1], ..., c[length(c)-1] are TRUE, c[k] and c[k-1] and 
# c[length(c)] are FALSE. This completes the proof. 