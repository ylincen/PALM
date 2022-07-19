# This function gets the code length of model 
get_LM = function(C_til_ori, Kmax, num_dim, 
                  option = 1){
  if(option == 1){
    Lm = lchoose(length(C_til_ori), 0:(Kmax-1)) / log(2) # change to log2 as lchoose is log_e
    if(num_dim > 2){
      Lm = Lm + log2(num_dim - 1) # encode which dimension to split
    }
  } else if(option == 2){
    stop("Erorr in get_LM.R: option=2 will be developed in the future.")
  }
  return(Lm)
}

# Option 1:
# L(M) = positions of cut points locally

# Option 2: 
# Note that for each sub-regions that is being searched now, 
# the code length used to describe positions of cuts in other sub-regions
# is a constant. Thus, we only need to consider the L(M) locally.
# That is, L(M) = L(|D|) + L(which dim) + L(|cuts|) + L(which cuts)