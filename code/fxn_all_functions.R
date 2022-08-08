######################################################################################################################################################
###### Functions

# Calculate percentage change
fxn_pct_change <- function(start_val, end_val, sig_fig){
  pct_change <- (start_val - end_val) / abs(x = start_val)
  pct_change_round <- round(x = pct_change, digits = sig_fig)
  return(pct_change_round)
}
