# mode_diff
# inputs
#    - df.oscil_ref. Reference df
#    - list_of_df_oscill to compare with
#          it can take a single value or a range of values
#    - label = NULL (default). The code labels each df as a numbered list,
#                   starting in 1. 'ref' label is used for reference df.
#                   user labels will affect to dfs except the ref one.
# Condition
#    - All input df must constain $n, $l, and $sig
#
# Code workflow
#    1. Homogeinize all the dfs in n and l ranges to ensure squared matrices
#    2. Compute the frequency differences for each L matching the n_i
#    3. Store the results into a df

f_diff <- function(modelref.df, model.df, relative = F, negative = F){
  # Determine the global n and L ranges
  nlr_ref <- nl_range(modelref.df)
  nlr_mod <- nl_range(model.df)
  nlr <- c(max(nlr_ref[1], nlr_mod[1]), min(nlr_ref[2], nlr_mod[2]),
                max(nlr_ref[3], nlr_mod[3]), min(nlr_ref[4], nlr_mod[4])
                )
  # Restrict the frequencies to global (n,l) range
  condition_ref <- (modelref.df$l %in% nlr[1]:nlr[2]) &
    (modelref.df$n %in% nlr[3]:nlr[4])
  condition_mod <- (model.df$l %in% nlr[1]:nlr[2]) &
    (model.df$n %in% nlr[3]:nlr[4])

  # Compute the f diff and apply relative and negative conditions
  fref <- modelref.df$sig[condition_ref]
  fmod <- model.df$sig[condition_mod]

  if (negative == T) fdif <- fmod - fref else fdif <- abs(fmod-fref)
  if (relative == T) fdif <- df/fref
  return(data.frame(n = model.df$n[condition_mod],
                    l = model.df$l[condition_mod],
                    fdfiff = fdif)
         )
}


