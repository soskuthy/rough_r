# function for generating predictions from posterior of beta regression
# (only works when outcome is rating, and predictors are r + l)
# (any random effect structure is OK!)
beta_summary <- function (model, dat, rpred="all.r", lpred="all.l", binary_pred=T, printPlease=T) {
  brm_beta_post <- posterior_samples(model)
  rcol <- paste0("b_", rpred)
  lcol <- paste0("b_", lpred)
  if (binary_pred) {
    rcol <- paste0(rcol, "TRUE")
    lcol <- paste0(lcol, "TRUE")
  }
  
  # 1a - no R
  pred_rating_no_R <- (plogis(
    brm_beta_post[,1] + 
      mean(dat[,lpred][[1]])*brm_beta_post[,lcol]
  )*14)-7
  
  # 1a - yes R
  pred_rating_yes_R <- (plogis(
    brm_beta_post[,1] + 
      mean(dat[,lpred][[1]])*brm_beta_post[,lcol] +
      brm_beta_post[,rcol]
  )*14)-7
  
  # 1b
  pred_rating_diff_R <- pred_rating_yes_R - pred_rating_no_R
  
  # 2a - no L
  pred_rating_no_L <- (plogis(
    brm_beta_post[,1] + 
      mean(dat[,rpred][[1]])*brm_beta_post[,rcol]
  )*14)-7
  
  # 2a - yes L
  pred_rating_yes_L <- (plogis(
    brm_beta_post[,1] + 
      mean(dat[,rpred][[1]])*brm_beta_post[,rcol] +
      brm_beta_post[,lcol]
  )*14)-7
  
  # 2b
  pred_rating_diff_L <- pred_rating_yes_L - pred_rating_no_L
  
  if (printPlease) {
    txt <- paste0(
      "predicted roughness rating based on R:\n",
      "   without R: ", round(mean(pred_rating_no_R), 2), 
      " [", round(quantile(pred_rating_no_R, 0.025), 2), 
      ",", round(quantile(pred_rating_no_R, 0.975), 2), "]\n",
      "   with R: ", round(mean(pred_rating_yes_R), 2), 
      " [", round(quantile(pred_rating_yes_R, 0.025), 2), 
      ",", round(quantile(pred_rating_yes_R, 0.975), 2), "]\n",
      "predicted roughness difference (R - no R):\n",
      "   diff: ", round(mean(pred_rating_diff_R), 2), 
      " [", round(quantile(pred_rating_diff_R, 0.025), 2), 
      ",", round(quantile(pred_rating_diff_R, 0.975), 2), "]\n\n",
      "predicted roughness rating based on L:\n",
      "   without L: ", round(mean(pred_rating_no_L), 2), 
      " [", round(quantile(pred_rating_no_L, 0.025), 2), 
      ",", round(quantile(pred_rating_no_L, 0.975), 2), "]\n",
      "   with L: ", round(mean(pred_rating_yes_L), 2), 
      " [", round(quantile(pred_rating_yes_L, 0.025), 2), 
      ",", round(quantile(pred_rating_yes_L, 0.975), 2), "]\n",
      "predicted roughness difference (L - no L):\n",
      "   diff: ", round(mean(pred_rating_diff_L), 2), 
      " [", round(quantile(pred_rating_diff_L, 0.025), 2), 
      ",", round(quantile(pred_rating_diff_L, 0.975), 2), "]\n",
      sep="")
    cat(txt)
  }
  return(tibble(pred_rating_no_R, pred_rating_yes_R, pred_rating_diff_R, 
                pred_rating_no_L, pred_rating_yes_L, pred_rating_diff_L))
}


logistic_summary <- function (model, dat, outcome="/r/", roughpred="rough", binary_pred=T, printPlease=T) {
  brm_beta_post <- posterior_samples(model)
  roughcol <- paste0("b_", roughpred)
  if (binary_pred) {
    roughcol <- paste0(roughcol, "TRUE") 
  }
  
  # smooth
  pred_prob_smooth <- plogis(
    brm_beta_post[,1]
  )
  
  # rough
  pred_prob_rough <- plogis(
    brm_beta_post[,1] +
    brm_beta_post[,roughcol]
  )
  
  # diff
  pred_prob_diff <- pred_prob_rough - pred_prob_smooth
  
  if (printPlease) {
    txt <- paste0(
      "predicted probability of ", outcome, " rating based on roughness:\n",
      "   smooth: ", round(mean(pred_prob_smooth), 2), 
      " [", round(quantile(pred_prob_smooth, 0.025), 2), 
      ",", round(quantile(pred_prob_smooth, 0.975), 2), "]\n",
      "   rough: ", round(mean(pred_prob_rough), 2), 
      " [", round(quantile(pred_prob_rough, 0.025), 2), 
      ",", round(quantile(pred_prob_rough, 0.975), 2), "]\n",
      "predicted difference in probability of ", outcome, " (rough - smooth):\n",
      "   diff: ", round(mean(pred_prob_diff), 2), 
      " [", round(quantile(pred_prob_diff, 0.025), 2), 
      ",", round(quantile(pred_prob_diff, 0.975), 2), "]\n\n",
      sep="")
    cat(txt)
  }
  return(tibble(pred_prob_smooth, pred_prob_rough, pred_prob_diff))
}

plot_varimp <- function(varimps,
                        show_n = length(varimps), xaxis) {
  varimps <- rev(varimps[seq(length(varimps), length(varimps) - show_n, -1)])
  
  plot(1, 1, type = 'n', xlab = '', ylab = '',
       xaxt = 'n', yaxt = 'n', bty = 'n',
       xlim = range(xaxis), ylim = c(0, show_n + 1))
  mtext(side = 1, text = 'Relative Variable Importance',
        line = 4, font = 2, cex = 2)
  mtext(side = 1, text = '(permutation based)',
        line = 5.7, font = 2, cex = 1.5)
  abline(h = seq_along(varimps), lty = 2, col = 'darkgrey')
  axis(side = 1, at = xaxis, lwd = 2, cex.axis = 1.5, font = 2)
  axis(side = 2, at = seq_along(varimps), labels = names(varimps),
       lwd = 2, cex.axis = 1.25, font = 2, las = 2)
  points(x = varimps, y = seq_along(varimps), pch = 15, cex = 1.5)
  box(lwd = 2)
  abline(v = 0)
}





