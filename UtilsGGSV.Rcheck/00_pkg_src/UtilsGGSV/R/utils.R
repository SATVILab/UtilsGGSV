# In R/utils.R or a similar file in your package
# declare these variables as global
# as they are column names in dataframes manipulated
# by dplyr
utils::globalVariables(c(
  'x', 'y', 'grp_y', '.id', '.grp', 'grp_x',
  'txt', 'lb', 'ub', 'est', 'data', 'pval', 'g2',
  'g1', '.y'
  ))
