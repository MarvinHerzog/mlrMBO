# Random search, where we shrink the region of interest after restarts
# around the currently best point. only numeric / ints are currently "shrunken"
# works for ALL parameter sets
#
#FIXME it would be nice to have a REASONABLE way to shrink categorical stuff too.
#FIXME should we shrink if a local value is NA (dependent param)
#
# See infillOptCMAES.R for interface explanation.
infillOptFocus = function (infill.crit, models, control, par.set, opt.path, designs, 
          iter, ...) 
{
  global.y = Inf
  for (restart.iter in seq_len(control$infill.opt.restarts)) {
    ps.local = par.set
    for (local.iter in seq_len(control$infill.opt.focussearch.maxit)) {
      newdesign = generateDesign(control$infill.opt.focussearch.points, 
                                 ps.local, randomLHS)
      newdesign = convertDataFrameCols(newdesign, ints.as.num = TRUE, 
                                       logicals.as.factor = TRUE)
      if(length(designs)>1){
        browser()
      } else {
        design_size_1 = nrow(newdesign)
        newdesign_alt = dplyr::anti_join(newdesign, designs[[1]] %>% select(-y),by = colnames(newdesign))
        if(nrow(newdesign_alt)<design_size_1){
          #cat0("Duplicates removed from design: ",design_size_1,"->",nrow(newdesign_alt))
          if(nrow(newdesign_alt)==0){
            cat0("No non duplicates... using default design")
          } else {
            newdesign = newdesign_alt
          }
        }
      }
      

      y = infill.crit(newdesign, models, control, ps.local, 
                      designs, iter, ...)
      local.index = getMinIndex(y, ties.method = "random")
      local.y = y[local.index]
      local.x.df = newdesign[local.index, , drop = FALSE]
      local.x.list = dfRowToList(recodeTypes(local.x.df, 
                                             ps.local), ps.local, 1)
      if (local.y < global.y) {
        global.x.df = local.x.df
        global.y = local.y
      }
      ps.local$pars = lapply(ps.local$pars, function(par) {
        val = local.x.list[[par$id]]
        if (!isScalarNA(val)) {
          if (isNumeric(par)) {
            range = par$upper - par$lower
            par$lower = pmax(par$lower, val - (range/4))
            par$upper = pmin(par$upper, val + (range/4))
            if (isInteger(par)) {
              par$lower = floor(par$lower)
              par$upper = ceiling(par$upper)
            }
          }
          else if (isDiscrete(par)) {
            if (length(par$values) > 1L) {
              val.names = names(par$values)
              to.del = sample(which(val.names != val), 
                              1L)
              par$values = par$values[-to.del]
            }
          }
        }
        return(par)
      })
    }
  }
  recodeTypes(global.x.df, par.set)
}
