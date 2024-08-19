# library(INLA)
# library(lubridate)

forecasting.inla <- function(dados,   # dados - Data containing columns (cases, date, target)
                             #Year2forecast = 2023,
                             MC = FALSE, M = 2000,
                             # Changing the first week and redefine year as seasons
                             start = 41, 
                             # make week 53 as week 52 (POG!)
                             week53 = T,
                             quantiles = c(0.05, 0.25, 0.5, 0.75, 0.9, 0.95),
                             output.only = F,
                             likelihood = "nbinomial",
                             timeRE = "rw2",
                             cyclic = T,
                             WAIC = F){
  
  data.inla <- dados %>% ungroup() %>% 
    transmute(
      #Pop = Pop,
      Date = Date,
      week = week.season(Date, start = start),
      # week = ifelse(week == 53, 52, week),
      year = season(Date, start = start),
      cases = cases,
      target = target
    )  
  
  
  formula.q <- cases ~ 1 +
    f(week, model = timeRE, constr = T, cyclic = cyclic,
      hyper = list(
        # Precision of unstructure random effects
        prec = list(
          prior="pc.prec",
          param=c(3, 0.01)
        )
      )
    ) + 
    f(year, model = "iid", constr = T,
      hyper = list(
        # Precision of unstructure random effects
        prec = list(
          prior="pc.prec",
          param=c(3, 0.01)
        )
      )
    )
  
  # # Adding forecasting component
  # data.inla <- data.inla %>% 
  #   add_row(week = 1:52, Year = Year2forecast) %>% 
  #   mutate(year = Year - min(Year) + 1)
  
  linear.term.year.cur <- which(data.inla$target == T)
  
  
  output.mean <- inla(formula = formula.q, 
                      data = data.inla %>% 
                        mutate(
                          cases = ifelse(target==F, cases, NA)
                        ),
                      control.predictor = list(link = 1, compute = T, 
                                               quantiles = quantiles),
                      family = likelihood, 
                      # offset = log(Pop / 1e5),
                      # control.family = list(
                      #   control.link = list(
                      #     model = "quantile",
                      #     quantile = 0.5
                      #     )
                      #   ),
                      # control.fixed = control.fixed(prec.intercept = 1),
                      control.compute = list(config = MC, waic = WAIC)
  )
  
  out = NULL
  
  out$inla = output.mean
  
  out$pred = data.inla %>% filter(target == T) %>% 
    bind_cols( output.mean$summary.fitted.values[linear.term.year.cur,] )
  
  if(MC){
    param.samples <- inla.posterior.sample(output.mean, n = M)
    
    samples.MC <- param.samples %>%
      map(.f = function(xxx, idx = linear.term.year.cur){
        rnbinom(
          n = idx, 
          mu = exp(xxx$latent[idx]), 
          size = xxx$hyperpar[1]
        )} ) 
    
    names(samples.MC) <- 1:M
    
    samples.MC <- samples.MC %>%   
      bind_rows(.id = "samples") %>% 
      rowid_to_column(var = "week") %>% 
      gather(key = "samples", value = "values", -week) 
    
    
    out$MC = samples.MC
  }
  
  out 
}



threshold.MC <- function(samples.MC){
  thresholdw.data <- samples.MC %>%  
    group_by(week) %>% 
    summarise(
      Q1 = quantile(probs = 0.25, values),
      Q2 = quantile(probs = 0.50, values),
      Q3 = quantile(probs = 0.75, values),
      P90 = quantile(probs = 0.9, values),
    )
  thresholdw.data
} 



season <- function(x, start = 41){
  if(!is.Date(x)) stop("Not Date format")
  
  ew = epiweek(x)
  ey = epiyear(x)
  
  ifelse(ew < start, paste(ey-1, ey, sep = "-"), paste(ey, ey+1, sep = "-"))
}

# season(today())

week.season  <- function(x, start = 41, week53 = T){
  if(!is.Date(x)) stop("Not Date format")
  
  ew = epiweek(x)
  
  if(week53 & any(ew==53)) ew[ew==53] = 52
  
  # 1 = start, 2 = start + 1, 3 = start + 2,...
  ifelse(ew >= start, ew - start + 1, 52 - start + 1 + ew)
}
