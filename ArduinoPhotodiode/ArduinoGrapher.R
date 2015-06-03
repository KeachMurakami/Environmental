# path to the output files 
ArdOutput <-
  "~/Python34/ArduinoPFD.csv"


# Future prospect: exe python from R

# library(rPython)
# pass the smoothing to python from R
# execute ArduinoPython.py via Python


Light <- "FR"
Ard_graph(Light.Col = Light)

Ard_graph <- function(Light.Col){
  slope <-
    switch(Light.Col, B =  0.31703, R = 0.28209, FR = 0.286312)
  intercept <-
    switch(Light.Col, B =  4.95123, R = -1.34458, FR = -0.479106)
  
  
  volt <-
    read.csv(ArdOutput, sep = ":", header = FALSE) %>%
    .[-1]
  
  Ard <-
    volt %>% t %>% data.frame %>%
      head(dim(volt)[2] - 1) %>%
      mutate(raw = as.numeric(as.character(.)),
             calibrated = raw * slope + intercept,
             time = 1:(dim(volt)[2]-1))
  
  Meas.Time <- 
    file.info(ArdOutput) %>%
    select(mtime) %>%
    format("%Y/%m/%d %H:%M:%S") %>%
    .[1,1] %>%
    paste0("@", .)
  
  aveSD <-
    paste0("ave: ", sprintf("%.1f", mean2(Ard$calibrated)),
           ", SD: ", sprintf("%.2f", sd(Ard$calibrated)))
  
  Ymax <- max(ceiling(Ard$calibrated/10) * 10)
  Ymin <- min(floor(Ard$calibrated/10) * 10)
  
  
  Fig_PFD <-
    Ard %>%
    ggplot(aes(x = time, y = calibrated)) +
    geom_line() +
    geom_point() +
    ylim(c(Ymin, Ymax)) +
    annotate("text", label = Meas.Time,
             x = 0, y = Ymin, hjust = 0, vjust = 0, size = 10) +
    annotate("text", label = aveSD,
             x = 0, y = Ymax, hjust = 0, vjust = 1, size = 10)

    
  list(Stats = aveSD, TimeCourse = Fig_PFD) %>%
    return
}