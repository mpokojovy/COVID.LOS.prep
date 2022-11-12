####################################################################################################
# Copyright (C) 2022 Michael Pokojovy                                                              #
#                                                                                                  #
# Data preparation for COVID-19 patient hospital length-of-stay (LOS) analysis reported in:        #
#                                                                                                  #
# Y. Wen, M.F. Rahman, Y. Zhuang, M. Pokojovy et al (2022)                                         #
# Time-to-event modeling for hospital length of stay prediction for COVID-19 patients              #
# Machine Learning with Applications. Volume 9, 15 September 2022, 100365                          #
# https://www.sciencedirect.com/science/article/pii/S2666827022000603?via%3Dihub                   #
#                                                                                                  #
####################################################################################################

str2days <- function(s) {
  if (s == "")
    return(NA)
  else {
    pos.days = as.integer(regexpr("days", s))
    pos.col  = as.integer(regexpr(":", s))
    
    days = as.numeric(substr(s, 1L, pos.days - 2L))
    hrs  = as.numeric(substr(s, pos.days + 5L, pos.col - 1L))
    mins = as.numeric(substr(s, pos.col + 1L, pos.col + 2L))
    
    return(days + hrs/24.0 + mins/(24.0*60.0))
  }
}