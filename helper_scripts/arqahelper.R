# title: "Helper functions for discrete RQA with crqa"
# author: "Dr. Erin N. McCormick, Dr. Leslie M. Blaha"
# date: "July 26, 2021"


# LICENSE ##############################
#
# Helper functions for conducting discrete recurrence quantification analysis with the crqa package.
# Copyright (C) 2021 Erin N. McCormick and Leslie M. Blaha
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# README ##############################
#
# This file has six functions to help conduct auto- and cross-recurrence 
# quantification analysis on discrete data using the "crqa" R package:
# 
# 1) arqa.map
# 2) crqa.map
# 3) arqa.tidy.recurrencematrix
# 4) crqa.tidy.recurrencematrix
# 5) calc.arqa.stats
# 6) tidy.recurrencematrices
# 
# Please see AutoRQA_ExampleCode.RMD to see a complete demonstration of these functions.
#

# Required Packages  ##############################
#
# The crqa and tidyverse packages are currently required. Uncomment the following two lines,
# or call these packages in the document that you call the arqahelper.R code.

# require(crqa)
# require(tidyverse) # dplyr, purrr, magrittr, ggplot2, etc. ...



# arqa.map  ##############################
# 
# A wrapper function to simplify setting the additional arguments for crqa()
#
# Future changes:
# *  Create named list to pass through calc.arqa.stats() to allow for adjusting parameters at very start point, rather than changing the function itself

arqa.map <- function(seq) {
  crqa(ts1 = seq, ts2 = seq, delay = 1, embed = 1, rescale = 0, 
       radius = 0.0001, normalize = 0, mindiagline = 2, minvertline = 2,
       tw = 1) 
  # setting tw to 1 removes Line of Identity/Incidence for auto-recurrence analysis
}

# crqa.map  ##############################
# 
# A wrapper function to simplify setting the additional arguments for crqa()
#
# Future changes:
# *  Create named list to pass through calc.arqa.stats() to allow for adjusting parameters at very start point, rather than changing the function itself

crqa.map <- function(seq1, seq2) {
  crqa(ts1 = seq1, ts2 = seq2, delay = 1, embed = 1, rescale = 0, 
       radius = 0.0001, normalize = 0, mindiagline = 2, minvertline = 2)
}


# arqa.tidy.recurrencematrices ##############################
# 
# Creates dataframe with recurrence points for each sequence, for all sequences provided. Points are labeled both as recurrence, and with the recurrent event state. 
# 
# Notes: 
# *  In order to (slightly) reduce object size in memory, only recurrent points are present in the dataframe (non-recurrent points are missing). 
# *  If parameter remove_loi is TRUE then line of incidence/identity is removed.
#

arqa.tidy.recurrencematrix <- function(event.sequence, seqkey, remove_loi=TRUE, ticks=NA) {
  
  # for each element in the sequence of events, identify which() other trials are recurrent/have the same event state
  # slight adjustments for if ticks labels are provided to the function
  if (is.na(ticks)) {
    lapply(c(1:length(event.sequence)), FUN = function(curr.element) {
      data.frame(seqkey = seqkey, 
                 x = curr.element, 
                 y = which(event.sequence == event.sequence[curr.element]), 
                 recurrent = 1, 
                 event.state = event.sequence[curr.element],
                 stringsAsFactors = FALSE)
    }) -> test.elements
  } else {
    lapply(c(1:length(event.sequence)), FUN = function(curr.element) {
      data.frame(seqkey = seqkey, 
                 x = curr.element, 
                 y = which(event.sequence == event.sequence[curr.element]), 
                 recurrent = 1, 
                 event.state = event.sequence[curr.element],
                 x.ticks = ticks[x],
                 y.ticks = ticks[y],
                 stringsAsFactors = FALSE)
    }) -> test.elements
  }
  
  # combine all the sections into one data.frame
  results <- Reduce(function(df1, df2) rbind(df1, df2), test.elements)
  
  # remove the Line of Incidence/Identity if parameter is true
  if (remove_loi) {
    results <- results[which(!(results$x == results$y)),]
  }
  
  # need to add "ticks" option back in
  # ...
  
  return(results)
}

# crqa.tidy.recurrencematrices ##############################
# 
# Creates dataframe with recurrence points for each sequence, for all sequences provided. Points are labeled both as recurrence, and with the recurrent event state. 
# 
# Notes: 
# *  In order to (slightly) reduce object size in memory, only recurrent points are present in the dataframe (non-recurrent points are missing). 
# *  If parameter remove_loi is TRUE then line of incidence/identity is removed.
#

crqa.tidy.recurrencematrix <- function(event.sequence1, event.sequence2, seqkey, remove_loi=FALSE, ticks=NA) {
  
  # for each element in the sequence of events, identify which() other trials are recurrent/have the same event state
  # slight adjustments for if ticks labels are provided to the function
  if (is.na(ticks)) {
    lapply(c(1:length(event.sequence1)), FUN = function(curr.element) {
      if (length(which(event.sequence1[curr.element] == event.sequence2)) > 0) {
        data.frame(seqkey = seqkey, 
                   x = curr.element, 
                   y = which(event.sequence1[curr.element] == event.sequence2), 
                   recurrent = 1, 
                   event.state = event.sequence1[curr.element],
                   stringsAsFactors = FALSE)
      }
    }) -> test.elements
  } else {
    lapply(c(1:length(event.sequence1)), FUN = function(curr.element) {
      if (length(which(event.sequence1[curr.element] == event.sequence2)) > 0) {
        data.frame(seqkey = seqkey, 
                   x = curr.element, 
                   y = which(event.sequence1[curr.element] == event.sequence2), 
                   recurrent = 1, 
                   event.state = event.sequence1[curr.element],
                   x.ticks = ticks[x],
                   y.ticks = ticks[y],
                   stringsAsFactors = FALSE)
      }
    }) -> test.elements
  }
  
  # combine all the sections into one data.frame
  results <- Reduce(function(df1, df2) rbind(df1, df2), test.elements)
  
  # remove the Line of Incidence/Identity if parameter is true
  if (remove_loi) {
    results <- results[which(!(results$x == results$y)),]
  }
  
  return(results)
}


# calc.arqa.stats ##############################
# 
# Steps to pull individual event sequences, calculate the crqa stats and recurrence plot matrices.
# 
# Notes: 
# *  Example arguments values include: seqkey.var = "participantid", seqorder.var = "trial", seqevents.var = "choice"
#
# Future changes:
# *  rewrite code to use base R instead of the tidyverse

calc.arqa.stats <- function(thedf, seqkey.var, seqorder.var, seqevents.var) {
  
  thedf %>%
    group_by(!!as.name(seqkey.var)) %>%
    arrange(!!as.name(seqorder.var)) %>%
    summarise(event.sequence = list(!!sym(seqevents.var))) %>%
    ungroup() %>%
    mutate(crqa.object = purrr::map(event.sequence, arqa.map),
           crqa.RR = unlist(purrr::map(crqa.object, function(x) {x$RR})),
           crqa.DET = unlist(purrr::map(crqa.object, function(x) {x$DET})),
           crqa.NRLINE = unlist(purrr::map(crqa.object, function(x) {x$NRLINE})),
           crqa.maxL = unlist(purrr::map(crqa.object, function(x) {x$maxL})),
           crqa.L = unlist(purrr::map(crqa.object, function(x) {x$L})),
           crqa.ENTR = unlist(purrr::map(crqa.object, function(x) {x$ENTR})),
           crqa.rENTR = unlist(purrr::map(crqa.object, function(x) {x$rENTR})),
           crqa.LAM = unlist(purrr::map(crqa.object, function(x) {x$LAM})),
           crqa.TT = unlist(purrr::map(crqa.object, function(x) {x$TT})),
           crqa.RP = purrr::map(crqa.object, function(x) {x$RP}),
           crqa.RP.long = purrr::pmap(list(event.sequence, !!as.name(seqkey.var), remove_loi=TRUE), arqa.tidy.recurrencematrix)
    ) -> results
  
  return(results)
}


# calc.crqa.stats ##############################
# 
# Steps to pull individual event sequences, calculate the crqa stats and recurrence plot matrices.
# 
# Notes: 
# *  Example arguments values include: seqkey1.var = "participantid", seqorder1.var = "trial", seqevents1.var = "choice"
#    seqkey2.var = "participantid"", seqorder1.var = "trial", seqevents1.var = "optimalchoice"
#
# Future changes:
# *  rewrite code to use base R instead of the tidyverse

calc.crqa.stats <- function(thedf, seqkey.var, seqorder1.var, seqevents1.var,
                            seqorder2.var=NA, seqevents2.var) {

  thedf %>%
    group_by(!!as.name(seqkey.var)) %>%
    arrange(!!as.name(seqorder1.var)) %>%
    summarise(event.sequence1 = list(!!sym(seqevents1.var))) %>%
    ungroup() -> interim.df
  
  # if multiple sequences are going to be compared to one sequence, then seqevents2.var can equal a vector of 
  #   event states rather than a variable/column name
  
  if (is.character(seqevents2.var) & length(seqevents2.var) == 1) {
      
    if(is.na(seqorder2.var[1])) {
      thedf %>%
        group_by(!!as.name(seqkey.var)) %>%
        arrange(!!as.name(seqorder1.var)) %>%
        summarise(event.sequence2 = list(!!sym(seqevents2.var))) %>%
        ungroup() -> seq2.df
    } else {
      thedf %>%
        group_by(!!as.name(seqkey.var)) %>%
        arrange(!!as.name(seqorder2.var)) %>%
        summarise(event.sequence2 = list(!!sym(seqevents2.var))) %>%
        ungroup() -> seq2.df
    }
   
    interim.df %>%
      left_join(seq2.df) -> interim.df
  } else if (is.vector(seqevents2.var) & is.numeric(seqevents2.var[1])) {
    
    thedf %>%
      group_by(!!as.name(seqkey.var)) %>%
      arrange(!!as.name(seqorder1.var)) %>%
      summarise(event.sequence1 = list(!!sym(seqevents1.var)),
                event.sequence2 = list(seqevents2.var)) %>%
      ungroup() -> interim.df
    
  } else {
    print("ERROR: Sequence 2 in unexpected form (not a single character element or a vector of numeric elements).")
  }

  interim.df %>% # View()
    mutate(crqa.object = purrr::pmap(list(event.sequence1,event.sequence2), crqa.map),
           crqa.RR = unlist(purrr::map(crqa.object, function(x) {x$RR})),
           crqa.DET = unlist(purrr::map(crqa.object, function(x) {x$DET})),
           crqa.NRLINE = unlist(purrr::map(crqa.object, function(x) {x$NRLINE})),
           crqa.maxL = unlist(purrr::map(crqa.object, function(x) {x$maxL})),
           crqa.L = unlist(purrr::map(crqa.object, function(x) {x$L})),
           crqa.ENTR = unlist(purrr::map(crqa.object, function(x) {x$ENTR})),
           crqa.rENTR = unlist(purrr::map(crqa.object, function(x) {x$rENTR})),
           crqa.LAM = unlist(purrr::map(crqa.object, function(x) {x$LAM})),
           crqa.TT = unlist(purrr::map(crqa.object, function(x) {x$TT})),
           crqa.RP = purrr::map(crqa.object, function(x) {x$RP}),
           crqa.RP.long = purrr::pmap(list(event.sequence1, event.sequence2, !!as.name(seqkey.var), remove_loi=FALSE), crqa.tidy.recurrencematrix)
    ) -> results
  
  return(results)
}

# tidy.recurrence.matrices ##############################
# 
# Pulls out tidy format of recurrence matrices, binds them all together (across values of the sequence key), and adds additional information as directed by the arguments. 
# 
# Notes: 
# *  rqa.stats = TRUE  will add the relevant RQA stats (from recurrent rate to trapping time) as columns to the tidy dataset. (in case having in the tidy dataframe object is easier to work with)
# *  Every variable in key.info.df will be joined.
#
# Future changes:
# *  ...

tidy.recurrence.matrices <- function(rqa.results, seqkey.var, rqa.stats = FALSE, key.info.df=NA, key.info.var=NA) {
  
  rqa.results %>% 
    pull(crqa.RP.long) %>%
    bind_rows() -> base.tidydf
  
  # add extra information to the recurrence matrices depending on function arguments
  # (creates a larger object, but available in case it's useful to include with the recurrence points, rather than use related datasets)
  if (rqa.stats & is.na(key.info.var)) {
    
    base.tidydf %>%
      left_join(rqa.results %>% 
                  select(!!as.name(seqkey.var), crqa.RR:crqa.TT),
                by = c("seqkey" = seqkey.var) ) -> results.tidydf
    
  } else if (rqa.stats & !is.na(key.info.var)) {
    
    base.tidydf %>%
      left_join(rqa.results %>% 
                  select(!!as.name(seqkey.var), crqa.RR:crqa.TT),
                by = c("seqkey" = seqkey.var) ) %>% 
      left_join(key.info.df,
                by = c("seqkey" = key.info.var)
      ) -> results.tidydf
    
  } else if (rqa.stats==FALSE & is.na(key.info.var)) {
    
    base.tidydf -> results.tidydf
    
  } else if (rqa.stats==FALSE & !is.na(key.info.var)) {
    
    base.tidydf %>% 
      left_join(key.info.df,
                by = c("seqkey" = key.info.var)
      ) -> results.tidydf
    
  } else {
    
    print("ERROR: Issue with conditional for rqa.stats and key.info parameters.")
    NA -> results.tidydf
    
  }
  
  results.tidydf %>% 
    rename(!!seqkey.var := seqkey) -> results.tidydf
  
  return(results.tidydf)
}


# Example use of all functions ##############################

# set.seed(2020)
#
# example.df <- data.frame(participantid = rep(c("A","B","C"), each = 20),
#                          trial = rep(c(1:20), times = 3),
#                          choice = sample(c(1,2), 60, replace = TRUE),
#                          stringsAsFactors = FALSE)
# 
# example.df
# 
# example.df %>%
#   calc.arqa.stats(seqkey.var = "participantid", seqorder.var = "trial", seqevents.var = "choice") -> temp.results
# 
# tidy.recurrence.matrices(rqa.results=temp.results, seqkey.var = "participantid", rqa.stats = TRUE, key.info.df=data.frame(testvar = c("A","B","C"), condition = c(1,1,2), covariate = c(6,10,25)), key.info.var="testvar") -> results.tidydf
