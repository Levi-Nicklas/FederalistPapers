# Obtain and Reformat Data
# Levi C. Nicklas
# October 10th, 2020
#
# Notes:  This data is going to serve as the topic of my midterm "mini-project"
#         for my data visualization class. Additionally, this will be a trial
#         run for some methods I will use for my thesis. Practicing at reformating
#         the data will be very important; this will be a good exercise.
#


# Use {gutenbergr} to download the _Federalist Papers_.
library(gutenbergr)
library(tidyverse)

# Federalist Papers are Book 1404 on Project Gutenberg.
# Check it out here. https://www.gutenberg.org/ebooks/1404
federalist <- gutenberg_download(c(1404))


federalist <- federalist %>% 
  # get rid of white space.
  filter(text != "") %>% 
  # find authors.
  mutate(author_flag = 
           ifelse((text == "HAMILTON" | text == "JAY" | text == "MADISON"),TRUE,FALSE))

  
# Grab just a Single Paper!
get_paper <- function(paper_num){
  # My code uses the next paper's title to find the text...
  # so for the last paper, 85, that doesnt work.
  if(paper_num == 85){
    paper_start <- paste0("FEDERALIST No. ", paper_num)
    
    temp_fed <- federalist %>% 
      mutate(id = 1:nrow(federalist)) %>% 
      mutate(header_flag = 
               ifelse(text == paper_start,TRUE,FALSE))
    
    header_locs <- temp_fed %>% 
      filter(header_flag == TRUE) %>% 
      pull(id)
    
    temp_fed %>% 
      filter(id > header_locs[[1]])
    
    #OTHERWISE!
  }else{
    # Define Paper Titles.
    paper_start <- paste0("FEDERALIST No. ", paper_num)
    paper_end <- paste0("FEDERALIST No. ", paper_num+1)
  
    # Find Paper Titles.
    temp_fed <- federalist %>% 
      mutate(id = 1:nrow(federalist)) %>% 
      mutate(header_flag = 
              ifelse((text == paper_start | text == paper_end),TRUE,FALSE))
    
    header_locs <- temp_fed %>% 
      filter(header_flag == TRUE) %>% 
      pull(id)
  
    # Get the text between headers.
    temp_fed %>% 
      filter(id > header_locs[[1]] & id < header_locs[[2]]-1)
  }
}


# Allocate space for the data
tidy_federalist <- data.frame(paper_num = 1:85,
           text = rep(0,85),
           author = rep(0,85))

for(i in 1:85){
  print(paste("On Paper:",i))
  tmp_paper <- get_paper(i)
  
  # find author.
  tmp_author <- tmp_paper %>% 
    filter(author_flag == TRUE) %>% 
    pull(text)
  
  # Handle Co-Author Cases.
  if(is_empty(tmp_author)){
    tmp_author <- "MULTIPLE"
  }
  
  # build text.
  tmp_text <- paste(tmp_paper$text, collapse = " ")
  
  # put it together.
  tidy_federalist$text[i] <- tmp_text
  tidy_federalist$author[i] <- tmp_author
}
  

# Write Data
write_rds(tidy_federalist, "../data/tidy_federalist.RDS")

