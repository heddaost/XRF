#Import and check datapath
read_datapath_XRF <- function(datapath){
datafile.df <- read_delim(datapath, delim = '\t', locale = locale(decimal_mark = ","))
datafile.df <- datafile.df %>%
  select(-(3:18)) %>%
  select(-starts_with("X")) %>%
  rename_all(str_remove, pattern = " .*")
return(datafile.df)
}

#Import and check infopath
#' @importFrom assertr verify has_all_names
read_infopath_XRF <- function(infopath){
  infofile.df <- read_excel(infopath) %>%
  verify(has_all_names("Filter_box_nr", "Filter_type", "Filter_size", "Filter_blank"))
  return(infofile.df)
}




