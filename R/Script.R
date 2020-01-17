####from script to function####
#Set working directory or make project
#setwd("~/Desktop/R_XRF/R_XRF_comproject/XRF package")
#Issues;
#Is it ok to load the packages used inside the function?
#Names of important files must be the same! (can I refer to it in function insted? better?)
#- in each project you have to make a folder where the files have the same names..best solution?
#Files; XRFdata.TXT, Infofile.xlsx, xrf_setup.xlsx
# Should pivot wider be in function? (where should the code end?)
#Extra functions must be after..
#Drift; the date should be possible to choose in my package! - and people can include in the infofile when needed!

#' import xrf data
#'
#' @param datapath name of my XRFdata.TXT file
#' @param infopath name of my Infofile.xlsx file
#' @param setuppath name of xrf_setup.xlsx file
#' @param year yeat the drift was measured
#' @importFrom readr read_delim locale
#' @importFrom dplyr %>% select starts_with rename_all
#' @importFrom dplyr inner_join anti_join filter group_by summarise mutate left_join
#' @importFrom tidyr pivot_longer
#' @importFrom readxl read_excel
#' @importFrom stringr str_remove
#' @importFrom rlang .data
#' @examples
#' \dontrun {
#'   read_XRF(datapath = "data/Test.TXT",  infopath = "data/Infofile.xlsx",
#'            setuppath = "data/xrf_setup.xlsx", year = "2019")
#' }
#' @export

#My function;
read_XRF <- function(datapath, infopath, setuppath, year){
  datafile.df <- read_delim(datapath, delim = '\t', locale = locale(decimal_mark = ","))
  datafile.df <- datafile.df %>%
    select(-(3:18)) %>%
    select(-starts_with("X")) %>%
    rename_all(str_remove, pattern = " .*")

  infofile.df <- read_excel(infopath)
  projectfile.df <-  inner_join(datafile.df, infofile.df, by = 'Sample')
  notinprojectfile.df <-  anti_join(datafile.df, infofile.df, by = 'Sample')
  if(nrow(notinprojectfile.df)>0){
    warning('WARNING !!! something did not match between your datasets')
  }
  notinprojectfile.df

  pivotproject.df <- projectfile.df %>%
    pivot_longer(.data$C:.data$As,
                 names_to = 'Element',
                 values_to = 'Value')
  mean.blanks.df <- pivotproject.df %>%
    filter(Filter == 'blank') %>%
    group_by(.data$Box, .data$Element) %>% #Box = Infofile Variable
    summarise(mean_blank = mean(.data$Value))
  adjustedforbl.df <- left_join(pivotproject.df, mean.blanks.df, by = c('Box', 'Element')) %>%
    mutate(net_counts = .data$Value - .data$mean_blank)

  setupfile.df <- read_excel(setuppath)
  pivotsetup.df <- setupfile.df %>%
    pivot_longer(.data$PC:.data$GFF,
                 names_to = 'Filter',
                 values_to = 'Cal.cons')
  detectionlimits.df <- setupfile.df %>%
    select(.data$DL_PC:.data$DL_GFF, .data$Element) %>%
    pivot_longer(.data$DL_PC:.data$DL_GFF,
                 names_to = 'Filter',
                 values_to = 'Detection.lim') %>%
    mutate(Filter = str_remove(Filter, 'DL_'))
  join.df <- left_join(adjustedforbl.df, pivotsetup.df, by = c('Filter', 'Element'))

  calculations.df <- join.df %>%
    mutate(Value = (.data$net_counts*.data$Cal.cons) * 9.078935 * (1000 / .data$Volume) / .data$MolarW * 1000 * (.data$Drift_2008/.data[[paste0("Drift_", year)]]))

  Project.df <- calculations.df %>%
    select(.data$Sample:.data$Element, .data$Value)

  return(Project.df)
} #FIX errors!

#Lisence; MIT or GPL3

# test.df <- XRFmydata('datafile.df')
# write_csv(test.df, 'test.csv')
#
#
# #OOOBSSSS
# #######Import
# datafile.df <- read_delim("Test.TXT", delim = "\t", locale = locale(decimal_mark = ","))
# datafile2.df <- read_delim("Test2.TXT", delim = "\t", locale = locale(decimal_mark = ","))
# #OBS: The C is imported differentyl!
# datafile.df <- datafile.df %>%
#   select(-('C':'As (PPM)')) %>%
#   select(-starts_with("X")) %>%
#   rename_all(str_remove, pattern = " .*")
# #OR:
# datafile2.df <- datafile2.df %>%
#   select(-('C (%)':'As (PPM)')) %>%
#   select(-starts_with("X")) %>%
#   rename_all(str_remove, pattern = " .*")
# #solution;
# datafile.df <- datafile.df %>%
#   select(-(3:18)) %>%
#   select(-starts_with("X")) %>%
#   rename_all(str_remove, pattern = " .*")
#
#
#
#
