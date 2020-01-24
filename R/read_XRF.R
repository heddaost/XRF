#' import xrf data
#'
#' @param datapath name of my XRFdata.TXT file
#' @param infopath name of my Infofile.xlsx file
#' @param setuppath name of xrf_setup.xlsx file
#' @param year year the drift was measured
#' @importFrom readr read_delim locale
#' @importFrom dplyr %>% select starts_with rename_all
#' @importFrom dplyr inner_join anti_join filter group_by summarise mutate left_join
#' @importFrom tidyr pivot_longer
#' @importFrom readxl read_excel
#' @importFrom stringr str_remove
#' @importFrom rlang .data
#' @examples
#' datadir <- system.file("extdata", package = "XRF")
#'   read_XRF(datapath = file.path(datadir, "xrf_rawdata.TXT"),
#'            infopath = file.path(datadir,"project_info.xlsx"),
#'            setuppath = file.path(datadir,"xrf_setup.xlsx"),
#'            year = "2019")
#' @export

#My function;
read_XRF <- function(datapath, infopath, setuppath, year){
  area_filter <- 9.078935

  #read datafile
  datafile.df <- read_datapath_XRF(datapath = datapath)

  #read infofile
  infofile.df <- read_infopath_XRF(infopath = infopath)

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
    group_by(.data$Box, .data$Element) %>%
    summarise(mean_blank = mean(.data$Value))
  adjustedforbl.df <- left_join(pivotproject.df, mean.blanks.df, by = c('Box', 'Element')) %>%
    mutate(net_counts = .data$Value - .data$mean_blank)

  setupfile.df <- read_excel(setuppath)
  pivotsetup.df <- setupfile.df %>%
    pivot_longer(.data$PC:.data$GFF,
                 names_to = 'Filter',
                 values_to = 'Cal.cons')
  join.df <- left_join(adjustedforbl.df, pivotsetup.df, by = c('Filter', 'Element'))

  calculations.df <- join.df %>%
    mutate(Value = (.data$net_counts*.data$Cal.cons) * area_filter * (1000 / .data$Volume) / .data$MolarW * 1000 * (.data$Drift_2008/.data[[paste0("Drift_", year)]]))

  detectionlimits.df <- setupfile.df %>%
    select(.data$DL_PC:.data$DL_GFF, .data$Element) %>%
    pivot_longer(.data$DL_PC:.data$DL_GFF,
                 names_to = 'Filter',
                 values_to = 'Detection.lim') %>%
    mutate(Filter = str_remove(Filter, 'DL_'))
  Project.w.detectionlim.df <- left_join(calculations.df, detectionlimits.df, by = c ("Filter", "Element"))

  Project.df <- Project.w.detectionlim.df %>%
    select(.data$Sample:.data$Element, .data$Value, .data$Detection.lim)

  return(Project.df)
} #

#Lisence; MIT or GPL3

# test.df <- XRFmydata('datafile.df')
# write_csv(test.df, 'test.csv')
#
# #OOOBSSSS
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
# #solution now;
# datafile.df <- datafile.df %>%
#   select(-(3:18)) %>%
#   select(-starts_with("X")) %>%
#   rename_all(str_remove, pattern = " .*")
#
#
#
#
