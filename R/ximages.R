#' @title ximages
#' @description FUNCTION_DESCRIPTION
#' @param locations PARAM_DESCRIPTION
#' @param variables PARAM_DESCRIPTION
#' @param statistics PARAM_DESCRIPTION
#' @param layersdepths PARAM_DESCRIPTION
#' @param localPath PARAM_DESCRIPTION, Default: getwd()
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[tidyr]{gather}},\code{\link[tidyr]{pivot_wider}},\code{\link[tidyr]{nest}},\code{\link[tidyr]{c("nest", "nest", "nest")}},\code{\link[tidyr]{separate}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{case_when}},\code{\link[dplyr]{select}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{ranking}},\code{\link[dplyr]{mutate-joins}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{map2}}
#' @rdname ximages
#' @export 
#' @importFrom tidyr gather pivot_wider unnest nest separate
#' @importFrom dplyr mutate case_when select group_by ungroup filter row_number left_join
#' @importFrom purrr map map2
ximages <- function(locations,
                    variables,
                    statistics,
                    layersdepths,
                    localPath = getwd()){

  suppressMessages(library(tidyr))
  suppressMessages(library(dplyr))
  suppressMessages(library(purrr))
  suppressMessages(library(forcats))

  if(!is.data.frame(locations))
    stop('Input must be a data.frame object')

  if(length(unique(locations$ID)) < nrow(locations))
    stop('Please use unique IDs for your locations')


  ###
  ### IDENTIFY MINIMUM NUMBER OF SQUARES
  ###

  temp1 = locations %>%
    tidyr::gather('Coord','Value',-ID) %>%
    dplyr::mutate(Value = as.numeric(Value)) %>%

    # Upper and lower square boundaries (one-by-one lat-long)
    dplyr::mutate(Up = ceiling(Value), Lw = floor(Value)) %>%

    dplyr::mutate( # Just in case a locations falls in square border
      Alt1 = dplyr::case_when(Up==Lw~paste0(Lw,Up+1), TRUE~paste0(Lw,Up)),
      Alt2 = dplyr::case_when(Up==Lw~paste0(Lw-1,Up), TRUE~paste0(Lw,Up))) %>%

    # Possible square combinations
    dplyr::select(-Up, -Lw, -Value) %>%
    tidyr::pivot_wider(names_from = Coord,
                       values_from = c(Alt1,Alt2)) %>%

    dplyr::mutate(img1 = paste0('lat',Alt1_lat,'_lon',Alt1_long),
                  img2 = paste0('lat',Alt2_lat,'_lon',Alt1_long),
                  img3 = paste0('lat',Alt2_lat,'_lon',Alt2_long),
                  img4 = paste0('lat',Alt1_lat,'_lon',Alt2_long)) %>%

    # Choosing the option with fewer squares to download (fewer images)
    dplyr::select(ID,img1,img2,img3,img4) %>% tidyr::unnest(colnames(.)) %>%
    tidyr::gather('Opt','lim',-ID) %>% dplyr::group_by(Opt) %>%
    tidyr::nest() %>% dplyr::ungroup() %>%

    dplyr::mutate(Unique = data %>% purrr::map(~length(unique(.$lim))) %>% unlist()) %>%
    dplyr::filter(Unique==min(Unique)) %>% dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::select(-Unique,-Opt) %>% tidyr::unnest('data') %>%

    # Bringing dataframe back into the original shape
    tidyr::separate(lim, into=c('lat_range','long_range'), sep='_') %>%
    dplyr::left_join(locations, by='ID') %>% dplyr::group_by(lat_range,long_range) %>%
    tidyr::nest(coordinates = c(ID,lat,long)) %>% dplyr::ungroup() %>%
    dplyr::mutate(squares = dplyr::row_number()) %>%

    dplyr::select(squares,lat_range,long_range,coordinates)


  ###
  ### MINIMUM NUMBER OF IMAGES TO RETRIEVE THE DATA
  ###

  temp2 = temp1 %>% # All data to be extracted
    dplyr::left_join(expand.grid(squares = temp1$squares,
                                 variables = variables,
                                 statistics = statistics,
                                 layersdepths = layersdepths),

                     # Square and image IDs that will cover all the locations
                     by = 'squares') %>% dplyr::mutate(images = dplyr::row_number()) %>%

    dplyr::mutate( # Building image paths to extract soil
      local_file = paste0(localPath,'/POLARISOut/', statistics,'/',variables,
                          '/',layersdepths,'/',lat_range,'_',long_range,'.tif')) %>%

    dplyr::select(squares,lat_range,long_range, # Square information
                  images,variables,statistics,layersdepths, # Image information
                  coordinates,local_file) # Location information (nested tibble)


  ###
  ### CREATING LOCAL FOLDERS TO SAVE THE IMAGES
  ###

  temp2 %>%
    dplyr::select(variables,statistics,layersdepths) %>% unique() %>% dplyr::mutate(
      local_folder = paste0(localPath,'/POLARISOut/', statistics,'/',variables,'/',layersdepths)) %>%

    # If the directories already exist they will not be overwritten with new ones
    dplyr::mutate(create = local_folder %>% purrr::map(~dir.create(., recursive = T, showWarnings = F)))


  ###
  ### DOWNLOADING THE MISSING IMAGES
  ###

  temp3 = temp2 %>% dplyr::select(local_file,squares,lat_range,long_range,
                                  images,variables,statistics,layersdepths) %>%

    unique() %>% dplyr::mutate( # Online path to the POLARIS database
      online_file = paste0('http://hydrology.cee.duke.edu/POLARIS/PROPERTIES/v1.0/',
                           variables,'/',statistics,'/',layersdepths,'/',
                           lat_range,'_',long_range,'.tif')) %>%

    # Identify images already downloaded
    dplyr::mutate(downloaded = dplyr::case_when(
      file.exists(local_file) ~ 'yes', TRUE ~ 'no')) %>%
    dplyr::filter(downloaded == 'no')

  if(nrow(temp3) > 0) # Downloading
    dplyr::mutate(temp3, download = purrr::map2(
      .x = online_file, .y = local_file, # Paths
      .f = ~download.file(url = .x, destfile = .y)))


  ### Tibble
  return(temp2)


}
