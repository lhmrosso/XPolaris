#' @title ximages
#' @description Downloading images from the POLARIS database.
#' @param locations A `data.frame` object with locations data.
#' @param variables A `vector` with soil variable codes. There are 13 
#' variables in the POLARIS database: `ph` (soil water pH), `om` (organic 
#' matter), `clay`, `sand`, `silt`, `bd` (bulk density), `hb` (bubbling 
#' pressure), `n` (pore size distribution), `alpha` (scale parameter 
#' inversely proportional to mean pore diameter), `ksat` (saturated 
#' hydraulic conductivity), `lambda` (pore size distribution index), 
#' `theta_r` (residual soil water content), and `theta_s` (saturated 
#' soil water content).
#' @param statistics A `vector` with the distribution summary options. 
#' There are five options of summary statistics: `mean`, `mode`, median 
#' (`p50`), five (`p5`) and 95 (`p95`) percentiles.
#' @param layersdepths A `vector` with the soil depth codes. There are six 
#' options of soil depth layers: `0_5`, `5_15`, `15_30`, `30_60`, `60_100`, 
#' and `100_200` cm.
#' @param localPath Path to store the images. Default: `tempdir()`
#' @return This function simply downloads the images from the POLARIS 
#' database, according to the user request. Images are saved under a 
#' new directory, called `POLARISOut`, within the `localPath`.
#' @details For details on units check package repository.
#' @examples 
#' \dontrun{
#' df_test <- exkansas
#' df_ximages <- ximages(locations = df_test,
#'                       variables = c('clay'),
#'                       statistics = c('mean'),
#'                       layersdepths = c('0_5'))
#' }
#' @seealso 
#'  \code{\link[XPolaris]{xplot}},\code{\link[XPolaris]{xsoil}}
#' @rdname ximages
#' @export 
#' @importFrom purrr map map2
#' @importFrom magrittr "%>%" 
#' @importFrom curl has_internet 
#' @importFrom utils download.file 
#' @importFrom httr GET timeout http_error message_for_status
#' @importFrom tidyr gather pivot_wider unnest nest separate
#' @importFrom dplyr mutate case_when select group_by 
#' ungroup filter row_number left_join

ximages <- function(locations,
                    variables,
                    statistics,
                    layersdepths,
                    localPath = tempdir()){
  
  # Binding variables to the function
  
  ID <- Value <- Up <- Lw <- Coord <- Alt1 <- Alt2 <- Alt1_lat <- 
    Alt1_long <- Alt2_lat <- Alt2_long <- img1 <- img2 <- img3 <- 
    img4 <- . <- Opt <- Unique <- lim <- lat_range <- long_range <- 
    lat <- long <- squares <- coordinates <- images <- local_file <- 
    local_folder <- downloaded <- online_file <- POLARISWeb <- NULL
  
  ## IDENTIFYING MINIMUM NUMBER OF LAT-LONG SQUARES
  
  temp1 <- locations %>%
    
    tidyr::gather('Coord', 'Value', -ID) %>%
    
    dplyr::mutate(Value = as.numeric(Value)) %>%
    
    # Round coordinates interval
    
    dplyr::mutate(Up = ceiling(Value), 
                  Lw = floor(Value)) %>%

    # In case coordinates are round it creates two possibilities
    
    dplyr::mutate(Alt1 = dplyr::case_when(Up == Lw ~ paste0(Lw, Up+1), 
                                          TRUE ~ paste0(Lw, Up)),
                  
                  Alt2 = dplyr::case_when(Up == Lw ~ paste0(Lw-1, Up), 
                                          TRUE ~ paste0(Lw, Up))) %>%

    dplyr::select(-Up, -Lw, -Value) %>%
    
    tidyr::pivot_wider(names_from = Coord,
                       values_from = c(Alt1, Alt2)) %>%

    # Possible image options in case there are round values
    
    dplyr::mutate(img1 = paste0('lat', Alt1_lat, '_lon', Alt1_long),
                  img2 = paste0('lat', Alt2_lat, '_lon', Alt1_long),
                  img3 = paste0('lat', Alt2_lat, '_lon', Alt2_long),
                  img4 = paste0('lat', Alt1_lat, '_lon', Alt2_long)) %>%

    dplyr::select(ID, img1, img2, img3, img4) %>% 
    
    tidyr::unnest(colnames(.)) %>%
    
    tidyr::gather('Opt', 'lim', -ID) %>% 
    
    dplyr::group_by(Opt) %>%
    
    tidyr::nest() %>% 
    
    dplyr::ungroup() %>%

    dplyr::mutate(Unique = data %>% 
                    purrr::map(~length(unique(.$lim))) %>% 
                    unlist()) %>%
    
    # Choosing the option with minimum number of images
    
    dplyr::filter(Unique == min(Unique)) %>% 
    
    dplyr::filter(dplyr::row_number() == 1) %>%
    
    dplyr::select(-Unique, -Opt) %>% 
    
    tidyr::unnest('data') %>%
    
    tidyr::separate(lim, into = c('lat_range', 
                                  'long_range'), sep = '_') %>%
    
    dplyr::left_join(locations, by = 'ID') %>% 
    
    dplyr::group_by(lat_range, long_range) %>%
    
    tidyr::nest(coordinates = c(ID, lat, long)) %>% 
    
    dplyr::ungroup() %>%
    
    dplyr::mutate(squares = dplyr::row_number()) %>%
    
    # Final images to cover all locations

    dplyr::select(squares,
                  lat_range,
                  long_range,
                  coordinates)
  
  ## CREATING FOLDERS IN THE LOCAL MACHINE
  
  temp2 <- temp1 %>% 
    
    dplyr::left_join(expand.grid(squares = temp1$squares,
                                 variables = variables,
                                 statistics = statistics,
                                 layersdepths = layersdepths),
                     
                     by = 'squares') %>% 
    
    dplyr::mutate(images = dplyr::row_number()) %>%

    # Convenient way to store the images
    
    dplyr::mutate(local_file = 
                    file.path(localPath,
                              'POLARISOut',
                              statistics,
                              variables,
                              layersdepths, 
                              
                              paste0(lat_range, '_',
                                     long_range,'.tif'))) %>%

    # temp2 will be used twice
    
    dplyr::select(squares,
                  lat_range,
                  long_range,
                  images,
                  variables,
                  statistics,
                  layersdepths,
                  coordinates,
                  local_file)
  
  # Creating the folders in the local machine
  
  temp2 %>%
    
    dplyr::select(variables,
                  statistics,
                  layersdepths) %>% 
    
    unique() %>% 
    
    # Sub-folders to save the images from POLARIS
    
    dplyr::mutate(local_folder = file.path(localPath,
                                           'POLARISOut',
                                           statistics,
                                           variables,
                                           layersdepths)) %>%

    dplyr::mutate(create = local_folder %>% 
                    purrr::map(~dir.create(., 
                                           recursive = TRUE, 
                                           showWarnings = FALSE)))
  
  ## DOWNLOADING THE IMAGES
  
  POLARISWeb <- 'http://hydrology.cee.duke.edu/POLARIS/PROPERTIES/v1.0/'
  
  temp3 <- temp2 %>% 
    
    dplyr::select(local_file,
                  squares,
                  lat_range,
                  long_range,
                  images,
                  variables,
                  statistics,
                  layersdepths) %>%
    
    unique() %>% 
    
    # Online database with images organized is sub-directories
    
    dplyr::mutate(online_file = 
                    paste0(POLARISWeb,
                           variables, '/',
                           statistics, '/',
                           layersdepths, '/',
                           
                           lat_range, '_',
                           long_range, '.tif')) %>%

    # If the files have already been downloaded they will not be again
    
    dplyr::mutate(downloaded = 
                    dplyr::case_when(file.exists(local_file) ~ 'yes', 
                                     TRUE ~ 'no')) %>%
    
    # Identifying images still missing from the user request
    
    dplyr::filter(downloaded == 'no')
  

  ## ROUTINE TO DOWNLOAD IMAGES
  
  xdown <- function(ON,LC){
    
    try_GET <- function(x){
      tryCatch(httr::GET(url = x, httr::timeout(600)),
               error = function(e) conditionMessage(e),
               warning = function(w) conditionMessage(w))
    }
    
    is_response <- function(x){
      class(x) == "response"
    }
    
    # Internet connection
    if(!curl::has_internet()){
      message("No internet connection.")
      return(invisible(NULL))
    }
    
    # Timeout problems
    resp <- try_GET(ON)
    if(!is_response(resp)){
      message(resp)
      return(invisible(NULL))
    }
    
    # Stop if status > 400
    if(httr::http_error(resp)){
      httr::message_for_status(resp)
      return(invisible(NULL))
    }
    
    download.file(url = ON, 
                  destfile = LC, 
                  mode = 'wb')
    
  }
  
  if(nrow(temp3) > 0)
    
    dplyr::mutate(temp3, 
                  download = purrr::map2(.x = online_file, 
                                         .y = local_file,
                                         .f = ~xdown(ON = .x, 
                                                     LC = .y)))
  
  ## FUNCTION OUTPUT
  
  return(temp2)

}