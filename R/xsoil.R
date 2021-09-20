#' @title xsoil
#' @description Extracting soil data from downloaded images.
#' @param ximages_output A `data.frame` output from `ximages` function.
#' @param localPath Path to store the images. Default: `tempdir()`
#' @return It returns a `data.frame` object and exports a `.csv` 
#' file with the soil data. The `.csv` file will be save under 
#' a new directory (`POLARISOut`) in the user's machine.
#' @details This function must be executed after downloading the images, 
#' because it depends on the output from the `ximages` function.
#' @examples 
#' \dontrun{
#' df_test <- exkansas
#' df_ximages <- ximages(locations = df_test,
#'                       variables = c('clay'),
#'                       statistics = c('mean'),
#'                       layersdepths = c('0_5'))
#' df_xsoil <- xsoil(ximages_output = df_ximages)
#' }
#' @seealso 
#'  \code{\link[XPolaris]{xplot}},\code{\link[XPolaris]{ximages}}
#' @rdname xsoil
#' @export 
#' @importFrom sf st_as_sf
#' @importFrom magrittr "%>%" 
#' @importFrom purrr map map2
#' @importFrom utils data write.csv
#' @importFrom raster raster extract
#' @importFrom tidyr unnest pivot_wider
#' @importFrom dplyr mutate select ungroup case_when

xsoil <- function(ximages_output,
                  localPath = tempdir()){
  
  # Binding variables to the function
  
  coordinates <- local_file <- shape <- extracts <- variables <- 
    statistics <- layersdepths <- Values <- . <- checking <- NULL
  
  temp1 <- ximages_output %>%
    
    # Making data.frames into shape files
    
    dplyr::mutate(shape = coordinates %>% 
                    purrr::map(~sf::st_as_sf(., coords = c('long', 'lat'),
                                             crs = 4326))) %>%
    
    # Checking if all images were downloaded
    
    dplyr::mutate(checking = dplyr::case_when(file.exists(local_file) ~ 'yes', 
                                              TRUE ~ 'no')) %>% 
    
    dplyr::filter(checking == 'yes') %>% 
    
    dplyr::select(-checking)
  
  ## IMPORTING DOWNLOADED
  
  temp2 <- data.frame(NULL)
  
  if(nrow(temp1) > 0)
    
    # Importing images from local folders
    
    temp2 <- temp1 %>% 
    
    dplyr::mutate(raster = local_file %>% 
                    purrr::map(~raster::raster(.))) %>%
    
    # Extracting values for the shape file points
    
    dplyr::mutate(extracts = purrr::map2(.x = raster, 
                                         .y = shape,
                                         .f = ~raster::extract(.x, .y))) %>%

    # Appending column in the original coordinates data.frame
    
    dplyr::mutate(coordinates = 
                    purrr::map2(.x = coordinates, 
                                .y = extracts,
                                .f = ~dplyr::mutate(.x, Values = .y))) %>%

    dplyr::select(variables, 
                  statistics, 
                  layersdepths, 
                  coordinates) %>%
    
    tidyr::unnest(cols = coordinates) %>% 
    
    dplyr::ungroup() %>%
    
    # Converting POLARIS units into conventional units
    
    dplyr::mutate(Values = dplyr::case_when(variables == 'om' ~ 10^Values,
                                            variables == 'hb' ~ 10^Values,
                                            variables == 'alpha' ~ 10^Values,
                                            variables == 'ksat' ~ 10^Values,
                                            TRUE ~ Values)) %>%

    # Re-arrange table for final output
    tidyr::pivot_wider(names_from = variables,
                       values_from = Values) %>%

    tidyr::unnest(colnames(.))
  
  ## EXPORTING CSV
  
  if(nrow(temp1) > 0)
    
    write.csv(temp2, 
              
              file.path(localPath,
                        'POLARISOut',
                        'xsoil_data.csv'),
              
              row.names = FALSE, 
              na = '')
  
  ## FUNCTION OUTPUT
  
  return(temp2)
  
}