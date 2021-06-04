#' @title xsoil
#' @description Extracting soil data from downloaded raster images.
#' @param ximages_output The `data.frame` output from `ximages` function.
#' @param localPath Path in the user's machine to store the images. Default: getwd()
#' @return It returns a `data.frame` object and exports a `.csv` file with the soil data. The `.csv` file will be save under the `POLARISOut` directory.
#' @details This function must be executed after downloading the images (excuting the `ximages` function).
#' @examples 
#' \dontrun{
#' 
#' # df_test <- exkansas
#' #
#' # df_ximages <- ximages(locations = df_test,
#' #                       variables = c('clay'),
#' #                       statistics = c('mean'),
#' #                       layersdepths = c('0_5'))
#' #
#' # df_xsoil <- ximages(locations = df_ximages)
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
#' 
xsoil <- function(ximages_output,
                  localPath = getwd()){
  
  # Binding variables locally to the function (R CMD check)
  coordinates <- local_file <- shape <- extracts <- variables <- 
    statistics <- layersdepths <- Values <- . <- NULL
  
  temp1 = ximages_output %>%
    dplyr::mutate(shape = coordinates %>% # Converting coordinates to shape
                    purrr::map(~sf::st_as_sf(., coords = c('long','lat'),
                                             crs = 4326))) %>%

    # Importing images within the tibble (raster)
    dplyr::mutate(raster = local_file %>% purrr::map(~raster::raster(.))) %>%

    # Extracting values for the points (crop)
    dplyr::mutate(extracts = purrr::map2(.x=raster, .y=shape,
                                         .f=~raster::extract(.x,.y))) %>%

    # Appending column in the data (values)
    dplyr::mutate(coordinates = purrr::map2(.x=coordinates, .y=extracts,
                                            .f=~dplyr::mutate(.x, Values = .y))) %>%

    dplyr::select(variables, statistics, layersdepths, coordinates) %>%
    tidyr::unnest(cols = coordinates) %>% dplyr::ungroup() %>%
    
    dplyr::mutate(Values = dplyr::case_when(
      variables == 'om' ~ 10^Values,
      variables == 'hb' ~ 10^Values,
      variables == 'alpha' ~ 10^Values,
      variables == 'ksat' ~ 10^Values,
      TRUE ~ Values)) %>%

    # Re-arrange table for final output
    tidyr::pivot_wider(names_from = variables,
                       values_from = Values) %>%

    tidyr::unnest(colnames(.))
  
  write.csv(temp1, paste0(localPath,'/POLARISOut/','xsoil_data.csv'),
            row.names = FALSE, na = '')
  
  return(temp1)

}