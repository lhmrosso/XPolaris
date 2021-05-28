#' @title xsoil
#' @description FUNCTION_DESCRIPTION
#' @param ximages_output PARAM_DESCRIPTION
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
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{case_when}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{map2}}
#'  \code{\link[sf]{st_as_sf}}
#'  \code{\link[raster]{raster}},\code{\link[raster]{extract}}
#'  \code{\link[tidyr]{nest}},\code{\link[tidyr]{pivot_wider}}
#' @rdname xsoil
#' @export 
#' @importFrom dplyr mutate select ungroup case_when
#' @importFrom purrr map map2
#' @importFrom sf st_as_sf
#' @importFrom raster raster extract
#' @importFrom tidyr unnest pivot_wider
xsoil <- function(ximages_output,
                  localPath = getwd()){

  suppressMessages(library(sf))
  suppressMessages(library(raster))

  suppressMessages(library(tidyr))
  suppressMessages(library(dplyr))
  suppressMessages(library(purrr))
  suppressMessages(library(forcats))


  ###
  ### EXTRACTING VALUES
  ###

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


    ###
    ### CONVERTING UNITS
    ###

    dplyr::mutate(Values = dplyr::case_when(
      variables == 'om' ~ 10^Values,
      #variables == 'alpha' ~ (10^Values)/10.2,
      #variables == 'ksat' ~ (10^Values)*24,
      #variables == 'hb' ~ 10^Values,
      TRUE ~ Values)) %>%

    # Re-arrange table for final output
    tidyr::pivot_wider(names_from = variables,
                       values_from = Values) %>%

    tidyr::unnest(colnames(.))


  ###
  ### EXPORTING A .CSV
  ###

  write.csv(temp1, paste0(localPath,'/POLARISOut/','xsoil_data.csv'),
            row.names = FALSE, na = '')


  ### Data.frame
  return(temp1)


}
