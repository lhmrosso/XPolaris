#' @title ximages
#' @description Downloading raster images from the POLARIS database.
#' @param locations A `data.frame` object with location information.
#' @param variables A `vector` with soil variable codes. See the details section.
#' @param statistics A `vector` with the distribution summary options. See the details section.
#' @param layersdepths A `vector` with the soil depth codes requested by the user. See the details.
#' @param localPath Path in the user's machine to store the images. Default: getwd()
#' @return This function simply downloads the images from the POLARIS database, according to the user request. Images are saved under a new directory, called `POLARISOut`, with the `localPath`.
#' @details The 13 `variable` code options are: `ph` (soil water pH), `om` (organic matter), `clay`, `sand`, `silt`, `bd` (bulk density), `hb` (bubbling pressure), `n` (pore size distribution), `alpha` (scale parameter inversely proportional to mean pore diameter), `ksat` (saturated hydraulic conductivity), `lambda` (pore size distribution index), `theta_r` (residual soil water content), and `theta_s` (saturated soil water content). There are five options of summary statistics: `mean`, `mode`, median (`p50`), five (`p5`) and 95 (`p95`) percentiles. There are six options of soil depth layers: `0_5`, `5_15`, `15_30`, `30_60`, `60_100`, and `100_200` cm. For more details on variable units please check package documentation.
#' @examples 
#' \dontrun{
#' 
#' # df_test <- exkansas
#' #
#' # df_ximages <- ximages(locations = df_test,
#' #                       variables = c('clay'),
#' #                       statistics = c('mean'),
#' #                       layersdepths = c('0_5'))
#' }
#' @seealso 
#'  \code{\link[XPolaris]{xplot}},\code{\link[XPolaris]{xsoil}}
#' @rdname ximages
#' @export 
#' @importFrom purrr map map2
#' @importFrom magrittr "%>%" 
#' @importFrom tidyr gather pivot_wider unnest nest separate
#' @importFrom dplyr mutate case_when select group_by ungroup filter row_number left_join

ximages <- function(locations,
                    variables,
                    statistics,
                    layersdepths,
                    localPath = getwd()){
  
  # Binding variables locally to the function (R CMD check)
  ID <- Value <- Up <- Lw <- Coord <- Alt1 <- Alt2 <- Alt1_lat <- 
    Alt1_long <- Alt2_lat <- Alt2_long <- img1 <- img2 <- img3 <- 
    img4 <- . <- Opt <- Unique <- lim <- lat_range <- long_range <- 
    lat <- long <- squares <- coordinates <- images <- local_file <- 
    local_folder <- downloaded <- online_file <- NULL
  
  temp1 = locations %>%
    tidyr::gather('Coord','Value',-ID) %>%
    dplyr::mutate(Value = as.numeric(Value)) %>%
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
  
  temp2 %>%
    dplyr::select(variables,statistics,layersdepths) %>% unique() %>% dplyr::mutate(
      local_folder = paste0(localPath,'/POLARISOut/', statistics,'/',
                            variables,'/',layersdepths)) %>%

    # If the directories already exist they will not be overwritten with new ones
    dplyr::mutate(create = local_folder %>% purrr::map(~dir.create(., recursive = T, 
                                                                   showWarnings = F)))
  
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
  
  return(temp2)

}