#' @title xplot
#' @description Identifies images containing the locations.
#' @param locations A `data.frame` object with locations data.
#' @param localPath Path to store the images. Default: `tempdir()`
#' @return It returns a `ggplot` object and exports a `.jpeg` image with 
#' the locations map. Images will be saved under a new directory, called 
#' `POLARISOut`. The directory will be created within the `localPath` 
#' and used to store the raster images from the POLARIS database.
#' @details This function is useful for checking if locations were correctly 
#' informed, but its output is not required by `ximages` or `xsoil`. If a 
#' given location falls in the border of an image (round coordinates), 
#' the function will make sure a minimum number of images is downloaded.
#' @examples 
#' \dontrun{
#' df_test <- exkansas
#' xplot(locations = df_test)
#' }
#' @seealso 
#'  \code{\link[XPolaris]{ximages}},\code{\link[XPolaris]{xsoil}}
#' @rdname xplot
#' @export 
#' @importFrom purrr map
#' @importFrom magrittr "%>%" 
#' @importFrom utils data write.csv
#' @importFrom tidyr gather pivot_wider unnest nest separate
#' @importFrom dplyr mutate case_when select group_by ungroup filter 
#' row_number arrange mutate_at
#' @importFrom ggplot2 ggplot geom_hline geom_vline geom_polygon map_data 
#' aes geom_text scale_x_continuous scale_y_continuous coord_map theme 
#' element_blank ggsave

xplot <- function(locations,
                  localPath = tempdir()){
  
  # Binding variables to the function
  
  ID <- Value <- Up <- Lw <- Coord <- Alt1 <- Alt2 <- Alt1_lat <- 
    Alt1_long <- Alt2_lat <- Alt2_long <- img1 <- img2 <- img3 <- 
    img4 <- . <- Opt <- Unique <- lim <- Lwlong <- Lwlat <- Uplong <- 
    Uplat <- a <- b <- d <- e <- squares <- Values <- Corner <- vars <- 
    long <- lat <- group <- X <- Y <- lab <- NULL
  
  ## IDENTIFYING MINIMUM NUMBER OF LAT-LONG SQUARES
  
  temp1 <- locations %>%
    
    tidyr::gather('Coord', 'Value', -ID) %>%
    
    dplyr::mutate(Value = as.numeric(Value)) %>%
    
    dplyr::mutate(Up = ceiling(Value), 
                  Lw = floor(Value)) %>%

    # Different from the ximages function
    
    dplyr::mutate(Alt1 = dplyr::case_when(Up == Lw ~ paste0(Lw, '_', Up+1), 
                                          TRUE ~ paste0(Lw, '_', Up)),
                  
                  Alt2 = dplyr::case_when(Up == Lw ~ paste0(Lw-1, '_', Up),
                                          TRUE ~ paste0(Lw, '_', Up))) %>%

    # Possible image combinations
    
    dplyr::select(-Up, -Lw, -Value) %>%
    
    tidyr::pivot_wider(names_from = Coord,
                       values_from = c(Alt1, Alt2)) %>%

    dplyr::mutate(img1 = paste0(Alt1_lat, '_', Alt1_long),
                  img2 = paste0(Alt2_lat, '_', Alt1_long),
                  img3 = paste0(Alt2_lat, '_', Alt2_long),
                  img4 = paste0(Alt1_lat, '_', Alt2_long)) %>%

    # Choosing the option with fewer images to download
    
    dplyr::select(ID, img1, img2, img3, img4) %>% 
    
    tidyr::unnest(colnames(.)) %>%
    
    tidyr::gather('Opt', 'lim', -ID) %>% 
    
    dplyr::group_by(Opt) %>%
    
    tidyr::nest() %>% 
    
    dplyr::ungroup() %>%

    dplyr::mutate(Unique = data %>% 
                    purrr::map(~length(unique(.$lim))) %>% 
                    unlist()) %>%
    
    dplyr::filter(Unique == min(Unique)) %>% 
    
    dplyr::filter(dplyr::row_number() == 1) %>%
    
    dplyr::select(-Unique, -Opt) %>% 
    
    tidyr::unnest('data') %>%

    ## IMAGE CORNERS TO GENERATE POLYGONS
    
    tidyr::separate(lim, into = c('Lwlat', 
                                  'Uplat', 
                                  'Lwlong', 
                                  'Uplong'), sep = '_') %>%

    dplyr::mutate(a = paste(Lwlong, Lwlat, sep = '_'),
                  b = paste(Uplong, Lwlat, sep = '_'),
                  c = paste(Uplong, Uplat, sep = '_'),
                  d = paste(Lwlong, Uplat, sep = '_'),
                  e = paste(Lwlong, Lwlat, sep = '_')) %>%

    dplyr::select(a, b, c, d, e) %>% 
    
    unique() %>%
    
    dplyr::mutate(squares = dplyr::row_number()) %>%
    
    tidyr::gather('Corner', 'Values', -squares) %>%
    
    tidyr::separate(Values, into = c('long', 'lat'), sep = '_') %>%
    
    dplyr::arrange(squares, Corner) %>% 
    
    dplyr::ungroup() %>%
    
    # Making coordinates numeric to enter ggplot
    
    dplyr::mutate_at(dplyr::vars(long, lat), ~as.numeric(.))
  
  ## CREATING THE PLOT
  
  temp2 <- ggplot2::ggplot()+
    
    # Adding specific grid lines behind the US map
    
    ggplot2::geom_hline(yintercept = c(25:50), 
                        size = 0.3, linetype = 1, color = 'gray90')+
    
    ggplot2::geom_vline(xintercept = c(-125:-66), 
                        size = 0.3, linetype = 1, color = 'gray90')+
    
    ggplot2::geom_hline(yintercept = seq(25, 50, 5), 
                        size = 0.3, linetype = 1, color = 'gray80')+
    
    ggplot2::geom_vline(xintercept = seq(-125, -70, 5), 
                        size = 0.3, linetype = 1, color = 'gray80')+
    
    # Plotting the US shape with states
    
    ggplot2::geom_polygon(data = ggplot2::map_data("state"),
                          ggplot2::aes(x = long, y = lat, group = group),
                          fill = '#a1d99b', color = 'gray50', 
                          size = 0.1, alpha = 0.5)+

    # Square images for all locations
    
    ggplot2::geom_polygon(data = temp1, 
                          ggplot2::aes(x = long, y = lat, 
                                       group = as.character(squares)),
                          
                          fill = 'red', size = 0, alpha = 0.5)+

    # Grid labels for latitude
    
    ggplot2::geom_text(data = 
                         data.frame(X = rep(-127.5, 6), Y = seq(25, 50, 5), 
                                    lab = paste0(seq(25, 50, 5),' N')),
                       
                       ggplot2::aes(x = X, y = Y, label = lab), 
                       size = 2.75, hjust = 0.5, angle = 0)+
    
    # Grid labels for longitude
    
    ggplot2::geom_text(data = 
                         data.frame(X = seq(-125, -70, 5), Y = rep(23, 12), 
                                    lab = paste0(abs(seq(-125, -70, 5)),' W')),
                       
                       ggplot2::aes(x = X, y = Y, label = lab), 
                       size = 2.75, hjust = 0.5, angle = 0)+

    # Scales and projection
    
    ggplot2::scale_x_continuous(limits = c(-129.5, -65), 
                                expand = c(0, 0))+
    
    ggplot2::scale_y_continuous(limits = c(22, 51), 
                                expand = c(0, 0))+
    
    ggplot2::coord_map("conic", lat0 = 20)+

    # A simple theme
    
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.background = ggplot2::element_blank())
  
  ## CREATING LOCAL FOLDER TO SAVE THE PLOT
  
  dir.create(file.path(localPath,'POLARISOut'), 
             showWarnings = FALSE)
  
  # Exporting the plot
  
  ggplot2::ggsave(file.path(localPath,
                            'POLARISOut',
                            'ximages.jpg'),
                  
                  plot = temp2, 
                  width = 6.5, 
                  height = 4, 
                  dpi = 300, 
                  units = 'in')
  
  ## FUNCTION OUTPUT
  
  return(temp2)

}