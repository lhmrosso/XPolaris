#' @title xplot
#' @description Identifies the images from which locations must be accessed.
#' @param locations A data.frame with location information.
#' @param localPath Path in the user's machine to download the images., Default: getwd()
#' @return A ggplot object and an exported .jpeg image with the locations map.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[tidyr]{gather}},\code{\link[tidyr]{pivot_wider}},\code{\link[tidyr]{nest}},\code{\link[tidyr]{c("nest", "nest", "nest")}},\code{\link[tidyr]{separate}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{case_when}},\code{\link[dplyr]{select}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{ranking}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{mutate_all}}
#'  \code{\link[purrr]{map}}
#'  \code{\link[ggplot2]{ggplot}},\code{\link[ggplot2]{geom_abline}},\code{\link[ggplot2]{geom_polygon}},\code{\link[ggplot2]{map_data}},\code{\link[ggplot2]{aes}},\code{\link[ggplot2]{geom_label}},\code{\link[ggplot2]{scale_continuous}},\code{\link[ggplot2]{coord_map}},\code{\link[ggplot2]{theme}},\code{\link[ggplot2]{margin}},\code{\link[ggplot2]{ggsave}}
#' @rdname xplot
#' @export 
#' @importFrom tidyr gather pivot_wider unnest nest separate
#' @importFrom dplyr mutate case_when select group_by ungroup filter row_number arrange mutate_at
#' @importFrom purrr map
#' @importFrom ggplot2 ggplot geom_hline geom_vline geom_polygon map_data aes geom_text scale_x_continuous scale_y_continuous coord_map theme element_blank ggsave
xplot <- function(locations,
                  localPath = getwd()){

  suppressMessages(library(mapproj))
  suppressMessages(library(ggplot2))

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
      Alt1 = dplyr::case_when(Up==Lw~paste0(Lw,'_',Up+1), TRUE~paste0(Lw,'_',Up)),
      Alt2 = dplyr::case_when(Up==Lw~paste0(Lw-1,'_',Up), TRUE~paste0(Lw,'_',Up))) %>%

    # Possible square combinations
    dplyr::select(-Up, -Lw, -Value) %>%
    tidyr::pivot_wider(names_from = Coord,
                       values_from = c(Alt1,Alt2)) %>%

    dplyr::mutate(img1 = paste0(Alt1_lat,'_',Alt1_long),
                  img2 = paste0(Alt2_lat,'_',Alt1_long),
                  img3 = paste0(Alt2_lat,'_',Alt2_long),
                  img4 = paste0(Alt1_lat,'_',Alt2_long)) %>%

    # Choosing the option with fewer squares to download (fewer images)
    dplyr::select(ID,img1,img2,img3,img4) %>% tidyr::unnest(colnames(.)) %>%
    tidyr::gather('Opt','lim',-ID) %>% dplyr::group_by(Opt) %>%
    tidyr::nest() %>% dplyr::ungroup() %>%

    dplyr::mutate(Unique = data %>% purrr::map(~length(unique(.$lim))) %>% unlist()) %>%
    dplyr::filter(Unique==min(Unique)) %>% dplyr::filter(dplyr::row_number()==1) %>%
    dplyr::select(-Unique,-Opt) %>% tidyr::unnest('data') %>%

    # Corners of the square polygon for each location
    tidyr::separate(lim, into=c('Lwlat','Uplat','Lwlong','Uplong'), sep='_') %>%

    dplyr::mutate(a = paste(Lwlong,Lwlat, sep='_'),
                  b = paste(Uplong,Lwlat, sep='_'),
                  c = paste(Uplong,Uplat, sep='_'),
                  d = paste(Lwlong,Uplat, sep='_'),
                  e = paste(Lwlong,Lwlat, sep='_')) %>%

    dplyr::select(a,b,c,d,e) %>% unique() %>%
    dplyr::mutate(squares = dplyr::row_number()) %>%
    tidyr::gather('Corner','Values', -squares) %>% # Boundaries
    tidyr::separate(Values, into=c('long','lat'), sep='_') %>%
    dplyr::arrange(squares, Corner) %>% dplyr::ungroup() %>%
    dplyr::mutate_at(vars(long,lat), ~as.numeric(.))


  ###
  ### GENERATING THE PLOT
  ###

  temp2 = ggplot2::ggplot()+
    ggplot2::geom_hline(yintercept=c(25:50), size=0.3, linetype=1, color='gray90')+
    ggplot2::geom_vline(xintercept=c(-125:-66), size=0.3, linetype=1, color='gray90')+
    ggplot2::geom_hline(yintercept=seq(25,50,5), size=0.3, linetype=1, color='gray80')+
    ggplot2::geom_vline(xintercept=seq(-125,-70,5), size=0.3, linetype=1, color='gray80')+

    ggplot2::geom_polygon(data=ggplot2::map_data("state"),
                          ggplot2::aes(x=long, y=lat, group=group),
                          fill='#a1d99b', color='gray50', size=0.1, alpha=0.5)+

    ggplot2::geom_polygon(data=temp1, ggplot2::aes(x=long, y=lat, group=as.character(squares)),
                          fill='red', size=0, alpha=0.5)+

    ggplot2::geom_text(data = data.frame(X=rep(-127.5,6), Y=seq(25,50,5), lab=paste0(seq(25,50,5),'°N')),
                       ggplot2::aes(x = X, y = Y, label = lab), size=2.75, hjust=0.5, angle=0)+
    ggplot2::geom_text(data = data.frame(X=seq(-125,-70,5), Y=rep(23,12), lab=paste0(abs(seq(-125,-70,5)),'°W')),
                       ggplot2::aes(x = X, y = Y, label = lab), size=2.75, hjust=0.5, angle=0)+

    ggplot2::scale_x_continuous(limits=c(-129.5,-65), expand=c(0,0))+
    ggplot2::scale_y_continuous(limits=c(22,51), expand=c(0,0))+
    ggplot2::coord_map("conic", lat0=20)+

    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.background = ggplot2::element_blank())


  ###
  ### EXPORTING A .CSV
  ###

  dir.create(paste0(localPath,'/POLARISOut'), showWarnings = F)
  ggplot2::ggsave(paste0(localPath,'/POLARISOut/','ximages.jpg'),
                  plot=temp2, width=6.5, height=4, dpi=300, units='in')


  ### ggplot
  return(temp2)


}

# example of how to use this function
# xplot(exkansas)
