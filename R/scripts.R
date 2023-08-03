#' Extracts text from given URL
#'
#' @param url A character string of the URL.
#'
#' @return A character string of the extracted text.
#' 
#' @examples
#' get_text("https://www.youtube.com/@oggyinformatics/")
#' @export
get_text <- function(url) {
  webpage <- rvest::read_html(url)
  rvest::html_text(webpage)
}

#' Extracts subscriber count from given URL
#'
#' @param url A character string of the YouTube channel URL.
#'
#' @return A character string of the subscriber count.
#' 
#' @examples
#' get_subscriber_count("https://www.youtube.com/@oggyinformatics/")
#' @export
get_subscriber_count <- function(url) {
  text <- get_text(paste0(url, '/videos'))
  subscriberCountText <- stringr::str_extract(text, "(?<=subscriberCountText.{0,100})\"[0-9]+(\\.[0-9]+)?[A-Za-z]* subscribers")
  subscriberCountText <- gsub(".*\"([0-9]+(\\.[0-9]+)?[A-Za-z]* subscribers)", "\\1", subscriberCountText)
  return(subscriberCountText)
}

#' Extracts channel handle from given URL
#'
#' @param url A character string of the YouTube channel URL.
#'
#' @return A character string of the channel handle.
#' 
#' @examples
#' get_channel_handle("https://www.youtube.com/@oggyinformatics/")
#' @export
get_channel_handle <- function(url) {
  text <- get_text(paste0(url, '/videos'))
  channelHandleText <- stringr::str_extract(text, "(?<=channelHandleText.{0,100})\"@(.*?)\"")
  channelHandleText <- stringr::str_replace_all(channelHandleText, "\"", "")
  return(channelHandleText)
}

#' Extracts video count from given URL
#'
#' @param url A character string of the YouTube channel URL.
#'
#' @return A character string of the video count.
#' 
#' @examples
#' get_video_count("https://www.youtube.com/@oggyinformatics/")
#' @export
get_video_count <- function(url) {
  text <- get_text(paste0(url, '/videos'))
  videosCountText <- stringr::str_extract(text, "(?<=videosCountText.{0,100})\"[0-9]+")
  videosCountText <- stringr::str_replace_all(videosCountText, "\"", "")
  return(videosCountText)
}


#' Extracts channel name from given URL
#'
#' @param url A character string of the YouTube channel URL.
#'
#' @return A character string of the channel name.
#' 
#' @examples
#' get_channel_name("https://www.youtube.com/@oggyinformatics/")
#' @export
get_channel_name <- function(url) {
  text <- get_text(paste0(url, '/videos'))
  channelMetadataRenderer <- stringr::str_extract(text, "(?<=channelMetadataRenderer.:.{0,500})title\":\".*\",\"description")
  channelMetadataRenderer <- gsub(".*title\":\"(.*),\"description", "\\1", channelMetadataRenderer)
  channelMetadataRenderer <- stringr::str_replace_all(channelMetadataRenderer, "\"", "")
  return(channelMetadataRenderer)
}