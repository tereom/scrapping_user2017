library(rslurm)
library(rvest)
library(purrr)
library(dplyr)
library(printr)

videos_urls <- paste0("https://channel9.msdn.com/Events/useR-international-R-User-conferences/useR-International-R-User-2017-Conference?sort=status&direction=desc&page=", 1:16)

get_speakers <- function(talk_content){
    talk_content %>%
        html_node(".authors") %>%
        html_text(trim = TRUE) %>%
        stringr::str_replace(pattern = "by ", "")
}
get_views <- function(talk_content){
    talk_content %>%
        html_nodes(".views .count") %>%
        html_text(trim = TRUE) %>%
        as.numeric()
}
get_length <- function(talk_content){
    talk_content %>%
        html_nodes(".caption") %>%
        html_text(trim = TRUE) 
}
get_released_day <- function(talk_content){
    talk_content %>%
        html_nodes(".releaseDate") %>%
        html_text(trim = TRUE) 
}

get_info_talk <- function(talk_url, title = "No title"){ 
    # print(talk_url)
    talk_content <- read_html(talk_url) 
    views <- get_views(talk_content)
    data_frame(
        title = title,
        speakers = try(get_speakers(talk_content)),
        views = try(ifelse(length(views) > 0, views, NA)),
        day_released = try(get_released_day(talk_content))
    )
}

get_event_data <- function(video_url) {
    # Read the HTML page
    base_url <- "https://channel9.msdn.com"
    content <- read_html(video_url)
    
    # Extract information from the page
    titles <- content %>% 
        html_nodes("h3 a") %>%
        html_text(trim = TRUE)
    
    talks_urls <- content %>% 
        html_nodes("h3 a") %>%
        html_attr("href")
    talks_urls <- paste(base_url, talks_urls, sep = "")
    talks_values <- map2_df(talks_urls, titles, ~get_info_talk(.x, .y))
}
videos_urls_df <- data.frame(video_url = videos_urls, stringsAsFactors = FALSE)

sjob <- slurm_apply(get_event_data, videos_urls_df, jobname = "test_job", 
    nodes = 5, cpus_per_node = 2, slurm_options = list(partition = "optimus"), 
    add_objects = c("get_info_talk", "get_speakers", "get_views", "get_length", "get_released_day"))
print_job_status(sjob)
res_raw <- get_slurm_out(sjob, outtype = "raw", wait = FALSE)
res_raw
videos_views <- Reduce(rbind, res_raw)