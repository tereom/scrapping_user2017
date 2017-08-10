# Dean Attali
# July 11, 2017
#
# This script scrapes data from the useR 2017 scheduling website to see the
# attendance preferences of conference attendees.
# This code was written in a hurry while on a train, and it is far
# from being "great and robust" scraping code, so beware trying to copy any of
# thid code :)

library(rvest)

base_url <- "https://user2017.sched.com"

# Get all info for a talk from its event URL; return a one-row tibble
get_event_data <- function(event_id) {
    # Read the HTML page
    full_url <- paste0(base_url, "/event/", event_id)
    content <- read_html(full_url)
    
    # Extract information from the page
    title <-
        content %>% html_node(".event a.name") %>%
        html_text(trim = TRUE)
    attendance <-
        content %>% html_node("#sched-page-event-attendees h2") %>%
        html_text(trim = TRUE) %>%
        sub(pattern = ".*\\((.*)\\).*", replacement = "\\1", x = .) %>%
        as.numeric()
    speaker <-
        content %>% html_node(".sched-person h2 a") %>%
        html_text(trim = TRUE)
    time <-
        content %>% html_node(".sched-event-details-timeandplace") %>%
        html_text(trim = TRUE) %>%
        sub(pattern = ".*(July.*) -.*", replacement = "\\1", x = .) %>%
        strptime("%B %d, %Y %I:%M%p", tz = "CET") %>%
        as.POSIXct()
    room <-
        content %>% html_node(".sched-event-details-timeandplace a") %>%
        html_text(trim = TRUE)
    type <-
        content %>% html_node(".sched-event-type a") %>%
        html_text(trim = TRUE)
    
    # Return all event information as a one-row tibble
    tibble::tibble(type = type, title = title, attendance = attendance,
        speaker = speaker, time = time, room = room, url = full_url)
}

# Find all event links from the main page, extract the event ID,
# and scrape each event page
content <- read_html(base_url)
event_ids <- content %>%
    html_nodes(".event a.name") %>%
    html_attr("href") %>%
    sub("/event/(.*)/.*", "\\1", .)
all_talks <- purrr::map_df(event_ids, get_event_data)