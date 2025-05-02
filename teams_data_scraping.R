library(tidyverse)
library(RSelenium)
library(netstat)
library(janitor)

rs_driver_object <- rsDriver(
  browser = "firefox",
  chromever = "latest",
  port = free_port()
)

remDr <- rs_driver_object$client

## remDr$open()

remDr$navigate("https://www.playhq.com/afl")

search_bar <- remDr$findElement(using = "id", "query")

search_bar$sendKeysToElement(list("(SMFJL)", key = "enter"))

get_deets <- function(x) {
  c(
    x$getElementText(),
    x$getElementAttribute("href")
  )
}

club_list <- tibble()

clubs <- remDr$findElements(using = "class", "hFbHax") %>%
  sapply(get_deets) %>%
  unlist() %>%
  matrix(ncol = 2, byrow = TRUE) %>%
  as_tibble() %>%
  set_names(c("club", "link")) %>%
  mutate(
    club = str_remove_all(club, "^AFL\\n|\\n.*$"),
  )

club_list <- club_list %>%
  bind_rows(clubs)

## Click "Next Page" link
remDr$findElement(using = "xpath", '//*[@data-testid = "page-next"]')$clickElement()


## Filter out SMJFL itself
club_list <- club_list %>%
  filter(!str_detect(club, "Metro"))

get_teams <- function(club, link) {
  
remDr$navigate(link)

}

## Navigate to club page, select "Completed" seasons and create another table of links
## for each year

get_season_list <- function(club,link) {
  
  remDr$navigate(link)
  
  remDr$findElement(using = "css", 'button.sc-kAyceB:nth-child(3)')$clickElement()
  
  seasons <- remDr$findElements(using = "class", "dImJDh") %>%
    sapply(function(x) {
      c(
        x$getElementText(),
        x$getElementAttribute("href")
      )
    }) %>%
    unlist() %>%
    matrix(ncol = 2, byrow = TRUE) %>%
    as_tibble() %>%
    separate(col = V1,
             into = c("year", "season_dates", "status", "select"),
             sep = "\\n") %>%
    rename(link = V2) %>%
    select(-select) %>%
    mutate(
      club = club
    )
  
  return(seasons)
  
}

# ## Filter out any non SMJFL club links
# club_list <- club_list %>%
#   filter(str_detect(club, "\\(SMJFL\\)"))

club_season_list <- map2_dfr(club_list$club, club_list$link, get_season_list)


## Retrieve tibble of team names for year once navigated

get_teams <- function(club, year, link) {

    remDr$navigate(link)
    
    remDr$findElement(using = 'xpath', '//ul[@data-testid="teams-list"]')$getElementText() %>%
      str_split("\n") %>%
      unlist(.[1]) %>%
      str_subset("Select", negate = TRUE) %>%
      matrix(ncol = 4, byrow = TRUE) %>%
      row_to_names(row_number = 1) %>%
      as_tibble() %>%
      clean_names() %>%
      mutate(
        club = club,
        year = year
      )
}

club_season_list <- club_season_list %>%
  filter(!str_detect(club, "Metro") & !str_detect(season_dates, "Jun"))

all_clubs_teams <- pmap_dfr(list(club_season_list$club,
                         club_season_list$year,
                         club_season_list$link), 
                    get_teams)

rs_driver_object$server$stop()
remDr$close()


all_clubs_teams %>% 
  group_by(club, year, gender) %>% 
  tally() %>%
  spread(year, n) %>% 
  View()

all_clubs_teams %>% 
  mutate(age_group = factor(age_group, 
                          levels = c("U8", "U9", "U10", "U11", "U12", "U13", "U14", "U15", "U16", "U17", "U18") )) %>% 
  filter(gender != "Girls") %>% 
  group_by(club, year) %>% 
  tally() %>%
  pivot_wider(names_from = year, values_from = n) %>% 
  arrange(desc(`2025`)) %>%
  print(n = Inf)

all_clubs_teams %>% 
  group_by(year) %>% 
  tally()

all_clubs_teams %>% 
  group_by(year, gender) %>% 
  tally() %>% 
  pivot_wider(names_from = gender, values_from = n) %>% 
  mutate(Total = Boys + Girls + Mixed)

all_clubs_teams %>% 
  mutate(age_group = factor(age_group, 
                            levels = c("U8", "U9", "U10", "U11", "U12", "U13", "U14", "U15", "U16", "U17", "U18") )) %>% 
  filter(year == "2025") %>% 
  group_by(club, age_group) %>% 
  tally() %>%
  pivot_wider(names_from = age_group, values_from = n, values_fill = 0) %>%
  mutate(Total = rowSums(across(U8:U18), na.rm = TRUE)) %>%
  arrange(desc(Total)) %>% 
  print(n = Inf)
