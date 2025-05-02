library(tidyverse)
library(RSelenium)
library(janitor)
library(netstat)

rs_driver_object <- rsDriver(
  browser = "firefox",
  chromever = "latest",
  port = free_port()
)

remDr <- rs_driver_object$client