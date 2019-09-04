library(influxdbrv2)
library(purrr)
library(forecast)
db = "aiops"

con <- influx_connection(scheme = "http",
                         #host = "10.1.72.1",
                         #port = 9005,
                         host = "183.129.199.122",
                         port = 33964,
                         user = "admin",
                         pass = "admin",
                         path = "/",
                         verbose = FALSE)

input <- influx_query(con,
                      db = db,
                      query = "select mean(value) as y from raw_data where time > '2019-07-05' and time < '2019-07-22' group by time(5m),* fill(linear)",
                      timestamp_format = c("s"),
                      return_xts = FALSE,
                      chunked = FALSE, 
                      simplifyList = FALSE)
input2 <- xts_to_tibble(input, timestamp_format="s")
input2 <- as.data.frame(input2)
nb_ets(data = input2,freq=288,predlenth=576,force_positive=TRUE)

