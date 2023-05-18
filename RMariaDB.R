options(width = 150)
library(DBI)
library(sonik)

conn <- dbConnect(
  RMariaDB::MariaDB(),
  host = "relational.fit.cvut.cz",
  port = 3306,
  username = "guest",
  password = "relational",
  dbname = "sakila"
)

x <- setDT(dbGetQuery(conn, "SELECT * FROM information_schema.TABLE_CONSTRAINTS"))
