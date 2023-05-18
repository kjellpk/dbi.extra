options(width = 120)
library(dbi.extra)

dbListSchema(chinook)
dbListPrimaryKeys(chinook, Id(table = "Track"))
dbListForeignKeys(chinook, Id(table = "Track"))
