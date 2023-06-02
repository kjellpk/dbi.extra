options(width = 120)
library(dbi.extra)

CS <- "DRIVER={ODBC Driver 17 for SQL Server}; DATABASE=UWINCO_DEV; Trusted_Connection=Yes; SERVER=invprod2.s.uw.edu"
dev <- dbConnect(odbc::odbc(), .connection_string = CS)

dbListSchema(dev, prefix = Id(catalog = "UWINCO_DEV", schema = "dbo"))
dbListPrimaryKeys(dev, Id(table = "SECURITY_IDENTIFIER"))
dbListForeignKeys(dev, Id(table = "SECURITY_IDENTIFIER"))

dbListSchema(chinook)
dbListPrimaryKeys(chinook, Id(table = "InvoiceLine"))
dbListForeignKeys(chinook, Id(table = "InvoiceLine"))


