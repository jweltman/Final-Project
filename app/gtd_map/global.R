library(pool)

get_gtddb <- dbPool(
      RMySQL::MySQL(),
      host="gtd-dev.cddrif03m2ft.us-east-2.rds.amazonaws.com",
      port=3306,
      dbname="gtd",
      user="gtdmaster",
      password="gtdmaster"
    )