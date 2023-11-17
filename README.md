# About RESPIRE

RESPIRE is a system designed to make diverse data available through a single application. To accomplish this, RESPIRE uses a modular approach in which each data type is served by a REST API registered with a central registry API.

`respireAdmin` is provided as an interface to the underlying data, intended to simply routine data ingestion tasks.

# About the `respireModule` class

The `respireModule` class takes an API stub and produces an object that is used in a similar fashion to a database connection. When provided to a `respireAdmin` function, it leverages metadata from required REST endpoints to facilitate the validation and ingestion of data sets.

# Process for ingesting new data or metadata

1. Install the `respireAdmin` package
2. Get database credentials from your database administrator and set them using `set_db_credentials` (Use `?set_db_credentials` to see documentation)
3. Get the API stub for the RESPIRE module you will add data or metadata to.
4. In an R script, load `respireAdmin` and create a new `respireModule` instance using `create_module('my/api/stub/here')`. Assign the result to an object in memory.
5. Retrieve and process your data. This is a necessarily bespoke process that will be unique to each type of data. To see the data structure required by the system, either print your `respireModule` object or use `create_templates(myModuleHere)`
6. When you have data that matches the specification from the API, use `write_data()` and `write_metadata()` respectively to add the data to the database.
7. If applicable, run `populate_has_data()`. This will update the `has_data` field of the study metadata, if present, for all studies now present in the data table.

```
# create a module object. This will tell other `respireAdmin` functions where to write data and what shape that data should be
myModule <- create_module('https://my-api.here/v1')

# read data from a file, or create it in memory
# For a tutorial on reading data: https://www.datacamp.com/tutorial/r-data-import-tutorial

my_data <- readr::read_csv("path/to/my/data.csv")

# you may also have study or sample metadata
# read this into memory, if applicable

# you will need to connect to a database to store data
# get your credentials from your database administrator and set them using `set_db_credentials` (Use `?set_db_credentials` to see documentation)

set_db_credentials(...)

# write the data to the database
# use ?write_data to view documentation and available options
# by default, write_data() will append the data to the table
# you may need to do some basic data processing to ensure data types in `my_data` match the specification from the API
write_data(myModule, my_data)

# if you have study or sample metadata, use `write_metadata()` to add that data to the database. 
# use ?write_metadata to view documentation and available options

```
