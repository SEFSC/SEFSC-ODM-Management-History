# This script is intended to compare the previously ran MH procedure against a static analysis-ready dataset where we know the dates are correct

# The test data to compare to is MH_test_data.Rdata
# Load .Rdata file with test data set
load(here('test', 'MH_test_data.RData'))

# Load .Rdata file with most recent MH analysis ready data set
# Function to select the file with most recent date
latest_file = function(fpattern, fpath) {
  f = list.files(pattern=fpattern, path=fpath, full.names=TRUE)
  f = file.info(f)
  rownames(f)[which.max(f$mtime)] 
}
# File with the most recent internal mtime
myfile = latest_file(fpattern="MH_AL.*RData", fpath=here("data", "processed"))
load(myfile)

# Create most recent analysis-ready dataset for the clusters in the test dataset
mynew_analysis_ready <- mh_analysis_ready %>%
  filter(CLUSTER %in% test$CLUSTER) %>%
  arrange(CLUSTER, ZONE_USE, START_DATE2)

# Compare
librarian::shelf(compareDF)
# Compare by cluster ID
# If you get the error message:
  # Error in stop_or_warn("The two data frames are the same!", stop_on_error) : 
  # The two data frames are the same!
# THAT"S A GOOD THING
result <- compare_df(mynew_analysis_ready, test, c("CLUSTER"))

# Visualize difference
# A single cell is colored if it has changed across the two datasets. 
# The value of the cell in the older dataset is colored red and the value of the cell in the newer dataset is colored green. 
# Cells that haven't changed across the two datasets are colored grey.
create_output_table(result)
