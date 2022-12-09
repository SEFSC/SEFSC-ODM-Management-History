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


