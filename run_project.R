PATH = ""                 # Path to folder containing the main file
DATAPATH = "R2022--DATA"  # Path to folder containing data files
keywords_to_display = 20  # Number of most cited keywords to display
timing = TRUE             # Shows the time each section takes to complete


# Expected run time < 2mn30s on my toaster

source(paste(PATH, "functions.R", sep=""))
main(PATH = DATAPATH, keywords_to_display = keywords_to_display, timing=timing)

# Warning messages are due to NA's introduced the "noteworthy_names" column. 
# No data has been lost.