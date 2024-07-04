# testing stuff
casmo.out <- "/home/mjf/Projects/generic_git_repo/casmo.inp"


# casmo2openmc
# ============

# package management
library(stringr)

# command-line flags
args <- commandArgs()

# help text
help_text <- character()
help_text[1]  <- "Print to stdout the CASMO compositions and geometries in OpenMC format."
help_text[2]  <- "Usage: Rscript casmo2openmc.R [OPTION] file"
help_text[3]  <- "Example: Rscript casmo2openmc.R --thermal-expansion /path/to/CASMO/out/file.out"
help_text[4]  <- ""
help_text[5]  <- "Options:"
help_text[6]  <- "  -t, --thermal-expansion      Account for thermal expansion by altering the densities of the materials (openmc.Material.set_density)"
help_text[7]  <- "                               and the radii of the mesh (openmc.ZCylinder). This follows the methodology as outlined in CASMO5 THE card."
help_text[8]  <- "  -h, --help                   This menu"

# print help only
if (TRUE %in% (str_detect(args, "--help") | str_detect(args, "-h"))) {
  writeLines(help_text)
}

# check for CASMO argument and file
casmo.out <- args[length(args)]
if (!file.exists(casmo.out)) {
  stop(paste("Error:", casmo.out, "doesn't exist!"))
}

# thermal expansion flag is FALSE, TRUE if specified in the command line
thermal_expansion_flag <- FALSE
if (TRUE %in% (str_detect(args, "--thermal-expansion") | str_detect(args, "-t"))) thermal_expansion_flag <- TRUE

# read file
lines <- readLines(casmo.out)
page <- grep("1_________________________", lines) # the lines between these strings are referred to as pages
bop <- page[-length(page)] # beginning of page
eop <- page[-1] # end of page


#################
# Geometry Data #
#################

# find page(s) with PIN data
for (i in 1:length(bop)) {
  page <- lines[bop[1]:eop[i]] # 
  # determine page of interest with some flags
  all_data_flag <- TRUE %in% str_detect(page, "\\*  All_data_on_file") # end of input file echo
  if (all_data_flag) break # leave for loop once page of interest is found
}
writeLines("**** Geometry Specification Unavailable ****")
writeLines("**** Thermal Expansion Calc Unavailable ****")
writeLines("****       Input File Echo Below        ****")
writeLines(page)
writeLines("")



####################
# Composition Data #
####################

# find page with composition data
for (i in 1:length(bop)) {
  page <- lines[bop[i]:eop[i]]
  # determine page of interest with some flags
  burnup_flag <- TRUE %in% str_detect(page, " * Burnup =     0.000") # beginning of life (BOL), zero-burnup
  ie_flag <- TRUE %in% str_detect(page, " Input Echo") # "Input Echo"
  h1_flag <- TRUE %in% str_detect(page, "Density  Th.Exp.     Temp   Weight") # header 1
  h2_flag <- TRUE %in% str_detect(page, "Tot Wt%  Nuclide Wt%") # header 2
  if (burnup_flag & ie_flag & h1_flag & h2_flag) break # leave for loop once page of interest is found
}

# loop through compositions
n_compositions <- as.numeric(str_split(page[length(page)-1], ":")[[1]][2]) # number of compositions
den_units <- str_sub(page[6], 11, 15) # units of density
page <- page[8:(length(page)-3)] # remove preamble lines, and a few lines at the end
bom <- which(!grepl("                   ", page)) # beginning of material .... doesn't contain lots of consecutive spaces
eom <- c(bom[-1] - 1, length(page)) # end of material
i <- 1; j <- 1 # testing stuff
for (i in 1:n_compositions) {
  data <- page[bom[i]:eom[i]] # material data
  data1 <- data[1] # first line of material data
  name <- str_sub(data1, 2, 4) # material name
  id <- str_trim(str_sub(data1, 5, 9)) # material ID, if applicable
  data1 <- str_split(str_sub(data1, 10, 52), "\\s+") # split string from density to total weight
  data1 <- unlist(data1)[nzchar(unlist(data1))] # numeric values (no empty strings "") per vector element
  den <- as.numeric(data1[1]) # density
  thex <- as.numeric(data1[2]) # thermal expansion coefficient
  temp <- as.numeric(data1[3]) # temperature
  wt <- as.numeric(data1[4]) # weight
  totwt <- as.numeric(data1[5]) # total weight
  nuclide <- character()
  wtpc <- numeric()
  for (j in 1:length(data)) {
    s <- str_split(str_remove_all(str_sub(data[j], 55), "="), "\\s+")[[1]] # split string of nuclide data into isotope and wt%
    nuclide <- c(nuclide, s[1:length(s) %% 2 != 0]) # nuclide name .... odd elements in vector
    wtpc <- c(wtpc, as.numeric(s[1:length(s) %% 2 == 0])) # nuclide wt% .... even elements in vector
  }
  
  # CASMO to OpenMC nuclides
  nuclide <- str_replace(nuclide, "CR", "") # remove control rod identifier at the end of nuclide
  # nuclide <- str_replace(nuclide, "[other unwanted pattern]", "")
  # add dash between element and atomic number
  without_dash <- which(!str_detect(nuclide, "-"))
  first_digit <- as.numeric(str_locate(nuclide[without_dash], "[:digit:]")[,1])
  nuclide[without_dash] <- paste0(str_sub(nuclide[without_dash], 1, first_digit - 1), "-", str_sub(nuclide[without_dash], first_digit))

  # thermal expansion's effects on density
  if (thermal_expansion_flag) den <- den * 1
  
  # write openmc.Material() code
  writeLines(paste("# CASMO composition:", name, id)) # introductory comment
  writeLines(paste0(name, id, " = openmc.Material(name='", name, id, "', temperature=", temp, ")")) # assign openmc.Material
  writeLines(paste0(name, id, ".set_density(units=\"", den_units,"\"", ", density=", den, ")")) # density
  writeLines(paste0(name, id, ".add_nuclide(nuclide=\"", nuclide, "\", percent=", wtpc, ", percent_type='wo')"))
  writeLines("") # empty line
}
