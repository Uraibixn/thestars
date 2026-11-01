library(dplyr) # giving me extra help cuh for filtering and shit
library(jsonlite) # this lib helps me to convert the R data into JSOn and reading JSON files

url  <- "https://raw.githubusercontent.com/astronexus/HYG-Database/main/hyg/CURRENT/hygdata_v41.csv"
file <- "data/raw/hygdata_v41.csv" # where we should save the CSV file


if (!file.exists(file)) { # so we dont have to download again and again the csv file
  download.file(url, file, mode = "wb") # download the file, wb is binary mode
} # returns a boolean value


hyg <- read.csv(file) # load the csv file into R cuh

# the 5 family members
family_members <- c("Merak", "Phecda", "Megrez", "Alioth", "Mizar")


stars <- hyg |> #pass the result along, weird ass symbol
  filter(id != 0, mag <= 8, dist < 100000,) |> # remove garbage data, 
  transmute( # creating new columns and keeping the only columns i need, love this lib ngl
    id, # keep the exsisting id column
    name          = proper, # create a new column called name which uses values from proper
    designation   = ifelse(bayer != "", paste(bayer, con), paste("HYG", id)), 
    constellation = con,
    ra            = ra * 15, #okay this shit is quite confusing ngl, bascially space is measured in hours so. 
    # we gotta do some maths ;-;
    dec,
    dist_ly       = dist * 3.26156,
    mag,
    ci,
    spectral      = spect,
    family        = ifelse(proper %in% family_members, "uma", NA) #checking for the family
  )
  


families <- list(
  uma = list(
    name  = "Ursa Major Moving Group",
    age   = "about 400 million years",
    blurb = "These stars formed together and still move through space side by side."
  )
)

write_json(list(stars = stars, families = families), "data/out/stars.json", auto_unbox = TRUE)

nrow(stars)
stars |> filter(name %in% c("Mizar", "Sirius", "Polaris", "Vega"))