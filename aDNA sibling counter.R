### ### ### ### 
### Code for establishing sibling numbers from human aDNA parent lists 
### associated with paper on "Human monogamy in mammalian context
### ### ### ### 

require(stringr)

## Deposit the aDNA csv datasets in a folder containing only these files and set working directory to that folder 

setwd("~/Library/CloudStorage/OneDrive-UniversityofCambridge/RESEARCH/Sibling Props paper/Human data/aDNA csv peds")
#setwd("C:/Users/md479/OneDrive - University of Cambridge/RESEARCH/Sibling Props paper/Human data/aDNA csv peds")

items <- list.files() # List of the datasets
n_inds = full_sibs = pat_half = mat_half = rep(NA,length(items)) # empty vectors to store information in

for(z in 1:length(items)){ # loop through each dataset
    item = items[z] # pull out the name of the current dataset 
    dataset <- read.csv(item) # import that dataset 
Full = PatH = MatH = 0 # counters for sibling types

for(i in 1:nrow(dataset)){ # iterate through offspring pairs and count sibling freqeuncies 
  for(j in 1:nrow(dataset)){
      if(i!=j){
      if(dataset$Mother[i]==dataset$Mother[j] & dataset$Father[i]==dataset$Father[j]) Full = Full + 1
      if(dataset$Mother[i]==dataset$Mother[j] & dataset$Father[i]!=dataset$Father[j]) MatH = MatH + 1
      if(dataset$Mother[i]!=dataset$Mother[j] & dataset$Father[i]==dataset$Father[j]) PatH = PatH + 1
      }
    }
  }
full_sibs[z] = Full/2 # divide by two as all dyads get counted twice in the above (computationally inefficient but simple)
pat_half[z] = PatH/2
mat_half[z] = MatH/2
}
meta_data = cbind.data.frame(items,full_sibs, pat_half,mat_half)
meta_data

