### ### ### ### 
### Code for establishing reproductive exclusivity
### associated with paper on "Human monogamy in mammalian context
### ### ### ### 

par(mfrow = c(1, 1))
require(stringr)

### 1) First for ethnographic 

# set working directory to folder with the ethnographic parentage datasets in 

items <- list.files() # List of the datasets
n_inds = exclusive_inds = non_excl_inds = rep(NA,length(items)) # empty vectors to store information in

for(z in 1:length(items)){ # loop through each dataset
  item = items[z] # pull out the name of the current dataset 
  dataset <- read.csv(item) # important that dataset 
  dataset = dataset[which(dataset$Children!=""),] # look at rows containing info about offspring
  ID = Father = Mother = integer() # zero length vectors to add info to 
  for(i in 1:nrow(dataset)){ # go through all rows of dataset
    n_kids = str_count(dataset$Children[i], ";")+1 # count number of offspring listed in row
    ID = c(ID,str_split_fixed(dataset$Children[i], ";",n_kids)) # get a list of the offspring 
    Father = c(Father,rep(dataset$Father[i],times=n_kids)) # record the father of each offspring 
    Mother = c(Mother,rep(dataset$MotherId[i],times=n_kids)) # record the mother of each offspring 
  }
  n_inds[z] = length(ID) # note the number of offspring included in the dataset
  include = which(Father!="0" & Mother!="0");ID = ID[include]; Father = Father[include]; Mother = Mother[include] # include offspring with known parents (unkown is given as '0' in these datasets)
  mothers = unique(Mother) # list of the unique mother identities from the dataset
  fathers = unique(Father) # list of the unique father identities from the dataset
  excl = nonexcl = 0 # set up counters

  for(i in 1:length(mothers)){ # loop through mothers
    partners = unique(dataset$Father[which(dataset$Mother==mothers[i])]) # what reporudctive partners did this individual have?
    n_partners = length(partners) # how many unique ones?
    n_partners_of_partner = length(unique(dataset$Mother[which(dataset$Father%in%partners)])) # how many repord. partners of partners?
    if(n_partners==1 & n_partners_of_partner==1) excl = excl+1 else nonexcl=nonexcl+1 # count if exclusive or not
  }
  
  for(i in 1:length(fathers)){
    partners = unique(dataset$Mother[which(dataset$Father==fathers[i])]) # what reporudctive partners did this individual have?
    n_partners = length(partners) # how many unique ones?
    n_partners_of_partner = length(unique(dataset$Father[which(dataset$Mother%in%partners)])) # how many repord. partners of partners?
    if(n_partners==1 & n_partners_of_partner==1) excl = excl+1 else nonexcl=nonexcl+1 # count if exclusive or not
  }
  exclusive_inds[z] = excl # record numbers
  non_excl_inds[z] = nonexcl
}

meta_data_kinsources = cbind.data.frame(items,exclusive_inds,non_excl_inds) # combine data
meta_data_kinsources # take a look




### 2) for aDNA

# set working directory to folder with the aDNA parentage datasets in 

items <- list.files() # List of the datasets
n_inds = exclusive_inds = non_excl_inds = rep(NA,length(items)) # empty vectors to store information in

for(z in 1:length(items)){ # loop through each dataset
  item = items[z] # pull out the name of the current dataset 
  dataset <- read.csv(item) # import that dataset 
  
  mothers = unique(dataset$Mother)
  fathers = unique(dataset$Father)
  
  ## Only include those in the sample if they are in the ID column too (and therefore were actually sampled rather than genetically inferred)
  mothers = mothers[which(mothers%in%dataset$ID)]
  fathers = fathers[which(fathers%in%dataset$ID)]
  
  excl = nonexcl = 0
  
  for(i in 1:length(mothers)){
  partners = unique(dataset$Father[which(dataset$Mother==mothers[i])])
  n_partners = length(partners)
  n_partners_of_partner = length(unique(dataset$Mother[which(dataset$Father%in%partners)]))
  if(n_partners==1 & n_partners_of_partner==1) excl = excl+1 else nonexcl=nonexcl+1 
  }
  
  for(i in 1:length(fathers)){
    partners = unique(dataset$Mother[which(dataset$Father==fathers[i])])
    n_partners = length(partners)
    n_partners_of_partner = length(unique(dataset$Father[which(dataset$Mother%in%partners)]))
    if(n_partners==1 & n_partners_of_partner==1) excl = excl+1 else nonexcl=nonexcl+1 
  }
  exclusive_inds[z] = excl
  non_excl_inds[z] = nonexcl
}
meta_data_aDNA = cbind.data.frame(items,exclusive_inds,non_excl_inds)
meta_data_aDNA

## Only include ipopulations with >10 individuals included 
meta_data_aDNA = meta_data_aDNA[which(exclusive_inds+non_excl_inds>10),]




### 3) for mammals
# set working directory to folder with the aDNA parentage datasets in 

items <- list.files() # List of the datasets
items = items[-which(items=="Pan trog Budongo.csv")] # want one per species so exlc chimp popns
items = items[-which(items=="Pan trog Gombe.csv")] # want one per species so exlc chimp popns

n_inds = male_one = male_twoplus = female_one = female_twoplus = rep(NA,length(items)) # empty vectors to store information in
n_inds = exclusive_inds = non_excl_inds = rep(NA,length(items)) # empty vectors to store information in

for(z in 1:length(items)){ # loop through each dataset
  item = items[z] # pull out the name of the current dataset 
  dataset <- read.csv(item) # import that dataset 
  mothers = unique(dataset$Mother)
  fathers = unique(dataset$Father)
  excl = nonexcl = 0
  for(i in 1:length(mothers)){
    partners = unique(dataset$Father[which(dataset$Mother==mothers[i])])
    n_partners = length(partners)
    n_partners_of_partner = length(unique(dataset$Mother[which(dataset$Father%in%partners)]))
    if(n_partners==1 & n_partners_of_partner==1) excl = excl+1 else nonexcl=nonexcl+1 
  }
  for(i in 1:length(fathers)){
    partners = unique(dataset$Mother[which(dataset$Father==fathers[i])])
    n_partners = length(partners)
    n_partners_of_partner = length(unique(dataset$Father[which(dataset$Mother%in%partners)]))
    if(n_partners==1 & n_partners_of_partner==1) excl = excl+1 else nonexcl=nonexcl+1 
  }
  exclusive_inds[z] = excl
  non_excl_inds[z] = nonexcl
}
meta_data_mammals = cbind.data.frame(items,exclusive_inds,non_excl_inds)
meta_data_mammals

items # check index for monogamous species
items[c(2,3,7,16,19)]
meta_data_mammals_monog = meta_data_mammals[c(2,3,7,16,19),] # split monogamous and non-monog species
meta_data_mammals_nonmonog = meta_data_mammals[-c(2,3,7,16,19),]


####
ethno_props = (meta_data_kinsources$exclusive_inds/(meta_data_kinsources$exclusive_inds+meta_data_kinsources$non_excl_inds))*100
aDNA_props = (meta_data_aDNA$exclusive_inds/(meta_data_aDNA$exclusive_inds+meta_data_aDNA$non_excl_inds))*100
mono_mammal_props = (meta_data_mammals_monog$exclusive_inds/(meta_data_mammals_monog$exclusive_inds+meta_data_mammals_monog$non_excl_inds))*100
nonmono_mammal_props = (meta_data_mammals_nonmonog$exclusive_inds/(meta_data_mammals_nonmonog$exclusive_inds+meta_data_mammals_nonmonog$non_excl_inds))*100

## summarise
mean(ethno_props);length(ethno_props)
mean(aDNA_props);length(aDNA_props)
mean(mono_mammal_props);length(mono_mammal_props)
mean(nonmono_mammal_props);length(nonmono_mammal_props)

###### END ####################

