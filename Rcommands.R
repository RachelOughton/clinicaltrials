# work laptop
setwd("C:/Users/d415ax/OneDrive - Durham University/AP2020_21/Teaching/2023/ClinicalTrials4H/Notes/")

# home laptop
setwd("C:/Users/Rachel Oughton/OneDrive - Durham University/AP2020_21/Teaching/2023/ClinicalTrials4H/Notes/")

bookdown::render_book("index.Rmd", "bookdown::gitbook")

### for github:
# home laptop
setwd("C:/Users/Rachel Oughton/GitRepos/clinicaltrials")
# work laptop
setwd("J:/Documents/GitRepos/clinicaltrials")
bookdown::render_book("index.Rmd", "bookdown::gitbook")


