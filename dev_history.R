library(devtools)

### Do it once ####
# use_git()
# use_r("Allocation_Distance")
# use_r("CH_undex_ifunctions")
# use_build_ignore("dev_history.R")


### Do it often ####
load_all()
document()
use_tidy_description()
attachment::att_amend_desc()
