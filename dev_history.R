library(devtools)

### Do it once ####
# use_git()
# use_gpl_license(version = 3, include_future = TRUE)
# use_r("Allocation_Distance")
# use_r("CH_index_functions")
# use_build_ignore("dev_history.R")
# use_r("CritCF_index_functions")
#### use_r("CVE1_basic")  # ex ne marche pas
#### use_r("CVE2_VandVH")  # ex à faire
#### use_r("CVE3_LinearPred") # ex à faire
# use_r("exctract_center_position")      # ATT nom changé (. en _ ; maj)
#### use_r("glmnet_modified_function")  # ex à faire ?
use_r("Initiate.centers")
use_r("MIClust.mpool")
use_r("MImpute.surv")
use_r("MultiCons")
use_r("my.jack")
use_r("ncvsurv_modified_functions")
use_r("objective_clustering")
use_r("pareto")
use_r("PartitionGeneration")
use_r("seMIsup.cox")
use_r("temp")
use_r("unsupMI")







### Do it often ####
load_all()
document()
use_tidy_description()
attachment::att_amend_desc()
