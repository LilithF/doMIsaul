library(devtools)

### Do it once ####
# use_git()
# use_gpl_license(version = 3, include_future = TRUE)
# use_r("Allocation_Distance")
# use_r("CH_index_functions")
# use_build_ignore("dev_history.R")
# use_build_ignore("R/temp.R")
# use_r("CritCF_index_functions")
# use_r("CVE1_basic")
# use_r("CVE2_VandVH")
# use_r("CVE3_LinearPred")
# use_r("exctract_center_position")
# use_r("glmnet_modified_function")
# use_r("initiate_centers")
# use_r("MIclust_mpool")
# use_r("MImpute_surv")
# use_r("MultiCons")
# use_r("my_jack")
# use_r("ncvsurv_modified_functions")
# use_r("objective_clustering")
# use_r("pareto")
# use_r("partition_generation")
# use_r("seMIsupcox")
####  use_r("unsupMI")    # Example à faire


# # FOR the CVE need an up to date version of glmnet
# use_package("glmnet", type = "Imports", min_version = T)
# # FOR examples of CVEs
# use_package("survival", type = "Suggests")


# use_github()
# use_testthat()




# use_r("temp")

### Do it often ####
load_all()
document()
use_tidy_description()
attachment::att_amend_desc()


## Moins régulièrement ###
check()
# et si fonctionne :
install()
# build()  # construit le targz
goodpractice::goodpractice()




