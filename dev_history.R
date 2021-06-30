library(devtools)

### Do it once ####
# use_git()
# use_gpl_license(version = 3, include_future = TRUE)
# use_r("Allocation_Distance")
# use_r("CH_index_functions")
# use_build_ignore("dev_history.R")
# use_build_ignore("R/temp.R")
# use_r("CritCF_index_functions")
#### use_r("CVE1_basic")  # ex ne marche pas
#### use_r("CVE2_VandVH")  # ex à faire
#### use_r("CVE3_LinearPred") # ex à faire
# use_r("exctract_center_position")      # ATT nom changé (. en _ ; maj)
#### use_r("glmnet_modified_function")  # ex à faire ?
# use_r("initiate_centers")  # ATT nom changé (. en _ ; maj)
# use_r("MIclust_mpool") # ATT nom changé (. en _ ; maj)
####  use_r("MImpute_surv")   # ATT nom changé (. en _ ; maj)   ++ example a finir qd fonciton de simu
####  use_r("MultiCons")    # EXample à faire
# use_r("my_jack")   # ATT nom changé (. en _)
# use_r("ncvsurv_modified_functions")
# use_r("objective_clustering")
# use_r("pareto")
# use_r("partition_generation")  # ATT nom changé ( _ ; maj)
####  use_r("seMIsupcox")     # Example à faire
# use_r("temp")
####  use_r("unsupMI")    # Example à faire

# use_github()
use_testthat()





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




