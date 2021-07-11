library(devtools)
library(testthat)
### Do it often ####
document()
load_all()
use_tidy_description()
# attachment::att_amend_desc()

spell_check()

## Moins régulièrement ###
check()
test()
# And if it works:
install()
# build()  # construit le targz
goodpractice::goodpractice()
covr::package_coverage()
### Do it once ####
# use_git()
# use_gpl_license(version = 3, include_future = TRUE)
# use_r("Allocation_Distance")
# use_r("CH_index_functions")
# use_build_ignore("dev_history.R")
# use_build_ignore("R/temp.R")
# use_build_ignore("temp.R")
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
# use_r("unsupMI")
# # use_r("mice_imputers_censored")
# # use_r("evaluate_partition_semisup")
# # use_r("evaluate_partition_unsup")
# # use_r("Extract_AUC")
# use_r("plot_boxplot")
# use_r("plot_frequency")
# use_r("plot_MIpca")
# use_r("table_continous")
# use_r("chi2tab")
# use_r("formatpv")
# use_r("anovatab")
# use_r("table_categorical")
# use_r("cleanUp_partition")
# use_r("Investigate_unclass") # A faire + tard



# # FOR the CVE need an up to date version of glmnet
# use_package("glmnet", type = "Imports", min_version = T)


# use_github()
### TEST ####
# use_testthat()
# use_test("unsupMI")
# use_test("seMIsupcox")
# use_test("CH_index_functions")
# use_test("output_tables")
# use_test("CVE3_LinearPred")
# use_test("other")

# use_logo("../hex/doMIsaul_ihex.PNG")
# use_lifecycle_badge("stable")
# badger::badge_last_commit()
# use_vignette("unsupMI_semisup_cox")
# use_github_action_check_standard()

# use_spell_check()
# use_readme_rmd()
# use_r("temp")





