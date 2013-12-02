doc:
	R --slave -e 'library(devtools); document("pkg")'
	git add pkg/man/*.Rd

test:
	R CMD INSTALL pkg
	R --slave -e 'library(testthat); system.time(test_package("rcrunch", filter="${file}"))'

test-ci:
    rm -rf /var/lib/jenkins/R
    R --slave -e 'install.packages(c("httr", "RJSONIO", "methods", "testthat"), lib="/var/lib/jenkins/R")'
    R CMD INSTALL --library=/var/lib/jenkins/R pkg
	R --slave -e '.libPaths("/var/lib/jenkins/R"); library(testthat); test_package("rcrunch")'