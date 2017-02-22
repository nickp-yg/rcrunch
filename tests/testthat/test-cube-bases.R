context("'Base sizes' in cubes")

all.dims <- list(
    Admit=c("Admitted", "Rejected"),
    Dept=LETTERS[1:6],
    Gender=c("Male", "Female")
)

arrayify <- function (data, dims) {
    ## dims are names (aliases) of dims defined above
    dn <- all.dims[dims]
    array(data, dim=vapply(dn, length, integer(1), USE.NAMES=FALSE), dimnames=dn)
}

with_mock_HTTP({
    ## Load a ton of cube fixtures via the tab book feature
    ds <- loadDataset("test ds")
    m <- multitables(ds)[[1]]
    with_POST("api/datasets/1/multitables/tabbook-result.json", {
        book1 <- tabBook(m, data=ds, format="json")
    })
    with_POST("api/datasets/1/multitables/tabbook-array-result.json", {
        book2 <- tabBook(m, data=ds, format="json")
    })

    admit.dept <- book1[[1]][[2]]
    b <- arrayify(c(
        601, 332,
        370, 215,
        322, 596,
        269, 523,
        147, 437,
        46, 668), c("Admit", "Dept"))
    test_that("'bases' can be accessed and all margins work", {
        expect_identical(round(admit.dept),
            arrayify(c(
                573, 325,
                367, 210,
                318, 587,
                265, 513,
                151, 434,
                42, 668), c("Admit", "Dept")))
        expect_identical(bases(admit.dept, 0), b)
        expect_identical(bases(admit.dept, 1), margin.table(b, 1))
        expect_identical(bases(admit.dept, 2), margin.table(b, 2))
        expect_identical(bases(admit.dept), sum(b))
    })
    test_that("bases methods exist for TabBookResult and MultitableResult", {
        expect_identical(bases(book1, 0)[[1]][[2]], b)
        expect_identical(bases(book1)[[1]][[2]], sum(b))
    })

    test_that("Cubes can be sliced on 'i' and drop is default", {
        expect_identical(dim(admit.dept), c(2L, 6L))
        expect_is(admit.dept[1,], "CrunchCube")
        expect_identical(dim(admit.dept[1,]), 6L)
        expect_identical(dimnames(admit.dept[1,]), all.dims["Dept"])
        expect_identical(round(admit.dept[1,]),
            arrayify(c(573, 367, 318, 265, 151, 42), "Dept"))
    })
    test_that("Cube slicing with drop=FALSE", {
        expect_identical(dim(admit.dept[1, , drop=FALSE]), c(1L, 6L))
        expect_identical(dimnames(admit.dept[1, , drop=FALSE]),
            c(list(Admit="Admitted"), all.dims["Dept"]))
        expect_identical(round(admit.dept[1, , drop=FALSE]),
            array(c(573, 367, 318, 265, 151, 42),
                dim=c(1L, 6L),
                dimnames=c(list(Admit="Admitted"), all.dims["Dept"])))
    })

    test_that("Cubes can be sliced on 'j'", {
        jslice <- admit.dept[,3]
        expect_is(jslice, "CrunchCube")
        expect_identical(dim(jslice), 2L)
        expect_identical(dimnames(jslice), all.dims["Admit"])
        expect_identical(round(jslice),
            arrayify(c(318, 587), "Admit"))
    })

    test_that("Cube slicing to a single value is no longer array", {
        expect_identical(round(admit.dept[2,3]), 587)
    })
})
