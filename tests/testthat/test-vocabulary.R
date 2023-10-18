test_vocabulary <- function(vocabulary){
	
	test_that("vocabulary has the right columns", {
		expect_setequal(
			colnames(vocabulary),
			c("filename",
			  "total_prop",
			  "l1_prop",
			  "l2_prop",
			  "concept_prop",
			  "te_prop",
			  "total_contents",
			  "l1_contents",
			  "l2_contents",
			  "concept_contents",
			  "te_contents",
			  "is_imputed"
			))
	})
	
	test_that("vocabulary has the variable classes", {
		expect_type(vocabulary$filename, "character")
		expect_type(vocabulary$total_prop, "double")
		expect_type(vocabulary$l1_prop, "double")
		expect_type(vocabulary$l2_prop, "double")
		expect_type(vocabulary$concept_prop, "double")
		expect_type(vocabulary$te_prop, "double")
		expect_type(vocabulary$total_contents, "list")
		expect_type(vocabulary$l1_contents, "list")
		expect_type(vocabulary$l2_contents, "list")
		expect_type(vocabulary$concept_contents, "list")
		expect_type(vocabulary$te_contents, "list")
		expect_type(vocabulary$is_imputed, "logical")
		
		
	})
	
	test_that("vocabulary missing data is dealt with", {
		expect_false(any(is.na(vocabulary$filename)))
		expect_false(any(is.na(vocabulary$total_prop)))
		expect_false(any(is.na(vocabulary$l1_prop)))
		expect_false(any(is.na(vocabulary$l2_prop)))
		expect_false(any(is.na(vocabulary$concept_prop)))
		expect_false(any(is.na(vocabulary$te_prop)))
		expect_false(any(is.na(vocabulary$is_imputed)))
		
	})
	
	test_that("stimuli variables have the right values", {
		expect_true(all(between(unique(vocabulary$total_prop), 0, 1)))
		expect_true(all(between(unique(vocabulary$l1_prop), 0, 1)))
		expect_true(all(between(unique(vocabulary$l2_prop), 0, 1)))
		expect_true(all(between(unique(vocabulary$concept_prop), 0, 1)))
		expect_true(all(between(unique(vocabulary$te_prop), 0, 1)))
		expect_true(all(unique(vocabulary$is_imputed) %in% c(TRUE, FALSE)))
	})
	
}

