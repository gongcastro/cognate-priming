test_vocabulary <- function(vocabulary){
	
	test_that("vocabulary has the right columns", {
		expect_equal(
			colnames(vocabulary),
			c(
				"participant",
				"age_group",
				"lp",
				"vocab_size_total",
				"vocab_size_l1",
				"vocab_size_conceptual",
				"is_imputed"
			)
		)
	})
	
	test_that("vocabulary has the variable classes", {
		expect_type(vocabulary$participant, "character")
		expect_equal(class(vocabulary$age_group), "factor")
		expect_equal(class(vocabulary$lp), "factor")
		expect_type(vocabulary$vocab_size_total, "double")
		expect_type(vocabulary$vocab_size_l1, "double")
		expect_type(vocabulary$vocab_size_conceptual, "double")
		expect_type(vocabulary$is_imputed, "logical")
	})
	
	test_that("vocabulary missing data is dealt with", {
		expect_false(any(is.na(vocabulary$participant)))
		expect_false(any(is.na(vocabulary$age_group)))
		expect_false(any(is.na(vocabulary$lp)))
		expect_false(any(is.na(vocabulary$vocab_size_total)))
		expect_false(any(is.na(vocabulary$vocab_size_l1)))
		expect_false(any(is.na(vocabulary$vocab_size_conceptual)))
		expect_false(any(is.na(vocabulary$is_imputed)))
		
	})
	
	test_that("stimuli variables have the right values", {
		expect_equal(levels(vocabulary$age_group), paste0(c(21, 25, 30), " months"))
		expect_equal(levels(vocabulary$lp), c("Monolingual", "Bilingual"))
		expect_true(all(between(unique(vocabulary$vocab_size_total), 0, 1)))
		expect_true(all(between(unique(vocabulary$vocab_size_l1), 0, 1)))
		expect_true(all(between(unique(vocabulary$vocab_size_conceptual), 0, 1)))
		expect_true(all(unique(vocabulary$is_imputed) %in% c(TRUE, FALSE)))
	})
	
}
