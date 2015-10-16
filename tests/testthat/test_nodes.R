# Need to update authentication upon OAUTH2.0 implementation
context("test nodes functions")

# Implement something along the lines of posting a node and saving the id
# such that it can be deleted during testing
# do not want to delete actual nodes

test_that("get.nodes returns for different account types", {
	expect_that(class(get.nodes(id = NULL))[1], matches('list'))
	expect_that(get.nodes(id = 'me'), throws_error('Requires user'))
	expect_that(get.nodes(id = 'me', user = 'test'), throws_error('Requires password'))
	expect_that(class(get.nodes(id = 'me', user = 'h.schwarzenegger@gmail.com', 'testingtesting')),	matches('list'))
	})

test_that("existence of node is detected", {
	expect_that(get.nodes(id = '12345'), throws_error('Node not found.'))
	expect_that(names(get.nodes(id = 'nu97z')), matches('data'))
	})

test_that("contributors and files extraction functions", {
	expect_that(class(get.nodes('nu97z', contributors = TRUE)), matches('list'))
	expect_that(class(get.nodes('nu97z', files = TRUE)), matches('list'))
	expect_that(class(get.nodes('nu97z', children = TRUE)), matches('list'))
	expect_that(get.nodes('nu97z', contributors = TRUE, files = TRUE), throws_error("Specify contributors OR files OR children"))
	expect_that(get.nodes('nu97z', children = TRUE, files = TRUE), throws_error("Specify contributors OR files OR children"))
	expect_that(get.nodes('nu97z', children = TRUE, contributors = TRUE), throws_error("Specify contributors OR files OR children"))
	expect_that(get.nodes('nu97z', files = TRUE, children = TRUE, contributors = TRUE), throws_error("Specify contributors OR files OR children"))
	})

test_that("post.nodes operates properly", {
	expect_that(class(post.nodes(user = 'h.schwarzenegger@gmail.com',
		 password = 'testingtesting',
		  type = 'nodes',
		   title = 'crap',
		    description = NULL,
		     category = 'project',
		      public = 'true',
		       tags = c("tfadgfdagdf", "tgfhdjkga"))), matches('list'))
	expect_that(class(post.nodes(user = 'h.schwarzenegger@gmail.com',
		 password = 'testingtesting',
		  type = 'nodes',
		   title = 'crap',
		    description = "test")), matches('list'))
		expect_that(class(post.nodes(user = 'h.schwarzenegger@gmail.com',
		 password = 'testingtesting',
		  type = 'nodes',
		   title = 'crap',
		    description = "test", category = "hypothesis")), matches('list'))
		expect_that(class(post.nodes(user = 'h.schwarzenegger@gmail.com',
		 password = 'testingtesting',
		  type = 'nodes',
		   title = 'crap',
		    description = "test", category = "methods and measures")), matches('list'))
		expect_that(class(post.nodes(user = 'h.schwarzenegger@gmail.com',
		 password = 'testingtesting',
		  type = 'nodes',
		   title = 'crap',
		    description = "test", category = "procedure")), matches('list'))
		expect_that(class(post.nodes(user = 'h.schwarzenegger@gmail.com',
		 password = 'testingtesting',
		  type = 'nodes',
		   title = 'crap',
		    description = "test", category = "instrumentation")), matches('list'))
		expect_that(class(post.nodes(user = 'h.schwarzenegger@gmail.com',
		 password = 'testingtesting',
		  type = 'nodes',
		   title = 'crap',
		    description = "test", category = "data")), matches('list'))
		expect_that(class(post.nodes(user = 'h.schwarzenegger@gmail.com',
		 password = 'testingtesting',
		  type = 'nodes',
		   title = 'crap',
		    description = "test", category = "analysis")), matches('list'))
		expect_that(class(post.nodes(user = 'h.schwarzenegger@gmail.com',
		 password = 'testingtesting',
		  type = 'nodes',
		   title = 'crap',
		    description = "test", category = "communication")), matches('list'))
		expect_that(class(post.nodes(user = 'h.schwarzenegger@gmail.com',
		 password = 'testingtesting',
		  type = 'nodes',
		   title = 'crap',
		    description = "test", category = "other")), matches('list'))
		expect_that(post.nodes(user = 'h.schwarzenegger@gmail.com',
		 password = 'testingtesting',
		  type = 'nodes',
		   title = 'crap',
		    description = "test", category = "hypofdthesis"), throws_error("Please input proper category, see documentation"))
	})

