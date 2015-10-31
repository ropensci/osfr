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
	expect_that(names(get.nodes(id = 'jqdn2')), matches('data'))
	})

test_that("contributors and files extraction functions", {
	expect_that(class(get.nodes('jqdn2', contributors = TRUE)), matches('list'))
	expect_that(class(get.nodes('jqdn2', files = TRUE)), matches('list'))
	expect_that(class(get.nodes('jqdn2', children = TRUE)), matches('list'))
	expect_that(get.nodes('jqdn2', contributors = TRUE, files = TRUE), throws_error("Specify contributors OR files OR children"))
	expect_that(get.nodes('jqdn2', children = TRUE, files = TRUE), throws_error("Specify contributors OR files OR children"))
	expect_that(get.nodes('jqdn2', children = TRUE, contributors = TRUE), throws_error("Specify contributors OR files OR children"))
	expect_that(get.nodes('jqdn2', files = TRUE, children = TRUE, contributors = TRUE), throws_error("Specify contributors OR files OR children"))
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
		expect_that(post.nodes(user  ='h.schwarzenegger@gmail.com',
		 password = 'testingtesting',
		  type = 'nodes',
		   title = 'crap',
		    description = "test", category = "hypofdthesis"), throws_error("Please input proper category, see documentation"))
	})

test_that("get.nodes.contributors works properly", {
	expect_that(class(get.nodes.contributors('jqdn2', 'tyhc6')), matches('list'))
	expect_that(class(get.nodes.contributors(node_id = '3hu4n', user_id = 'tyhc6', user = 'h.schwarzenegger@gmail.com', password = 'testingtesting')), matches('list'))
	})

test_that("put.nodes operates properly", {
	expect_that(class(put.nodes(id = "jqdn2", user = 'h.schwarzenegger@gmail.com',
		 password = 'testingtesting',
		  type = 'nodes',
		   title = 'From tests',
		    description = NULL,
		     category = 'project',
		      public = 'true',
		       tags = c("tfadgfdagdf", "tgfhdjkga"))), matches('list'))
	expect_that(put.nodes(id = "jqdn2", user = 'h.schwarzenegger@gmail.com',
		 password = 'testingtesting',
		  type = 'nodes',
		   title = 'From tests',
		    description = "test", category = "hypofdthesis"), throws_error("Please input proper category, see documentation"))
	})

test_that("patch.nodes operates properly", {
	expect_that(class(patch.nodes(id = "jqdn2", user = 'h.schwarzenegger@gmail.com',
		 password = 'testingtesting',
		  type = 'nodes',
		   title = 'From tests',
		    description = NULL,
		     category = 'project',
		      public = 'true',
		       tags = c("tfadgfdagdf", "tgfhdjkga"))), matches('list'))
	expect_that(patch.nodes(id = "jqdn2", user = 'h.schwarzenegger@gmail.com',
		 password = 'testingtesting',
		  type = 'nodes',
		   title = 'From tests',
		    description = "test", category = "hypofdthesis"), throws_error("Please input proper category, see documentation"))
	})

test_that("nodes are deleted properly", {
	x = get.users(id = 'me', user = 'h.schwarzenegger@gmail.com', password = 'testingtesting')
	y = rjson::fromJSON(httr::content(httr::GET(x$data$relationships$nodes$links$related$href), 'text'))
	for (i in 1:5){
		if (y$data[[i]]$id == "jqdn2"){
			expect_that(delete.nodes(id = y$data[[i]]$id, user = 'h.schwarzenegger@gmail.com', password = 'testingtesting'),
				is_false())
		}
		expect_that(delete.nodes(id = y$data[[i]]$id, user = 'h.schwarzenegger@gmail.com', password = 'testingtesting'),
				is_true())}
	})