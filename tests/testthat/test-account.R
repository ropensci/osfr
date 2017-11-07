context('account operations')

test_that('login works', {
  expect_is(login(), 'character') # make sure login works when PAT is in env
  expect_is(login(pat = Sys.getenv('OSF_PAT')), 'character')
  expect_is(login(), 'character')
})

# test_that('welcome works', {
#   expect_null(welcome())
# })

# test_that('logout works', {
#   expect_true(logout())
# })
