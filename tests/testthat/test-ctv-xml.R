xml_fixture <- paste(
  '<CRANTaskView>',
  '  <name>Spatial</name>',
  '  <topic>Analysis of Spatial Data &amp; Maps</topic>',
  '  <maintainer email="Roger.Bivand@nhh.no">Roger Bivand</maintainer>',
  '  <version>2015-10-12</version>',
  '  <info>',
  '    <p>The <pkg>sp</pkg> package and <pkg>notlisted</pkg> in prose.</p>',
  '  </info>',
  '  <packagelist>',
  '    <pkg priority="core">sp</pkg>',
  '    <pkg>rgdal</pkg>',
  "    <pkg priority='core'>maptools</pkg>",
  '    <pkg> spdep </pkg>',
  '  </packagelist>',
  '  <links>',
  '    <view>Environmetrics</view>',
  '  </links>',
  '</CRANTaskView>', sep = "\n")

test_that("membership comes from <packagelist> only, with core from priority", {
  m <- parse_ctv_xml_membership(xml_fixture)
  expect_equal(sort(names(m)), c("maptools", "rgdal", "sp", "spdep"))
  expect_equal(m[["sp"]], 1L)
  expect_equal(m[["maptools"]], 1L)
  expect_equal(m[["rgdal"]], 0L)
  expect_false("notlisted" %in% names(m))   # a mention in <info> is not membership
})

test_that("a file with no packagelist, or an empty one, has no members", {
  expect_equal(parse_ctv_xml_membership("<CRANTaskView><info/></CRANTaskView>"), integer(0))
  expect_equal(parse_ctv_xml_membership("<packagelist>\n</packagelist>"), integer(0))
  expect_equal(parse_ctv_xml_membership(""), integer(0))
})

test_that("a package listed twice is core if either listing says so", {
  m <- parse_ctv_xml_membership('<packagelist><pkg>a</pkg><pkg priority="core">a</pkg></packagelist>')
  expect_equal(m, c(a = 1L))
})

test_that("the header gives topic, maintainer and version, with entities decoded", {
  h <- parse_ctv_xml_header(xml_fixture)
  expect_equal(h$topic, "Analysis of Spatial Data & Maps")
  expect_equal(h$maintainer, "Roger Bivand")
  expect_equal(h$updated, "2015-10-12")
})

test_that("missing header fields are NA, as in the 2005 files with no <version>", {
  h <- parse_ctv_xml_header("<CRANTaskView><name>Econometrics</name><topic>Econometrics</topic></CRANTaskView>")
  expect_equal(h$topic, "Econometrics")
  expect_true(is.na(h$maintainer))
  expect_true(is.na(h$updated))
})
