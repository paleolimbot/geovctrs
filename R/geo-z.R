
geo_has_z <- function(x) {

}

geo_set_z <- function(x, z) {
  geovctrs_cpp_set_z(x, vec_recycle(z, vec_size(x)))
}
