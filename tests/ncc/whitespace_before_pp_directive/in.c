// FLAGS: -E
// clang-format off

     #   define FOO 1
  #ifndef FOO
 # error "FOO should be defined"
      #       endif
