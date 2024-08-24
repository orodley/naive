#ifndef NAIVE_SYNTAX_PREPROCESS_H_
#define NAIVE_SYNTAX_PREPROCESS_H_

#include "array.h"
#include "strings.h"

bool preprocess(
    String input_filename, Array(String) *include_dirs,
    Array(char) *preprocessed, Array(Adjustment) *adjustments);

#endif
