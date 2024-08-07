#ifndef NAIVE_SYNTAX_PREPROCESS_H_
#define NAIVE_SYNTAX_PREPROCESS_H_

#include "array.h"

bool preprocess(
    char *input_filename, Array(char *) *include_dirs,
    Array(char) *preprocessed, Array(Adjustment) *adjustments);

#endif
