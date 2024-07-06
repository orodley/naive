#ifndef NAIVE_PREPROCESS_H_
#define NAIVE_PREPROCESS_H_

#include "array.h"
#include "reader.h"

bool preprocess(
    char *input_filename, Array(char *) *include_dirs,
    Array(char) *preprocessed, Array(Adjustment) *adjustments);

#endif
