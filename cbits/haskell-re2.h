#ifndef HASKELL_RE2_H
#define HASKELL_RE2_H

#ifdef __cplusplus
extern "C" {
#endif  // __cplusplus

namespace re2 {
class RE2;
}

re2::RE2 *haskell_re2_compile_pattern(const char *input, int input_len);

void haskell_re2_delete_pattern(re2::RE2 *regex);

const char *haskell_re2_error(re2::RE2 *regex);

const char *haskell_re2_pattern_input(re2::RE2 *regex);

int haskell_re2_program_size(re2::RE2 *regex);

char *haskell_re2_quote_meta(const char *input);

char *haskell_re2_replace(re2::RE2 *regex, const char *input, const char *rewrite);

char *haskell_re2_global_replace(re2::RE2 *regex, const char *input, const char *rewrite, int *count);

char *haskell_re2_extract(re2::RE2 *regex, const char *input, const char *rewrite);

char **haskell_re2_match(re2::RE2 *regex, const char *input);

#ifdef __cplusplus
}
#endif  // __cplusplus

#endif  // HASKELL_RE2_H
