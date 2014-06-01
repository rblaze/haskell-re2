#ifndef HASKELL_RE2_H
#define HASKELL_RE2_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif  // __cplusplus

namespace re2 {
class RE2;
class RE2_Options;
}

re2::RE2_Options *haskell_re2_alloc_options();

void haskell_re2_free_options(re2::RE2_Options *opts);

void haskell_re2_setopt_posix_syntax(re2::RE2_Options *opts, bool);
void haskell_re2_setopt_longest_match(re2::RE2_Options *opts, bool);
void haskell_re2_setopt_max_mem(re2::RE2_Options *opts, int64_t);
void haskell_re2_setopt_literal(re2::RE2_Options *opts, bool);
void haskell_re2_setopt_never_nl(re2::RE2_Options *opts, bool);
void haskell_re2_setopt_dot_nl(re2::RE2_Options *opts, bool);
void haskell_re2_setopt_never_capture(re2::RE2_Options *opts, bool);
void haskell_re2_setopt_case_sensitive(re2::RE2_Options *opts, bool);
void haskell_re2_setopt_perl_classes(re2::RE2_Options *opts, bool);
void haskell_re2_setopt_word_boundary(re2::RE2_Options *opts, bool);
void haskell_re2_setopt_one_line(re2::RE2_Options *opts, bool);

re2::RE2 *haskell_re2_compile_pattern(re2::RE2_Options *opts, const char *input, int input_len);

void haskell_re2_delete_pattern(re2::RE2 *regex);

const char *haskell_re2_error(re2::RE2 *regex);

int haskell_re2_error_code(re2::RE2 *regex);

const char *haskell_re2_pattern_input(re2::RE2 *regex);

int haskell_re2_program_size(re2::RE2 *regex);

void haskell_re2_quote_meta(const char *in, int in_len, char **out, int *out_len);

char *haskell_re2_replace(re2::RE2 *regex, const char *input, const char *rewrite);

char *haskell_re2_global_replace(re2::RE2 *regex, const char *input, const char *rewrite, int *count);

char *haskell_re2_extract(re2::RE2 *regex, const char *input, const char *rewrite);

char **haskell_re2_match(re2::RE2 *regex, const char *input);

#ifdef __cplusplus
}
#endif  // __cplusplus

#endif  // HASKELL_RE2_H