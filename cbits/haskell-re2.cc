#include "haskell-re2.h"

#include <cstddef>
#include <cstdlib>

#include "re2/re2.h"

namespace re2 {
class RE2_Options : public RE2::Options {};
}

extern "C" {

re2::RE2_Options *haskell_re2_alloc_options() {
	re2::RE2_Options* opts = new re2::RE2_Options();
	opts->set_log_errors(false);
	return opts;
}

void haskell_re2_free_options(re2::RE2_Options *opts) {
	delete opts;
}

#define IMPL_SETOPT(optName, type) void haskell_re2_setopt_##optName(re2::RE2_Options *opts, type val) { \
	opts->set_##optName(val); \
}

IMPL_SETOPT(posix_syntax, bool);
IMPL_SETOPT(longest_match, bool);
IMPL_SETOPT(max_mem, int64_t);
IMPL_SETOPT(literal, bool);
IMPL_SETOPT(never_nl, bool);
IMPL_SETOPT(dot_nl, bool);
IMPL_SETOPT(never_capture, bool);
IMPL_SETOPT(case_sensitive, bool);
IMPL_SETOPT(perl_classes, bool);
IMPL_SETOPT(word_boundary, bool);
IMPL_SETOPT(one_line, bool);

#undef IMPL_SETOPT

re2::RE2 *haskell_re2_compile_pattern(re2::RE2_Options* opts, const char *input, int input_len) {
	return new re2::RE2(re2::StringPiece(input, input_len), *opts);
}

void haskell_re2_delete_pattern(re2::RE2 *regex) {
	delete regex;
}

const char *haskell_re2_error(re2::RE2 *regex) {
	const std::string &err = regex->error();
	if (err.size() == 0) {
		return NULL;
	}
	return err.c_str();
}

int haskell_re2_error_code(re2::RE2 *regex) {
	return regex->error_code();
}

const char *haskell_re2_pattern_input(re2::RE2 *regex) {
	return regex->pattern().c_str();
}

int haskell_re2_program_size(re2::RE2 *regex) {
	return regex->ProgramSize();
}

void haskell_re2_quote_meta(const char *in, int in_len, char **out, size_t *out_len) {
	std::string quoted = re2::RE2::QuoteMeta(re2::StringPiece(in, in_len));
	*out_len = quoted.size();
	*out = static_cast<char*>(malloc(quoted.size()));
	memcpy(*out, quoted.c_str(), quoted.size());
}

bool haskell_re2_replace(re2::RE2 *regex, const char *in, size_t in_len, const char *rewrite, int rewrite_len, char **out, size_t *out_len) {
	std::string str(in, in_len);
	if (re2::RE2::Replace(&str, *regex, re2::StringPiece(rewrite, rewrite_len))) {
		*out_len = str.size();
		*out = static_cast<char*>(malloc(str.size()));
		memcpy(*out, str.c_str(), str.size());
		return true;
	}
	return false;
}

void haskell_re2_global_replace(re2::RE2 *regex, const char *in, size_t in_len, const char *rewrite, int rewrite_len, char **out, size_t *out_len, int *count) {
	std::string str(in, in_len);
	*count = re2::RE2::GlobalReplace(&str, *regex, re2::StringPiece(rewrite, rewrite_len));
	if (*count > 0) {
		*out_len = str.size();
		*out = static_cast<char*>(malloc(str.size()));
		memcpy(*out, str.c_str(), str.size());
	}
}

bool haskell_re2_extract(re2::RE2 *regex, const char *in, int in_len, const char *rewrite, int rewrite_len, char **out, size_t *out_len) {
	std::string str;
	if (re2::RE2::Extract(re2::StringPiece(in, in_len), *regex, re2::StringPiece(rewrite, rewrite_len), &str)) {
		*out_len = str.size();
		*out = static_cast<char*>(malloc(str.size()));
		memcpy(*out, str.c_str(), str.size());
		return true;
	}
	return false;
}

char **haskell_re2_match(re2::RE2 *regex, const char *input) {
	std::string input_str(input);
	int capture_count = regex->NumberOfCapturingGroups();
	int argc = capture_count+1;
	re2::StringPiece *vec = new re2::StringPiece[argc];
	if (regex->Match(input_str, 0, input_str.size(), re2::RE2::UNANCHORED, vec, argc)) {
		char **args = new char*[argc+1];
		for (int ii = 0; ii < argc; ii++) {
			if (vec[ii].data() == NULL) {
				args[ii] = NULL;
			} else {
				args[ii] = (char*)malloc(vec[ii].size() + 1);
				strncpy(args[ii], vec[ii].data(), vec[ii].size());
				args[ii][vec[ii].size()] = 0;
			}
		}
		args[argc] = (char*)1;
		delete[] vec;
		return args;
	}
	delete[] vec;
	return NULL;
}

}
