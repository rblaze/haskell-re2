#include "haskell-re2.h"

#include <cstddef>
#include <cstdlib>

#include "re2/re2.h"

extern "C" {

re2::RE2 *haskell_re2_compile_pattern(const char *input, int input_len) {
	re2::RE2::Options opts;
	opts.set_log_errors(false);
	return new re2::RE2(re2::StringPiece(input, input_len), opts);
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

const char *haskell_re2_pattern_input(re2::RE2 *regex) {
	return regex->pattern().c_str();
}

int haskell_re2_program_size(re2::RE2 *regex) {
	return regex->ProgramSize();
}

void haskell_re2_quote_meta(const char *in, int in_len, char **out, int *out_len) {
	std::string quoted = re2::RE2::QuoteMeta(re2::StringPiece(in, in_len));
	*out_len = quoted.size();
	*out = static_cast<char*>(malloc(quoted.size()));
	memcpy(*out, quoted.c_str(), quoted.size());
}

char *haskell_re2_replace(re2::RE2 *regex, const char *input, const char *rewrite) {
	std::string str(input);
	if (re2::RE2::Replace(&str, *regex, rewrite)) {
		char *out = (char*)malloc(str.size() + 1);
		strcpy(out, str.c_str());
		return out;
	}
	return NULL;
}

char *haskell_re2_global_replace(re2::RE2 *regex, const char *input, const char *rewrite, int *count) {
	std::string str(input);
	*count = re2::RE2::GlobalReplace(&str, *regex, rewrite);
	if (*count > 0) {
		char *out = (char*)malloc(str.size() + 1);
		strcpy(out, str.c_str());
		return out;
	}
	return NULL;
}

char *haskell_re2_extract(re2::RE2 *regex, const char *input, const char *rewrite) {
	std::string str;
	if (re2::RE2::Extract(input, *regex, rewrite, &str)) {
		char *out = (char*)malloc(str.size() + 1);
		strcpy(out, str.c_str());
		return out;
	}
	return NULL;
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
