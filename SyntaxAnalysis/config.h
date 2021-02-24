#pragma once

#define MAX_ERRORS 0
#define MAX_STR_SIZE 512

#define ERR_UNTERMINATED_STR -1
#define ERR_ILL_STR -2

#define ERR_SEV_WARN 0
#define ERR_SEV_CRIT 10

#ifndef FLEX_DEBUG
#define FLEX_DEBUG 0
#define FLEX_SHOW_TOKENS 1
#define FLEX_SHOW_COMMENTS 1
#define FLEX_SHOW_NUMERIC 1
#define FLEX_SHOW_STRINGS 1
#endif