#ifndef TOKENS_H
#define TOKENS_H

// End Of File
#define T_EOF 0

/**
 * Reserved Words
 */

// ([a-zA-Z][a-zA-Z0-9]+)|(_[a-zA-Z0-9]+_[A-Za-z0-9]+_) 

//
#define T_FUNCTION 1
#define T_SUBROUTINE 2
#define T_END 3
#define T_INTEGER 4
#define T_REAL 5
#define T_LOGICAL 6
#define T_CHARACTER 7
#define T_RECORD 8
#define T_ENDRECORD 9
#define T_DATA 10
#define T_CONTINUE 11
#define T_GOTO 12
#define T_CALL 13
#define T_READ 14
#define T_WRITE 15
#define T_IF 16
#define T_THEN 17
#define T_ELSE 18
#define T_ENDIF 19
#define T_DO 20
#define T_ENDDO 21
#define T_STOP 22
#define T_RETURN 23

#define MAX_STR_LINE 6969

#define TOKEN(token_id) { print_token(token_id, #token_id); return token_id; }
#define NUMBER(token_id, base) { print_int(token_id, base); return token_id; }
#define FLOAT(token_id, base) { print_float(token_id, base); return token_id; }

#define STRING() { print_string(); return T_STRING; }

#define DEBUG 0



/**
 * Simple Constants
 */

/**
 * Represents Integer Constants
 * 
 * Allowed:
 * - 0
 * - 180
 * - 0Η9F0
 * - 0B1001
 * 
 * Not Allowed:
 * - 0180
 * - ΗΒ7
 * - 0Η0
 * - 00Β10
 * - 0ΗG8Α
 * - 0Β301
 */
#define T_ICONST 24

/**
 * Represents Real Constants
 * 
 * Allowed:
 * - 180Ε-2
 * - .5
 * - 180.100
 * - 7.0
 * - 0ΗΑ.
 * - 0Β1.0010
 * - 0Η0.00Β9CF
 * 
 * Not Allowed:
 * - 180Ε-2.2
 * - .Ε-2
 * - .5.
 * - 7.00
 * - .5G-2
 * - 05.2Ε-05
 * - 0ΗΒΕ-2
 * - 0Β01.1
 * - 1001
 */
#define T_RCONST 25

/**
 * Represents Logical Constants
 * 
 * .TRUE. and .FALSE.
 */
#define T_LCONST 26

/**
 * Represents Character Constants
 * 
 * Allowed:
 * - 'a'
 * - '$'
 * - ' '
 * - '\n'
 * - '"'
 * 
 * Not Allowed:
 * - 'ac' (only 1 character allowed)
 * - '\p'
 * - '\\'
 */
#define T_CCONST 27

/**
 * Represents an ID
 * 
 * An ID must start from a Character  can contain Alphanumeric Characters
 * 
 * The ID can start with _  but it MUST end with _ 
 * 
 * _ is allowed in the middle ONLY if it STARTS with _. Also  in the middle  it CANNOT contain 2 OR MORE consecutive _ (__  ____)
 * 
 * Allowed:
 * - a100version2
 * - _a100_version2_
 * 
 * Not Allowed:
 * 
 * - 100version2starts with number
 * - a100_version2      doesn't start with _
 * - _a100__version2_   consecutive _ in middle
 * - a100_version2_     does not start with _
 * - a100--version2     - is not allowed
 */
#define T_ID 28

/**
 * Operators
 */

// Logical .OR.
// \.OR\.
// ".OR."
#define T_OROP 29

// Logical .AND.
#define T_ANDOP 30

// Logical .NOT.
#define T_NOTOP 31

// Greater Than
#define T_RELOP_GT 32

// Greater Equal
#define T_RELOP_GE 33

// Lower Than
#define T_RELOP_LT 34

// Lower Equal
#define T_RELOP_LE 35

// Equal
#define T_RELOP_EQ 36

// Not Equal
#define T_RELOP_NE 37

#define T_ADDOP 38

#define T_MULTOP 39

#define T_DIVOP 40

#define T_POWOP 41

/**
 * Other Lectic Things 
 */

// (
#define T_LPAREN 42

// )
#define T_RPAREN 43

// ,
#define T_COMMA 44

// =
#define T_ASSIGN 45

// :
#define T_COLON 46

/**
 * Comments
 * 
 * A comment starts with $  and takes up the entire line
 * 
 * Basically # from python
 */
#define T_COMMENT 47

#define T_STRING 48

#define ERR_UNTERMINATED_STR -1
#define ERR_ILL_STR -2

#define ERR_SEV_WARN 0
#define ERR_SEV_CRIT 10

#define MAX_ERRORS 10

#endif