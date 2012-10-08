Definitions.

WS     = ([\s\t])
NL     = (\r?[\n])

COMMENT = (#.*\n)
STR = ([a-zA-Z_/\-0-9\+\.]+)

Rules.

{STR} : {token, {'string', TokenLine, TokenChars}}.
{NL}  : {token, {'newline', TokenLine}}.
 
{COMMENT} : skip_token.
{WS}+  : skip_token.


Erlang code.

