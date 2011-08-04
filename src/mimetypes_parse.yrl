Nonterminals

mappings mapping exts ext.

Terminals

string newline.

Rootsymbol mappings.

ext -> string : string('$1').

exts -> ext : ['$1'].
exts -> ext exts : ['$1' | '$2' ].

mapping -> string exts newline : {string('$1'), '$2'}.

mappings -> mapping : ['$1'].
mappings -> mapping mappings : ['$1' | '$2'].

Erlang code.

string({string, _L, S}) ->
  list_to_binary(S).
