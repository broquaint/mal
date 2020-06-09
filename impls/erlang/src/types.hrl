% https://erlang.org/doc/getting_started/record_macros.html

% XXX Have all seq types samey?
-record(mal_list, {elems}).
-record(mal_vec, {elems}).
-record(mal_map, {pairs}).

% XXX Have all scalar types samey?
-record(mal_num, {val}).
-record(mal_str, {val}).
-record(mal_kwd, {val}).
-record(mal_sym, {val}).
