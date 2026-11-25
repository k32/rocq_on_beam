Extract:

  $ rocq compile -I ../../../install/default/lib/ var_uniq.v > extr.hrl

  $ cat extr.hrl
  't_all_var_suff_unique'(_a@0) ->
    _a@1 = case _a@0 of
             'O' ->
               'None';
             {'S', _a@2} ->
               case _a@2 of
                 'O' ->
                   'None';
                 {'S', _a@3} ->
                   {'Some', _a@3}
               end
           end,
    _f@4 = fun(_a@5) ->
             case _a@5 of
               {'Some', _a@6} ->
                 {'Some', {'S', _a@6}};
               'None' ->
                 'None'
             end
           end,
    _f@4(_f@4(_a@1)).
  

Compile:
  $ erlc extr.erl

Run:
  $ erl -pa . -noshell -eval "extr:run(t_all_var_suff_unique, [{'S', {'S', 'O'}}])"
  {'Some',{'S',{'S','O'}}}
