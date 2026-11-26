Extract:

  $ rocq compile -I ../../../install/default/lib/ mutual_fix.v > extr.hrl

  $ cat extr.hrl
  't_mutual_fix2'(_a@0) ->
    case _a@0 of
      'O' ->
        'O';
      {'S', _a@1} ->
        't_mutual_fix'(_a@1)
    end.
  
  't_mutual_fix'(_a@0) ->
    _x@1 = {'S', _a@0},
    case _a@0 of
      'O' ->
        _x@1;
      {'S', _a@2} ->
        't_mutual_fix2'(_a@2)
    end.



Compile:

  $ erlc extr.erl

Run:

  $ erl -pa . -noshell -eval "extr:run(t_mutual_fix, [{'S', {'S', 'O'}}])"
  {'S','O'}
