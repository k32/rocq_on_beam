  $ rocq compile -I ../../../install/default/lib/ mutual_fix.v
  't_mutual_fix'(_a@0) ->
    _x@1 = {'S', _a@0},
    case _a@0 of
      'O' ->
        _x@1;
      {'S', _a@2} ->
        't_mutual_fix2'(_a@2)
    end.
  
  
  't_mutual_fix2'(_a@0) ->
    case _a@0 of
      'O' ->
        'O';
      {'S', _a@1} ->
        't_mutual_fix'(_a@1)
    end.
  
