  $ rocq compile -I ../../../install/default/lib/ struct.v
  -module('struct.m').
  -export(['foo'/1]).
  
  'foo'(_a@0) ->
    {'pair', _a@0, _a@0}.
  
  -module('struct').
  -export(['bar'/1, 'hi'/0]).
  
  'bar'(_a@0) ->
    case 'struct.m':'foo'(_a@0) of
      {'pair', _@1, _x@2} ->
        'Corelib.Init.Nat':'add'(_x@2, _x@2)
    end.
  
  'hi'() ->
    {'S', 'O'}.
  
  -module('Corelib.Init.Nat').
  -export(['add'/2]).
  
  'add'(_n@0, _m@1) ->
    case _n@0 of
      'O' ->
        _m@1;
      {'S', _p@2} ->
        {'S', 'add'(_p@2, _m@1)}
    end.
  
  
