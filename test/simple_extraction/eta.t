  $ rocq compile -I ../../../install/default/lib/ eta.v
  't_local_lam1'(_a@0) ->
    _f@1 = fun(_a@2) ->
             {'S', _a@2}
           end,
    _f@1(_f@1(_a@0)).
  
  't_local_lam2'(_a@0) ->
    _f@1 = fun(_a@2, _b@3) ->
             {'pair', _a@2, _b@3}
           end,
    _f@1(_f@1(_a@0, _a@0), _a@0).
  
  't_apply_var1'(_f@0, _x@1) ->
    _f@0(_f@0(_x@1)).
  
  't_apply_var2'(_f@0, _x@1, _y@2) ->
    _f@0(_f@0(_x@1, _y@2), _x@1).
  
