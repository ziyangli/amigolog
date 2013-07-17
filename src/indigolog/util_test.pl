
prim_fluent(test_counter).
initially(test_counter, 0).
cache(test_counter).

prim_fluent(counter_2).
initially(counter_2, 100).

prim_action(add_count).
poss(add_count, test_counter<1000).
causes_val(add_count, test_counter, V, V is test_counter+1).
causes_val(add_count, counter_2, V, V is counter_2-1).

proc(on_off_combine_test,
     [search([star(add_count, 10), ?(test_counter=4), go_table(1)], 'searching message.'),
      go_table(4)]).

proc(trans_final_test, [star(add_count), star(go_table(1)), star(go_table(2))]).


prim_action(del_count(X)) :- member(X, [1, 2, 3]).
poss(del_count, true).
proc(ndet_test, ndet([add_count, del_count], [add_count, add_count])).
proc(rpi_test, rpi(X, [1, 2, 3], star(add_count(X), 1))).
proc(star_test, star(add_count, 1)).

prim_action(rec_add).
poss(rec_add, true).

exog_action(fail_add).
rescues(add_count, fail_add_rec).
proc(fail_add_rec, [rec_add, rec_add]).

proc(roll_action_test, [add_count, add_count, add_count, add_count, add_count, add_count, add_count]).
proc(par_test, [add_count, add_count, par(add_count, add_count), add_count]).