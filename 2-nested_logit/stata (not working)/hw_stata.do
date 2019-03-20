describe
*create tree model
nlogitgen type = alt(purchase: br1 | br2 | br3 | br4 | br5 | br6 | br7 | br8, no_purchase: NP)
*view tree
nlogittree alt type, choice(chosen_choice)
*constraint no_purchase nest's lambda to 1
constraint 1 [no_purchase_tau]_cons = 1
*nlogit DV alt-specific_IV || level_1_alt_var: nest-specific_IV, base(ref_group) || level2_alt_var:, case(observation_id)
nlogit chosen_choice price || type: || alt:, case(chid) constraint(1)
