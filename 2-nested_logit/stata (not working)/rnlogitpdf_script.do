use http://www.stata-press.com/data/r15/restaurant
describe
*create tree model
nlogitgen type = restaurant(fast: Freebirds | MamasPizza, family: CafeEccell | LosNortenos| WingsNmore, fancy: Christophers | MadCows)
*view tree
nlogittree restaurant type, choice(chosen)
*nlogit DV alt-specific_IV || level_1_alt_var: nest-specific_IV, base(ref_group) || level2_alt_var:, case(observation_id)
nlogit chosen cost rating distance || type: income kids, base(family) || restaurant:, noconstant case(family_id)
