BattleCore:

; check W_CUROPPONENT to determine if wild or trainer battle
InitBattle:
	ld a, [W_CUROPPONENT]
	and a
	jr z, WildEncounterTest

; it's a pending trainer battle	
InitOpponent:
	ld a, [W_CUROPPONENT]
	ld [wcf91], a
	ld [wEnemyMonSpecies2], a
	jr BattleWillOccur

; could be a wild battle	
WildEncounterTest:
	ld a, [wNumberOfNoRandomBattleStepsLeft]
	and a
	ret nz
	callab TryDoWildEncounter
	ret nz
	
BattleWillOccur:
	ld a, [wMapPalOffset]
	push af
	ld hl, wd358
	ld a, [hl]
	push af
	res 1, [hl] ; @@@ what is this for?
	callab InitBattleVariables