BattleCore:

InitBattle:
	ld a, [W_CUROPPONENT]
	and a
	jr z, WildEncounterTest
	
InitOpponent:
	ld a, [W_CUROPPONENT]
	ld [wcf91], a
	ld [wEnemyMonSpecies2], a
	jr asm_3ef3d
	
WildEncounterTest:
	ld a, [wNumberOfNoRandomBattleStepsLeft]
	and a
	ret nz
	callab TryDoWildEncounter
	ret nz
	
asm_3ef3d:	