; adds 1 to damage and multiplies damage by %111RRRR0 / 255, where R is a random bit (min 224/255 - max 254/255)
RandomizeDamage: ; 3e687 (f:6687)
	ld hl, W_PLAYERDAMAGE
	ld a, [H_WHOSETURN]
	and a
	jr z, .ok
	inc hl
	inc hl ; W_ENEMYDAMAGE
.ok	
	inc hl
	inc [hl]
	dec hl
	
	ld a, [hli]
	and a
	jr nz, .DamageGreaterThanOne
	ld a, [hl]
	cp 2
	ret c ; return if damage is equal to 0 or 1
.DamageGreaterThanOne
	xor a
	ld [H_MULTIPLICAND], a
	dec hl
	ld a, [hli]
	ld [H_MULTIPLICAND + 1], a
	ld a, [hl]
	ld [H_MULTIPLICAND + 2], a
; loop until a random number greater than or equal to 217 is generated
.loop
	call BattleRandom
	and %00011110
	add %11100000
	ld [H_MULTIPLIER], a
	call Multiply ; multiply damage by the random number, which is in the range [217, 255]
	ld a, 255
	ld [H_DIVISOR], a
	ld b, $4
	call Divide ; divide the result by 255
; store the modified damage
	ld hl, W_PLAYERDAMAGE
	ld a, [H_WHOSETURN]
	and a
	jr z, .ok2
	inc hl
	inc hl ; W_ENEMYDAMAGE
.ok2
	ld a, [H_QUOTIENT + 2]
	ld [hli], a
	ld a, [H_QUOTIENT + 3]
	ld [hl], a
	ret


OtherModifiers:
; @@@ reflect/ls goes here
; @@@ weather goes here
; @@@ item boosts go here
; double damage to deal if critical hit
	ld a, [wCriticalHitOrOHKO]
	dec a
	jr z, .doubleDamage
	ret
	
.doubleDamage
	ld a, [H_WHOSETURN]
	ld hl, W_PLAYERDAMAGE
	and a
	jr z, .ok
	ld hl, W_ENEMYDAMAGE
.ok	
	sla [hl]
	inc hl
	sla [hl]
	dec hl
	jr nc, .doubleDone
	inc [hl]
	
.doubleDone
	ld a, [hli]
	cp 999 / $100
	jr c, .done
	jr nz, .cap
	
	ld a, [hl]
	cp 999 % $100
	jr c, .done	

.cap	
	ld a, 999 % $100
	ld [hld], a
	ld a, 999 / $100
	ld [hl], a
.done	
	ret


ApplyAttackToEnemyPokemon:
; fallthrough

ApplyDamageToEnemyPokemon:
	ld hl,W_PLAYERDAMAGE
	ld a,[hli]
	ld b,a
	ld a,[hl]
	or b
	jr z,ApplyAttackToEnemyPokemonDone ; we're done if damage is 0

; subtract the damage from the pokemon's current HP
; also, save the current HP at wHPBarOldHP
	ld a,[hld]
	ld b,a
	ld a,[wEnemyMonHP + 1]
	ld [wHPBarOldHP],a
	sub b
	ld [wEnemyMonHP + 1],a
	ld a,[hl]
	ld b,a
	ld a,[wEnemyMonHP]
	ld [wHPBarOldHP+1],a
	sbc b
	ld [wEnemyMonHP],a
	jr nc,.animateHpBar
; if more damage was done than the current HP, zero the HP and set the damage (W_PLAYERDAMAGE)
; equal to how much HP the pokemon had before the attack
	ld a,[wHPBarOldHP+1]
	ld [hli],a
	ld a,[wHPBarOldHP]
	ld [hl],a
	xor a
	ld hl,wEnemyMonHP
	ld [hli],a
	ld [hl],a
.animateHpBar
	ld hl,wEnemyMonMaxHP
	ld a,[hli]
	ld [wHPBarMaxHP+1],a
	ld a,[hl]
	ld [wHPBarMaxHP],a
	ld hl,wEnemyMonHP
	ld a,[hli]
	ld [wHPBarNewHP+1],a
	ld a,[hl]
	ld [wHPBarNewHP],a
	hlCoord 2, 2
	xor a
	ld [wHPBarType],a
	predef UpdateHPBar_Hook
ApplyAttackToEnemyPokemonDone:
	jp DrawHUDsAndHPBars
	

ApplyAttackToPlayerPokemon: ; 3e1a0 (f:61a0)
; fallthrough

ApplyDamageToPlayerPokemon: ; 3e200 (f:6200)
	ld hl,W_ENEMYDAMAGE
	ld a,[hli]
	ld b,a
	ld a,[hl]
	or b
	jr z,ApplyAttackToPlayerPokemonDone ; we're done if damage is 0

; subtract the damage from the pokemon's current HP
; also, save the current HP at wHPBarOldHP and the new HP at wHPBarNewHP
	ld a,[hld]
	ld b,a
	ld a,[wBattleMonHP + 1]
	ld [wHPBarOldHP],a
	sub b
	ld [wBattleMonHP + 1],a
	ld [wHPBarNewHP],a
	ld b,[hl]
	ld a,[wBattleMonHP]
	ld [wHPBarOldHP+1],a
	sbc b
	ld [wBattleMonHP],a
	ld [wHPBarNewHP+1],a
	jr nc,.animateHpBar
; if more damage was done than the current HP, zero the HP and set the damage (W_PLAYERDAMAGE)
; equal to how much HP the pokemon had before the attack
	ld a,[wHPBarOldHP+1]
	ld [hli],a
	ld a,[wHPBarOldHP]
	ld [hl],a
	xor a
	ld hl,wBattleMonHP
	ld [hli],a
	ld [hl],a
	ld hl,wHPBarNewHP
	ld [hli],a
	ld [hl],a
.animateHpBar
	ld hl,wBattleMonMaxHP
	ld a,[hli]
	ld [wHPBarMaxHP+1],a
	ld a,[hl]
	ld [wHPBarMaxHP],a
	hlCoord 10, 9
	ld a,$01
	ld [wHPBarType],a
	predef UpdateHPBar_Hook
ApplyAttackToPlayerPokemonDone
	jp DrawHUDsAndHPBars
	

; function to adjust the base damage of an attack to account for type effectiveness
AdjustDamageForMoveType: ; 3e3a5 (f:63a5)
; values for player turn
	ld hl,wBattleMonType
	ld a,[hli]
	ld b,a    ; b = type 1 of attacker
	ld c,[hl] ; c = type 2 of attacker
	ld hl,wEnemyMonType
	ld a,[hli]
	ld d,a    ; d = type 1 of defender
	ld e,[hl] ; e = type 2 of defender
	ld a,[W_PLAYERMOVETYPE]
	ld [wd11e],a
	ld a,[H_WHOSETURN]
	and a
	jr z,.next
; values for enemy turn
	ld hl,wEnemyMonType
	ld a,[hli]
	ld b,a    ; b = type 1 of attacker
	ld c,[hl] ; c = type 2 of attacker
	ld hl,wBattleMonType
	ld a,[hli]
	ld d,a    ; d = type 1 of defender
	ld e,[hl] ; e = type 2 of defender
	ld a,[W_ENEMYMOVETYPE]
	ld [wd11e],a
.next
	ld a,[wd11e] ; move type
	cp b ; does the move type match type 1 of the attacker?
	jr z,.sameTypeAttackBonus
	cp c ; does the move type match type 2 of the attacker?
	jr z,.sameTypeAttackBonus
	jr .skipSameTypeAttackBonus
.sameTypeAttackBonus
; if the move type matches one of the attacker's types
; multiply by 3/2
	ld hl, H_MULTIPLIER
	ld [hl], 3
	call Multiply
	
	ld [hl], 2
	ld b, 4
	call Divide
	
	ld hl,wDamageMultipliers
	set 7,[hl] ; STAB
.skipSameTypeAttackBonus
	ld a,[wd11e]
	ld b,a ; b = move type
	ld hl,TypeEffects
.loop
	ld a,[hli] ; a = "attacking type" of the current type pair
	cp a,$ff
	jr z,.done
	cp b ; does move type match "attacking type"?
	jr nz,.nextTypePair
	ld a,[hl] ; a = "defending type" of the current type pair
	cp d ; does type 1 of defender match "defending type"?
	jr z,.matchingPairFound
	cp e ; does type 2 of defender match "defending type"?
	jr z,.matchingPairFound
	jr .nextTypePair
.matchingPairFound
; if the move type matches the "attacking type" and one of the defender's types matches the "defending type"
	push hl
	push bc
	inc hl
	ld a,[wDamageMultipliers]
	and a,$80
	ld b,a
	ld a,[hl] ; a = damage multiplier
	ld [H_MULTIPLIER],a

; done if type immunity	
	and a
	jr z, .typeImmunityDone

; apply damage multiplier (5 or 20)
	add b
	ld [wDamageMultipliers],a
	call Multiply

; divide by 10	
	ld a,10
	ld [H_DIVISOR],a
	ld b,$04
	call Divide

	pop bc
	pop hl
.nextTypePair
	inc hl
	inc hl
	jp .loop
.done
	ret
	
.typeImmunityDone
;	xor a
	ld [wDamageMultipliers],a
	inc a
	ld [W_MOVEMISSED],a
	pop bc
	pop hl
	ret	


CalculateDamage: ; 3df65 (f:5f65)
; input:
;	b: attack
;	c: opponent defense
;	d: base power
;	e: level
; for max. level = 100 only 

; init math buffer
	xor a
	ld hl, H_DIVIDEND
	ldi [hl], a ; H_DIVIDEND [x][][][]
	ldi [hl], a ; H_DIVIDEND [][x][][]
	ld [hl], a  ; H_DIVIDEND [][][x][]
	
; Multiply (level + 1) by 2
	inc e
	ld a, e
	add a
	inc hl
	ld [hl], a ; H_DIVIDEND [][][][x]

; Add 2
	inc [hl] ; H_DIVIDEND [][][][x]
	inc [hl] ; H_DIVIDEND [][][][x]	
	inc hl ; H_MULTIPLIER

; Multiply by attack base power
	ld [hl], d
	call Multiply

; Multiply by attack stat
	ld [hl], b
	call Multiply

; Divide by 10
; this is here to prevent a potential H_MULTIPLICAND overflow
; maximum possible H_MULTIPLICAND value is eventually 0xede3f4
	ld [hl], 10
	ld b, 4
	call Divide		
	
; Apply type matchup and STAB
	push hl
	push de
	push bc
	push af
	call AdjustDamageForMoveType
	pop af
	pop bc
	pop de
	pop hl

; Divide by defender's defense stat
	ld [hl], c
	ld b, 4
	call Divide

; Divide by 30
	ld [hl], 30
	ld b, 4
	call Divide
	
; save result into p/e damage
; maximum possible value is 0xfdbff
	ld hl, W_PLAYERDAMAGE
	ld a, [H_WHOSETURN]
	and a
	jr z, .ok
	inc hl
	inc hl ; W_ENEMYDAMAGE
	
.ok
	ld b, [hl]
	ld a, [H_QUOTIENT + 3]
	add b
	ld [H_QUOTIENT + 3], a
	jr nc, .asm_3dfd0

	ld a, [H_QUOTIENT + 2]
	inc a
	ld [H_QUOTIENT + 2], a
	and a
	jr z, .asm_3e004

.asm_3dfd0
	ld a, [H_QUOTIENT]
	ld b, a
	ld a, [H_QUOTIENT + 1]
	or a
	jr nz, .asm_3e004

	ld a, [H_QUOTIENT + 2]
	cp 998 / $100
	jr c, .asm_3dfe8
	cp 998 / $100 + 1
	jr nc, .asm_3e004
	ld a, [H_QUOTIENT + 3]
	cp 998 % $100
	jr nc, .asm_3e004

.asm_3dfe8
	inc hl
	ld a, [H_QUOTIENT + 3]
	ld b, [hl]
	add b
	ld [hld], a

	ld a, [H_QUOTIENT + 2]
	ld b, [hl]
	adc b
	ld [hl], a
	jr c, .asm_3e004

	ld a, [hl]
	cp 998 / $100
	jr c, .asm_3e00a
	cp 998 / $100 + 1
	jr nc, .asm_3e004
	inc hl
	ld a, [hld]
	cp 998 % $100
	jr c, .asm_3e00a

.asm_3e004
; cap at 998
	ld a, 998 / $100
	ld [hli], a
	ld a, 998 % $100
	ld [hld], a

.asm_3e00a
; add 1 (minimum damage is 1)
; brings cap to 999
	inc hl
	ld a, [hl]
	add 1
	ld [hld], a
	jr nc, .done
	inc [hl]

.done
	ret


; swap player and enemy in commentary for enemy attack
ScaleStats:
	ld a, [hli]
	ld l, [hl]
	ld h, a ; hl = player's offensive stat
	or b ; is either high byte nonzero?
	jr z, .next2 ; if not, we don't need to scale
	
; bc /= 4 (scale enemy's defensive stat)
	srl b
	rr c
	srl b
	rr c
	
	ld a, c
	or b ; is the player's offensive stat 0?
	jr nz, .next1
	inc c ; if the player's offensive stat is 0, bump it up to 1	
	
.next1
; hl /= 4 (scale player's offensive stat)
	srl h
	rr l
	srl h
	rr l
	
	ld a, l
	or h ; is the player's offensive stat 0?
	jr nz, .next2
	inc l ; if the player's offensive stat is 0, bump it up to 1
	
.next2
; b = player's offensive stat (possibly scaled)
; c = enemy's defensive stat (possibly scaled)
	ld b, l
	ret


; sets b, c, d, and e for the CalculateDamage routine in the case of an attack by the enemy mon
GetDamageVarsForEnemyAttack:
; d = move power
	ld hl, W_ENEMYMOVEPOWER
	ld a, [hli]
	ld d, a ; d = move power
	
; is the move physical or special?
	ld a, [hl] ; W_ENEMYMOVETYPE
	ld hl, SpecialTypesArray
	push de
	ld de, 1	
	call IsInArray
	pop de
	jr c, .specialAttack
	
.physicalAttack
; bc = player defense
	ld hl, wBattleMonDefense
	ld a, [hli]
	ld b, a
	ld c, [hl]
; hl = enemy attack
	ld hl, wEnemyMonAttack
	jr .scaleStats
	
.specialAttack
; bc = player special @@@ special defense
	ld hl, wBattleMonSpecial
	ld a, [hli]
	ld b, a
	ld c, [hl]
; hl = enemy special	
	ld hl, wEnemyMonSpecial
	
; if either the offensive or defensive stat is too large to store in a byte, scale both stats by dividing them by 4
; this allows values with up to 10 bits (values up to 1023) to be handled
; anything larger will wrap around
.scaleStats
	call ScaleStats

; e = level		
	ld a, [wEnemyMonLevel]
	ld e, a
	ret

; sets b, c, d, and e for the CalculateDamage routine in the case of an attack by the player mon
; b = player's offensive stat (possibly scaled)
; c = enemy's defensive stat (possibly scaled)
; d = move power
; e = player level	
GetDamageVarsForPlayerAttack:
; d = move power
	ld hl, W_PLAYERMOVEPOWER
	ld a, [hli]
	ld d, a ; d = move power
	
; is the move physical or special?
	ld a, [hl] ; W_PLAYERMOVETYPE
	ld hl, SpecialTypesArray
	push de
	ld de, 1
	call IsInArray
	pop de
	jr c, .specialAttack
	
.physicalAttack
; bc = enemy defense
	ld hl, wEnemyMonDefense
	ld a, [hli]
	ld b, a
	ld c, [hl]
; hl = player attack
	ld hl, wBattleMonAttack
	jr .scaleStats
	
.specialAttack
; bc = enemy special @@@ special defense
	ld hl, wEnemyMonSpecial
	ld a, [hli]
	ld b, a
	ld c, [hl]
; hl = player special	
	ld hl, wBattleMonSpecial
	
; if either the offensive or defensive stat is too large to store in a byte, scale both stats by dividing them by 4
; this allows values with up to 10 bits (values up to 1023) to be handled
; anything larger will wrap around
.scaleStats
	call ScaleStats

; e = level		
	ld a, [wBattleMonLevel]
	ld e, a
	ret
	
SpecialTypesArray:
	db GHOST
	db FIRE
	db WATER
	db GRASS
	db ELECTRIC
	db PSYCHIC
	db ICE
	db $ff

; determines if attack is a critical hit
CriticalHitTest:
	ld a, [H_WHOSETURN]
	and a
	ld a, [wEnemyMonSpecies]
	ld hl, W_ENEMYMOVENUM
	jr nz, .criticalHitTest
	ld a, [wBattleMonSpecies]
	ld hl, W_PLAYERMOVENUM
	
.criticalHitTest
; get attacking mon base speed stat
	ld [wd0b5], a
	call GetMonHeader
	ld a, [W_MONHBASESPEED]
; add 50	
	add 50
	ld b, a

; check if a high critical rate move was used
	ld c, [hl] ; MOVENUM
	ld hl, HighCriticalMoves
.loop
	ld a, [hli]
	cp c
	jr z, .highCritical
; $ff entry terminates loop
	inc a
	jr nz, .loop
	
; it's a regular move: (base speed + 50) / 4
	srl b ; divide by 2

; it's a high critical move: (base speed + 50) / 2
.highCritical
	call BattleRandom
	srl b ; divide by 2
	cp b
	ret nc

; move is a critical hit, so set flag
	ld a, $1
	ld [wCriticalHitOrOHKO], a
	ret
	
HighCriticalMoves:
	db $ff

; some tests that need to pass for a move to hit
MoveHitTest: ; 3e56b (f:656b)
; scale the move accuracy according to attacker's accuracy and target's evasion
	call CalcHitChance
	ld a,[W_PLAYERMOVEACCURACY]
	ld b,a
	ld a,[H_WHOSETURN]
	and a
	jr z,.doAccuracyCheck
	ld a,[W_ENEMYMOVEACCURACY]
	ld b,a
	
.doAccuracyCheck
; if the accuracy is 255, the move hits
	ld a,b
	cp $ff
	jr z, .moveDidntMiss
; else, if the random number generated is greater than or equal to the scaled accuracy, the move misses
	call Random
	cp b
	jr nc,.moveMissed

.moveDidntMiss	
; reset W_MOVEMISSED flag	
	xor a
	ld [W_MOVEMISSED],a
	ret
	
.moveMissed
 ; zero the damage
	ld hl,W_PLAYERDAMAGE 
	ld a,[H_WHOSETURN]
	and a
	jr z, .zeroDamage
	ld hl,W_ENEMYDAMAGE
.zeroDamage	
	xor a
	ld [hli],a
	ld [hl],a

; set W_MOVEMISSED flag	
	inc a
	ld [W_MOVEMISSED],a
	ret


; called in the start of execute p/e move
InitTurnVariables:
; init move feedback addresses and critical hit flag
	xor a
	ld [W_MOVEMISSED], a
	ld [wMoveDidntMiss], a
	ld [wCriticalHitOrOHKO], a

; init damage multipliers to neutral	
	ld a, $a
	ld [wDamageMultipliers], a

	
; return selected move at wEnemySelectedMove
SelectEnemyMove: ; 3d564 (f:5564)
	ld a, [wLinkState]
	sub LINK_STATE_BATTLING
	jr nz, .noLinkBattle
	
; link battle
	call SaveScreenTilesToBuffer1
	call LinkBattleExchangeData
	call LoadScreenTilesFromBuffer1
	ld a, [wSerialExchangeNybbleReceiveData]
	cp $e
	jp z, .useStruggle
	cp $d
	jr z, .unableToSelectMove
	cp $4
	ret nc

; find out which move the other player selected
	ld [wEnemyMoveListIndex], a
	ld c, a
	ld hl, wEnemyMonMoves
	ld b, $0
	add hl, bc
	ld a, [hl]
	jr .done
	
.noLinkBattle
	ld a, [W_ISINBATTLE]
	dec a
	jr z, .chooseRandomMove ; wild battle

; returns enemy trainer chosen move at b	
;	callab AIEnemyTrainerChooseMoves ; trainer battle @@@ this needs lots of work!
;	jr .moveChosen
	
.chooseRandomMove
	ld hl, wEnemyMonMoves
	call BattleRandom
	ld b, $0
	cp $40 ; select move 1 in [0,3f] (64/256 chance)
	jr c, .moveChosen
	inc hl
	inc b
	cp $80 ; select move 2 in [40,7f] (64/256 chance)
	jr c, .moveChosen
	inc hl
	inc b
	cp $c0 ; select move 3 in [80,bf] (64/256 chance)
	jr c, .moveChosen
	inc hl
	inc b ; select move 4 in [c0,ff] (64/256 chance)
	
.moveChosen
	ld a, b
	ld [wEnemyMoveListIndex], a

	ld a, [hl]
	and a
	jr z, .chooseRandomMove ; move non-existant, try again

; save selected move at wEnemySelectedMove
.done
	ld [wEnemySelectedMove], a
	ret
	
.unableToSelectMove
	ld a, $ff
	jr .done
	
.useStruggle
	ld a, STRUGGLE
	jr .done


SendOutMon: ; 3cc91 (f:4c91)
	callab PrintSendOutMonMessage
	ld hl, wEnemyMonHP
	ld a, [hli]
	or [hl] ; is enemy mon HP zero?
	jp z, .skipDrawingEnemyHUDAndHPBar; if HP is zero, skip drawing the HUD and HP bar
	call DrawEnemyHUDAndHPBar
	
.skipDrawingEnemyHUDAndHPBar
	call DrawPlayerHUDAndHPBar
	predef LoadMonBackPic
	
	xor a
	ld [$ffe1], a
	
	ld hl, wcc2d
	ld [hli], a
	ld [hl], a
	
	ld [wBoostExpByExpAll], a
	ld [wDamageMultipliers], a
	ld [W_PLAYERMOVENUM], a
	
; player and enemy used moves	
	ld hl, wPlayerUsedMove
	ld [hli], a
	ld [hl], a
	
; player battle statuses	
	ld hl, W_PLAYERBATTSTATUS1
	ld [hli], a
	ld [hli], a
	ld [hl], a
	
; player disable/minimize	
	ld [W_PLAYERDISABLEDMOVE], a
	ld [wPlayerDisabledMoveNumber], a
	ld [wPlayerHasUsedMinimize], a
		
; enemy's trapping move battstatus	
	ld hl, W_ENEMYBATTSTATUS1
	res UsingTrappingMove, [hl]
	
; p/e damage addresses
	ld hl, W_PLAYERDAMAGE
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hl], a
	
; player bide accum. damage
	ld hl, wPlayerBideAccumulatedDamage
	ld [hli], a
	ld [hl], a
	
	ld b, $1
	call GoPAL_SET	

	ld a, $1
	ld [H_WHOSETURN], a
	ld a, POOF_ANIM
	call PlayMoveAnimation
	hlCoord 4, 11
	predef Func_3f073
	ld a, [wcf91]
	call PlayCry
	call PrintEmptyString
	jp SaveScreenTilesToBuffer1

LoadEnemyMonData: ; 3eb01 (f:6b01)
; load enemy mon data of wEnemyMonSpecies2 into the wEnemyMon struct
; this is executed on init wild battle, and also when enemy trainer sends a new mon
	ld a, [wLinkState]
	cp LINK_STATE_BATTLING
	jp z, LoadEnemyMonFromParty ; load mon from other player party if in a link battle
	
; not link battle	
	ld a, [wEnemyMonSpecies2]
	ld [wEnemyMonSpecies], a ; inside the enemy mon struct
	
	ld [wd0b5], a
; copies the base stat data of a pokemon to W_MONHDEXNUM (W_MONHEADER)
; INPUT:[wd0b5] = pokemon ID	
	call GetMonHeader
	
; transform will no longer copy DVs

; @@@ fixed DVs for trainer mon
	ld a, [W_ISINBATTLE]
	cp TRAINER_BATTLE
	ld a, $98
	ld b, $88
	jr z, .storeDVs
	
; random DVs for wild mon
	call BattleRandom
	ld b, a
	call BattleRandom
	
.storeDVs
	ld hl, wEnemyMonDVs ; enemy mon struct
	ld [hli], a
	ld [hl], b

; level	(for wild, was loaded during TryDoWildEncounter)
	ld de, wEnemyMonLevel
	ld a, [W_CURENEMYLVL]
	ld [de], a
	
; calculate stats (b = consider stat exp? ; 0 = do not consider it @@@)	
	inc de
	ld b, $0
	ld hl, wEnemyMonHP
	push hl
	call CalcStats ; home.asm
	pop hl

; trainer mon could be damaged or statused, so read from party	
	ld a, [W_ISINBATTLE]
	cp TRAINER_BATTLE 
	jr z, .copyHPAndStatusFromPartyData
	
; wild battle	
; init the current HP to max HP and the status to 0
	ld a, [wEnemyMonMaxHP]
	ld [hli], a
	ld a, [wEnemyMonMaxHP+1]
	ld [hli], a
	xor a
	inc hl
	ld [hl], a
	jr .copyTypes

; trainer battle	
; if it's a trainer mon, copy the HP and status from the enemy party data
.copyHPAndStatusFromPartyData
	ld hl, wEnemyMon1HP
	ld a, [wWhichPokemon]
	ld bc, wEnemyMon2 - wEnemyMon1
	call AddNTimes
	ld a, [hli]
	ld [wEnemyMonHP], a
	ld a, [hli]
	ld [wEnemyMonHP + 1], a
	
; wEnemyMonPartyPos	
	ld a, [wWhichPokemon]
	ld [wEnemyMonPartyPos], a
	
; wEnemyMonStatus	
	inc hl
	ld a, [hl]
	ld [wEnemyMonStatus], a

; wild and trainer battle	
.copyTypes
	ld hl, W_MONHTYPES
	ld de, wEnemyMonType
	
; copy type 1	
	ld a, [hli]
	ld [de], a
	inc de
	
; copy type 2	
	ld a, [hli]
	ld [de], a
	inc de
	
; skip wEnemyMonCatchRate_NotReferenced
	inc de
	
; moves	
	ld a, [W_ISINBATTLE]
	cp TRAINER_BATTLE
	jr nz, .copyStandardMoves
	
; if it's a trainer battle, copy moves from enemy party data
	ld hl, wEnemyMon1Moves
	ld a, [wWhichPokemon]
	ld bc, wEnemyMon2 - wEnemyMon1
	call AddNTimes
	ld bc, NUM_MOVES
	call CopyData
	jr .loadMovePPs

.copyStandardMoves
; for a wild mon, first copy default moves from the mon header (base moves)
	ld hl, W_MONHMOVES
	ld a, [hli]
	ld [de], a
	inc de
	ld a, [hli]
	ld [de], a
	inc de
	ld a, [hli]
	ld [de], a
	inc de
	ld a, [hl]
	ld [de], a
	dec de
	dec de
	dec de
	
	xor a
	ld [wHPBarMaxHP], a
	
	predef WriteMonMoves ; get moves based on current level ; @@@ dont get the last four, get them at random (based on exp etc)
	; at engine/evos_moves.asm

; PPs	
.loadMovePPs
	ld hl, wEnemyMonMoves
	ld de, wEnemyMonPP
	predef LoadMovePPs
	
; Base stats	
	ld hl, W_MONHBASESTATS
	ld de, wEnemyMonBaseStats
	ld b, NUM_STATS

.copyBaseStatsLoop
	ld a, [hli]
	ld [de], a
	inc de
	dec b
	jr nz, .copyBaseStatsLoop
	
; copy catch rate (the good one)	
	ld hl, W_MONHCATCHRATE
	ld a, [hli]
	ld [de], a
	inc de

; copy base exp
	ld a, [hl]
	ld [de], a

; get mon name	
	ld a, [wEnemyMonSpecies2]
	ld [wd11e], a
	call GetMonName

; copy nick from wcd6d to wEnemyMonNick	
	ld hl, wcd6d
	ld de, wEnemyMonNick
	ld bc, $b
	call CopyData
; get mon's pokedex number	
	ld a, [wEnemyMonSpecies2]
	ld [wd11e], a
	predef IndexToPokedex

; register mon as seen	
	ld a, [wd11e]
	dec a
	ld c, a
	ld b, $1
	ld hl, wPokedexSeen
	predef FlagActionPredef

; copy level and stats from wEnemyMon struct to Unmodified level and stats buffer	
	ld hl, wEnemyMonLevel
	ld de, wEnemyMonUnmodifiedLevel
	ld bc, 1 + NUM_STATS * 2
	call CopyData

; init stat mods	
	ld a, $7 ; default stat mod
	ld b, $8 ; number of stat mods (not needed as many)
	ld hl, wEnemyMonStatMods
.statModLoop
	ld [hli], a
	dec b
	jr nz, .statModLoop
	ret
	
	
; send out a trainer's mon
; set wPlayerMonNumber bit of wPartyGainExpFlags and reset the rest
; set wPlayerMonNumber bit of wPartyFoughtCurrentEnemyFlags and reset the rest
; @@@ not called yet
EnemySendOut: ; 3c90e (f:490e)
	ld hl,wPartyGainExpFlags
	xor a
	ld [hl],a
	ld a,[wPlayerMonNumber]
	ld c,a
	ld b,1
	push bc
	predef FlagActionPredef ; set bit a of [hl]
	ld hl,wPartyFoughtCurrentEnemyFlags
	xor a
	ld [hl],a
	pop bc
	predef FlagActionPredef

; EnemySendOut without changing wPartyGainExpFlags or wPartyFoughtCurrentEnemyFlags
EnemySendOutFirstMon: ; 3c92a (f:492a)
	xor a
	
; clear enemy battstatuses	
	ld hl,W_ENEMYBATTSTATUS1
	ld [hli],a
	ld [hli],a
	ld [hl],a

; clear enemy's disable	and minimize
	ld [W_ENEMYDISABLEDMOVE],a
	ld [wEnemyDisabledMoveNumber],a
	ld [wEnemyHasUsedMinimize],a

; clear p/e used move	
	ld hl,wPlayerUsedMove
	ld [hli],a
	ld [hl],a
	
; player no longer using a trapping move	
	ld hl,W_PLAYERBATTSTATUS1
	res UsingTrappingMove,[hl]
	
; clear p/e damage addresses	
	ld hl, W_PLAYERDAMAGE
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hl], a
	
; clear enemy bide damage
	ld hl, wEnemyBideAccumulatedDamage
	ld [hli], a
	ld [hl], a
	
	hlCoord 18, 0
	ld a,8
	call SlideTrainerPicOffScreen
	call PrintEmptyString
	call SaveScreenTilesToBuffer1
	ld a,[wLinkState]
	cp LINK_STATE_BATTLING
	jr nz,.next

; link battle only
; read which Pokemon the opponent sent out
	ld a,[wSerialExchangeNybbleReceiveData]
	sub 4
	ld [wWhichPokemon],a
	jr .next3
	
; not link battle only	
.next
	ld b,$FF

; find enemy mon in the next position
; @@@ this means, AI automatically sends enemy mons in order
; if it's first mon, wEnemyMonPartyPos is $FF
.next2
	inc b
	ld a,[wEnemyMonPartyPos]
	cp b
	jr z,.next2

; point hl to said next mon and save mon index (0-5) to wWhichPokemon
	ld hl,wEnemyMon1
	ld a,b
	ld [wWhichPokemon],a
	push bc
	ld bc,wEnemyMon2 - wEnemyMon1
	call AddNTimes
	pop bc
	
; if it's fainted, skip this mon
	inc hl
	ld a,[hli]
	ld c,a
	ld a,[hl]
	or c
	jr z,.next2
	
; all trainer battles (link or not)
; read data of the mon the enemy is going to send
.next3
	ld a,[wWhichPokemon]
	ld hl,wEnemyMon1Level
	ld bc,wEnemyMon2 - wEnemyMon1
	call AddNTimes

; level	from the EnemyMons struct
	ld a,[hl]
	ld [W_CURENEMYLVL],a

; species from the wEnemyPartyMons (the six species bytes) struct
	ld a,[wWhichPokemon]
	inc a
	ld hl,wEnemyPartyCount
	ld c,a
	ld b,0
	add hl,bc
	ld a,[hl]
	ld [wEnemyMonSpecies2],a
	ld [wcf91],a
		
	call LoadEnemyMonData

; variables to print a different text based on enemy HP when player sends out mon
; @@@ are these necessary here too?
	ld hl,wEnemyMonHP
	ld a,[hli]
	ld [wcce3],a
	ld a,[hl]
	ld [wcce4],a
	
	ld a,1
	ld [wCurrentMenuItem],a
	
	ld a,[wd11d]
	dec a
	jr z,.next4 ; jump if enemy mon not fainted
	
	ld a,[wPartyCount]
	dec a
	jr z,.next4 ; jump if enemy has no more mons left
	
	ld a,[wLinkState]
	cp LINK_STATE_BATTLING
	jr z,.next4 ; jump if link battle
	
	ld a,[W_OPTIONS]
	bit 6,a
	jr nz,.next4 ; jump if battle style is set

; allow the player to switch mon as enemy sends out another mon
; unless enemy is switching a non-fainted mon
; unless enemy has no more mons left
; unless it's link battle
; unless battle style is set
; @@@ make forced set style?
	ld hl, TrainerAboutToUseText
	call PrintText
	hlCoord 0, 7
	ld bc,$0801
	ld a,TWO_OPTION_MENU
	ld [wTextBoxID],a
	call DisplayTextBoxID
	
	ld a,[wCurrentMenuItem]
	and a
	jr nz,.next4 ; jump because player chose "NO"
	
	ld a,2
	ld [wd07d],a
	call DisplayPartyMenu
	
.next9
	ld a,1
	ld [wCurrentMenuItem],a
	jr c,.next7 ; no Pokemon chosen
	
	ld hl,wPlayerMonNumber
	ld a,[wWhichPokemon]
	cp [hl]
	jr nz,.next6 ; this Pokemon is not already out
	
	ld hl,AlreadyOutText
	call PrintText
.next8
	call GoBackToPartyMenu
	jr .next9 ; go back to chosing another mon
	
.next6
	call HasMonFainted
	jr z,.next8 ; go back to chosing another mon

; player has successfully chosen a mon to switch to	
	xor a
	ld [wCurrentMenuItem],a

; get out of party menu	
.next7 
	call GBPalWhiteOut
	call LoadHudTilePatterns
	call LoadScreenTilesFromBuffer1

; finally, enemy trainer sends out his mon
.next4
	call ClearSprites
	ld hl,wTileMap
	ld bc,$040B
	call ClearScreenArea
	ld b,1
	call GoPAL_SET
	call GBPalNormal
	ld hl,TrainerSentOutText
	call PrintText
	ld a,[wEnemyMonSpecies2]
	ld [wcf91],a
	ld [wd0b5],a
	call GetMonHeader
	ld de,vFrontPic
	call LoadMonFrontSprite
	ld a,$CF
	ld [$FFE1],a
	hlCoord 15, 6
	predef Func_3f073
	ld a,[wEnemyMonSpecies2]
	call PlayCry
	call DrawEnemyHUDAndHPBar
	ld a,[wCurrentMenuItem]
	and a
	ret nz

; player also switched mon	
	xor a
	ld [wPartyGainExpFlags],a
	ld [wPartyFoughtCurrentEnemyFlags],a
	call SaveScreenTilesToBuffer1
	
; updates wPlayerMonNumber and marks new mon as fought current mon and as to receive exp
	jp SwitchPlayerMon


; check W_CUROPPONENT to determine if wild or trainer battle
InitBattle:
	ld a, [W_CUROPPONENT]
	and a
	jr z, WildEncounterTest

; W_CUROPPONENT is non-0 so it's a pending trainer battle	
InitOpponent:
	ld a, [W_CUROPPONENT]
	ld [wcf91], a
	ld [wEnemyMonSpecies2], a
	jr BattleWillOccur

; else there could be a wild battle	
WildEncounterTest:
	ld a, [wNumberOfNoRandomBattleStepsLeft]
	and a
	ret nz
	callab TryDoWildEncounter ; wild_encounters_new.asm
	ret nz
	
BattleWillOccur:
	ld a, [wMapPalOffset]
	push af
	ld hl, wd358
	ld a, [hl]
	push af
	res 1, [hl] ; @@@ what is this for? something related to text
	
	callab InitBattleVariables ; init_battle_variables.asm
	
; wild or trainer battle?	
	ld a, [wEnemyMonSpecies2] ; contains wild mon id from TryDoWildEncounter or trainer class + $c8
	sub $c8
	jp c, InitWildBattle
	
; init trainer battle
	ld [W_TRAINERCLASS], a
	ld a, TRAINER_BATTLE
	ld [W_ISINBATTLE], a
	
	xor a
	ld [wEnemyMonSpecies2], a	
	dec a
	ld [wEnemyMonPartyPos], a ; reset position of cur enemy mon in the enemy party (00 is mon at pos 1)
	
	call GetTrainerInformation ; at home.asm, reads name, pic, money from trainer class at W_TRAINERCLASS
	callab ReadTrainer ; read_trainer_party.asm @@@TODO always full movesets, differentiating item/no item
	call DoBattleTransition ; battle_transitions.asm @@@TODO modify transition conditions?
	call _LoadTrainerPic
	
	jp InitBattle_Common	

; init wild battle
InitWildBattle:
	xor a
	ld [W_TRAINERCLASS], a
	inc a ; ld a, WILD_BATTLE
	ld [W_ISINBATTLE], a
	
	call LoadEnemyMonData ; @@@TODO work on this
	call DoBattleTransition
	ld de, vFrontPic
	call LoadMonFrontSprite ; load mon sprite

; common for wild and trainer battle	
InitBattle_Common:
	xor a
	ld [$ffe1], a ; this is used for pics stuff?	
	hlCoord 12, 0
	predef Func_3f0c6 ; sprite stuff apparently

; draw sprites, clean screen and related stuff	
	call LoadPlayerBackPic
	ld b, $0
	call SlidePlayerAndEnemySilhouettesOnScreen ; @@@ could use a faster sliding
	xor a
	ld [H_AUTOBGTRANSFERENABLED], a
	ld hl, .emptyString
	call PrintText
	call SaveScreenTilesToBuffer1
	call ClearScreen
	ld a, $98
	ld [$ffbd], a
	ld a, $1
	ld [H_AUTOBGTRANSFERENABLED], a
	call Delay3
	ld a, $9c
	ld [$ffbd], a
	call LoadScreenTilesFromBuffer1
	hlCoord 9, 7
	ld bc, $50a
	call ClearScreenArea
	hlCoord 1, 0
	ld bc, $40a
	call ClearScreenArea
	call ClearSprites
	
	ld a, [W_ISINBATTLE]
	dec a ; is it a wild battle?
	call z, DrawEnemyHUDAndHPBar ; draw enemy HUD and HP bar if it's a wild battle
	; because the enemy is already a mon pic, not a trainer pic
	
	call StartBattle ; battle engine actual starting point (executes main loop)
	
; end of battle	and return out of core.asm
	callab EndOfBattle

	pop af
	ld [wd358], a
	pop af
	ld [wMapPalOffset], a
	ld a, [wd0d4]
	ld [hTilesetType], a
	scf
	ret

.emptyString
	db "@"
	

StartBattle:
	ld a, [W_ISINBATTLE]
	dec a
	jr z, .goAhead

; find enemy trainer's first alive mon	
	ld hl, wEnemyMon1HP
	ld bc, wEnemyMon2 - wEnemyMon1 - 1
	ld d, $4 - 1

; @@@ shouldn't the first alive mon always be the first mon at this point?	
.findFirstAliveEnemyMonLoop
	inc d
	ld a, [hli]
	or [hl]
	jr nz, .foundFirstAliveEnemyMon
	add hl, bc
	jr .findFirstAliveEnemyMonLoop

.foundFirstAliveEnemyMon
	ld a, d
	ld [wSerialExchangeNybbleReceiveData], a ; 4-9 for mons 1-6
	
; enemy trainer sends out enemy mon	
	call EnemySendOutFirstMon
	
.goAhead
	ld c, 40
	call DelayFrames
	call SaveScreenTilesToBuffer1
	
.checkAnyPartyAlive
	call AnyPartyAlive
	ld a, d
	and a
	jp z, HandlePlayerBlackOut ; jump if no mon is alive

	call LoadScreenTilesFromBuffer1
	ld a, [W_BATTLETYPE]
	and a ; is it a normal battle?
	jp z, .playerSendOutFirstMon ; if so, send out player mon	
	
; safari zone battle
.displaySafariZoneBattleMenu
	call DisplayBattleMenu
	ret c ; return if the player ran from battle
	ld a, [wcd6a]
	and a ; was the item used successfully?
	jr z, .displaySafariZoneBattleMenu ; if not, display the menu again; XXX does this ever jump?
	ld a, [W_NUMSAFARIBALLS]
	and a
	jr nz, .notOutOfSafariBalls
	call LoadScreenTilesFromBuffer1
	ld hl, .outOfSafariBallsText
	jp PrintText
.notOutOfSafariBalls
	callab PrintSafariZoneBattleText
	ld a, [wEnemyMonSpeed + 1]
	add a
	ld b, a ; init b (which is later compared with random value) to (enemy speed % 256) * 2
	jp c, EnemyRan ; if (enemy speed % 256) > 127, the enemy runs
	ld a, [wSafariBaitFactor]
	and a ; is bait factor 0?
	jr z, .checkEscapeFactor
; bait factor is not 0
; divide b by 4 (making the mon less likely to run)
	srl b
	srl b
.checkEscapeFactor
	ld a, [wSafariEscapeFactor]
	and a ; is escape factor 0?
	jr z, .compareWithRandomValue
; escape factor is not 0
; multiply b by 2 (making the mon more likely to run)
	sla b
	jr nc, .compareWithRandomValue
; cap b at 255
	ld b, $ff
.compareWithRandomValue
	call Random
	cp b
	jr nc, .checkAnyPartyAlive
	jp EnemyRan ; if b was greater than the random value, the enemy runs

.outOfSafariBallsText
	TX_FAR _OutOfSafariBallsText
	db "@"

; player sends his first alive mon
.playerSendOutFirstMon
	xor a
	ld [wWhichPokemon], a

.findFirstAliveMonLoop
	call HasMonFainted
	jr nz, .foundFirstAliveMon
	
; this mon is fainted, go to the next one
	ld hl, wWhichPokemon
	inc [hl]
	jr .findFirstAliveMonLoop
	
.foundFirstAliveMon
	ld a, [wWhichPokemon]
	ld [wPlayerMonNumber], a

; save species into wcf91 and wBattleMonSpecies2	
	ld hl, wPartySpecies
	ld c, a
	ld b, 0
	add hl, bc
	ld a, [hl]
	ld [wcf91], a
	ld [wBattleMonSpecies2], a
	
	call LoadScreenTilesFromBuffer1
	hlCoord 1, 5
	ld a, $9
	call SlideTrainerPicOffScreen
	call SaveScreenTilesToBuffer1

; set player mon exp flags for this enemy mon
	ld a, [wWhichPokemon]
	ld c, a
	ld b, $1
	push bc
	ld hl, wPartyGainExpFlags
	predef FlagActionPredef
	ld hl, wPartyFoughtCurrentEnemyFlags
	pop bc
	predef FlagActionPredef
	
	call LoadBattleMonFromParty
	call LoadScreenTilesFromBuffer1
	call SendOutMon
;	jr MainInBattleLoop
	
MainInBattleLoop:
; update mon's hp and status in party struct
	call ReadPlayerMonCurHPAndStatus

; did player mon faint?	
	ld hl, wBattleMonHP
	ld a, [hli]
	or [hl]
	jp z, HandlePlayerMonFainted

; did enemy mon faint?	
	ld hl, wEnemyMonHP
	ld a, [hli]
	or [hl]
	jp z, HandleEnemyMonFainted
	
	call SaveScreenTilesToBuffer1
	xor a
	ld [wd11d], a

	ld a, [wLinkState]
	sub LINK_STATE_BATTLING
	jr z, .displayBattleMenu
	
	call SelectEnemyMove ; Not Link Battle Only
	
.displayBattleMenu
; display battle menu to select player move	
	call DisplayBattleMenu ; returns (FIGHT), displays party menu (POKEMON), or bag menu (ITEM)
	ret c ; return if player ran from battle (RUN)
	
.selectPlayerMove
; if player wasted his turn switching/sending mon this turn, or attempting to run away, 
; player can't select or execute move
	ld a, [wcd6a]
	and a
	ld a, $ff
	ld [wPlayerSelectedMove], a
	jr nz, .selectEnemyMove
	
	inc a ; 0
	ld [wMoveMenuType], a
	ld [wMenuItemToSwap], a	
	inc a ; 1
	ld [W_ANIMATIONID], a
	
	call MoveSelectionMenu
	push af
	call LoadScreenTilesFromBuffer1
	call DrawHUDsAndHPBars
	pop af
	jr nz, MainInBattleLoop ; if the player didn't select a move, jump back
	
.selectEnemyMove
; return selected move at wEnemySelectedMove
; random for wild battles
; chosen by AI in trainer battles
	ld a, [wLinkState]
	cp LINK_STATE_BATTLING
	jr nz, .compareSpeed

; link battle	
	call SelectEnemyMove ; Link Battle Only
	
	ld a, [wSerialExchangeNybbleReceiveData]
	cp $f
	jp z, EnemyRan
	cp $e
	jr z, .compareSpeed
	cp $d
	jr z, .compareSpeed
	sub $4
	jr c, .compareSpeed

; if we make it here, enemy link trainer switched mons
	callab SwitchEnemyMon ; trainer_ai.asm, calls EnemySendOut
	
.compareSpeed
	ld de, wBattleMonSpeed 
	ld hl, wEnemyMonSpeed
	ld c, $2
	call StringCmp
	jr z, .speedEqual
	jr nc, .playerMovesFirst
	jr .enemyMovesFirst
	
.speedEqual ; 50/50 chance for both players
; @@@ is invertOutcome needed for link battles?
	ld a, [$ffaa]
	cp $2
	jr z, .invertOutcome
	call BattleRandom
	cp $80
	jr c, .playerMovesFirst
	jr .enemyMovesFirst
.invertOutcome
	call BattleRandom
	cp $80
	jr c, .enemyMovesFirst
	jr .playerMovesFirst
	
.enemyMovesFirst
; set opponent turn
	ld a, $1
	ld [H_WHOSETURN], a
; execute enemy's selected move
; returns b=0 if player mon fainted, b=1 otherwise
	call ExecuteEnemyMove ; @@@TODO
	ld a, b
	and a
	jp z, HandlePlayerMonFainted
	call DrawHUDsAndHPBars

; set player turn
	xor a
	ld [H_WHOSETURN], a
; execute player's selected move
; returns b=0 if enemy mon fainted, b=1 otherwise	
	call ExecutePlayerMove	; @@@TODO
	ld a, b
	and a
	jp z, HandleEnemyMonFainted
	call DrawHUDsAndHPBars
	
	jp MainInBattleLoop
	
.playerMovesFirst
; set player turn
	xor a
	ld [H_WHOSETURN], a
; execute player's selected move
; returns b=0 if enemy mon fainted, b=1 otherwise
	call ExecutePlayerMove
	ld a, b
	and a
	jp z, HandleEnemyMonFainted
	call DrawHUDsAndHPBars
	
; set opponent turn
	ld a, $1
	ld [H_WHOSETURN], a
; execute enemy's selected move
; returns b=0 if player mon fainted, b=1 otherwise
	call ExecuteEnemyMove
	ld a, b
	and a
	jp z, HandlePlayerMonFainted
	call DrawHUDsAndHPBars

	jp MainInBattleLoop
	
ExecutePlayerMove:
; clear player damage
	ld hl, W_PLAYERDAMAGE
	xor a
	ld [hli], a
	ld [hl], a

; can execute move?	
	ld a, [wPlayerSelectedMove]
	inc a
	jp z, ExecutePlayerMoveDone

; init move feedback addresses, critical hit flag and damage multipliers	
	call InitTurnVariables

; check player status conditions
	call CheckPlayerStatusConditions

; get selected move name and load its data to wram
	call GetCurrentMove
	call PrintMonName1Text

; decrement PP of move at de (selected move)
	ld hl,DecrementPP
	ld de,wPlayerSelectedMove ; pointer to the move just used
	ld b,BANK(DecrementPP)
	call Bankswitch
	
; accuracy test
	call MoveHitTest	

; branch ahead the move missed
	ld a,[W_MOVEMISSED]
	and a
	jr nz, .printMoveFailureText
	
; damage calculation (skip if non-damaging move)
	ld a, [W_PLAYERMOVEPOWER]
	and a
	jr z, .playAnimation
	
	call CriticalHitTest
	call GetDamageVarsForPlayerAttack
	; changed formula, includes type matchup and stab bonuses for more accuracy
	call CalculateDamage
	ld a,[W_MOVEMISSED]
	and a
	jr nz, .printMoveFailureText
	; doubles damage if critical hit
	call OtherModifiers
	; adds 1 to damage and multiplies damage by %111RRRR0 / 255, where R is a random bit (min 224/255 - max 254/255)	
	call RandomizeDamage

; play animation
.playAnimation
	ld a, 4 ; animation BlinkEnemyMonSprite
	ld [wAnimationType],a
	ld a,[W_PLAYERMOVENUM]
	call PlayMoveAnimation
	call DrawPlayerHUDAndHPBar
	jr .moveDidNotMiss

; print that the move missed if it did
.printMoveFailureText
	call PrintMoveFailureText
	jr ExecutePlayerMoveDone
	
; apply attack if the move didn't miss
.moveDidNotMiss
	call ApplyAttackToEnemyPokemon
	call PrintCriticalOHKOText
	callab DisplayEffectiveness
	ld a,1
	ld [wMoveDidntMiss],a

; did the enemy faint? return b=0 if so
	ld hl,wEnemyMonHP	
	ld a,[hli]
	ld b,[hl]
	or b
	ret z	

ExecutePlayerMoveDone:
; return 0 at wcd6a and b=1 if enemy didn't faint
	xor a
	ld [wcd6a],a
	ld b,1
	ret	
	

ExecuteEnemyMove:
; clear enemy damage
	ld hl, W_ENEMYDAMAGE
	xor a
	ld [hli], a
	ld [hl], a

; can execute move?	
	ld a, [wEnemySelectedMove]
	inc a
	jp z, ExecuteEnemyMoveDone

; link battle stuff	
	ld a, [wLinkState]
	cp LINK_STATE_BATTLING
	jr nz, .executeEnemyMove
	ld b, $1
	ld a, [wSerialExchangeNybbleReceiveData]
	cp $e
	jr z, .executeEnemyMove
	cp $4
	ret nc

.executeEnemyMove	
; init move feedback addresses, critical hit flag and damage multipliers	
	call InitTurnVariables

; check enemy status conditions
	call CheckEnemyStatusConditions

; get selected move name and load its data to wram
	call GetCurrentMove
	call PrintMonName1Text
	
; decrement selected move PP @@@
	
; accuracy test
	call MoveHitTest

; branch ahead if the move missed
	ld a,[W_MOVEMISSED]
	and a
	jr nz, .printMoveFailureText
	
; damage calculation (skip if non-damaging move)
	ld a, [W_ENEMYMOVEPOWER]
	and a
	jr z, .playAnimation
	
	call CriticalHitTest
	call GetDamageVarsForEnemyAttack
	call CalculateDamage
	ld a,[W_MOVEMISSED]
	and a
	jr nz, .printMoveFailureText
	call OtherModifiers	
	call RandomizeDamage

.playAnimation	
; play animation
	ld a, 1 ; animation ShakeScreenVertically
	ld [wAnimationType],a
	ld a, [W_ENEMYMOVENUM]
	call PlayMoveAnimation
	call DrawEnemyHUDAndHPBar
	jr .moveDidNotMiss

; print that the move missed if it did
.printMoveFailureText
	call PrintMoveFailureText
	jr ExecuteEnemyMoveDone
	
; apply attack if the move didn't miss
.moveDidNotMiss
	call ApplyAttackToPlayerPokemon
	call PrintCriticalOHKOText
	callab DisplayEffectiveness
	ld a,1
	ld [wMoveDidntMiss],a

; did the player faint? return b=0 if so		
	ld hl, wBattleMonHP
	ld a, [hli]
	ld b, [hl]
	or b
	ret z	

; return b=1 if player didn't faint	
ExecuteEnemyMoveDone:
	ld b, $1
	ret

	
CheckPlayerStatusConditions:
	ret
	
	
CheckEnemyStatusConditions:
	ret	
