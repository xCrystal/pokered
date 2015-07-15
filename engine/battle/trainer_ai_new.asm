; creates a set of moves that may be used and returns its address in hl
; unused slots are filled with 0, all used slots may be chosen with equal probability
AIEnemyTrainerChooseMoves: ; 39719 (e:5719)
	ld a, $a
	ld hl, wHPBarMaxHP  ; init temporary move selection array. Only the moves with the lowest numbers are chosen in the end
	ld [hli], a   ; move 1
	ld [hli], a   ; move 2
	ld [hli], a   ; move 3
	ld [hl], a    ; move 4
	ld a, [W_ENEMYDISABLEDMOVE] ; forbid disabled move (if any)
	swap a
	and $f
	jr z, .noMoveDisabled
	ld hl, wHPBarMaxHP
	dec a
	ld c, a
	ld b, $0
	add hl, bc    ; advance pointer to forbidden move
	ld [hl], $50  ; forbid (highly discourage) disabled move
.noMoveDisabled
	ld hl, TrainerClassMoveChoiceModifications ; 589B
	ld a, [W_TRAINERCLASS]
	ld b, a
.loopTrainerClasses
	dec b
	jr z, .readTrainerClassData
.loopTrainerClassData
	ld a, [hli]
	and a
	jr nz, .loopTrainerClassData
	jr .loopTrainerClasses
.readTrainerClassData
	ld a, [hl]
	and a
	jp z, .useOriginalMoveSet
	push hl
.nextMoveChoiceModification
	pop hl
	ld a, [hli]
	and a
	jr z, .loopFindMinimumEntries
	push hl
	ld hl, AIMoveChoiceModificationFunctionPointers ; $57a3
	dec a
	add a
	ld c, a
	ld b, $0
	add hl, bc    ; skip to pointer
	ld a, [hli]   ; read pointer into hl
	ld h, [hl]
	ld l, a
	ld de, .nextMoveChoiceModification  ; set return address
	push de
	jp [hl]       ; execute modification function
.loopFindMinimumEntries ; all entries will be decremented sequentially until one of them is zero
	ld hl, wHPBarMaxHP  ; temp move selection array
	ld de, wEnemyMonMoves  ; enemy moves
	ld c, $4
.loopDecrementEntries
	ld a, [de]
	inc de
	and a
	jr z, .loopFindMinimumEntries
	dec [hl]
	jr z, .minimumEntriesFound
	inc hl
	dec c
	jr z, .loopFindMinimumEntries
	jr .loopDecrementEntries
.minimumEntriesFound
	ld a, c
.loopUndoPartialIteration ; undo last (partial) loop iteration
	inc [hl]
	dec hl
	inc a
	cp $5
	jr nz, .loopUndoPartialIteration
	ld hl, wHPBarMaxHP  ; temp move selection array
	ld de, wEnemyMonMoves  ; enemy moves
	ld c, $4
.filterMinimalEntries ; all minimal entries now have value 1. All other slots will be disabled (move set to 0)
	ld a, [de]
	and a
	jr nz, .moveExisting ; 0x3978a $1
	ld [hl], a
.moveExisting
	ld a, [hl]
	dec a
	jr z, .slotWithMinimalValue
	xor a
	ld [hli], a     ; disable move slot
	jr .next
.slotWithMinimalValue
	ld a, [de]
	ld [hli], a     ; enable move slot
.next
	inc de
	dec c
	jr nz, .filterMinimalEntries
	ld hl, wHPBarMaxHP    ; use created temporary array as move set
	ret
.useOriginalMoveSet
	ld hl, wEnemyMonMoves    ; use original move set
	ret

AIMoveChoiceModificationFunctionPointers: ; 397a3 (e:57a3)
	dw AIMoveChoiceModification1
	dw AIMoveChoiceModification2
	dw AIMoveChoiceModification3
	dw AIMoveChoiceModification4 ; unused, does nothing

; discourages moves that cause no damage but only a status ailment if player's mon already has one
AIMoveChoiceModification1: ; 397ab (e:57ab)
	ld a, [wBattleMonStatus]
	and a
	ret z ; return if no status ailment on player's mon
	ld hl, wBuffer - 1 ; temp move selection array (-1 byte offest)
	ld de, wEnemyMonMoves ; enemy moves
	ld b, NUM_MOVES + 1
.nextMove
	dec b
	ret z ; processed all 4 moves
	inc hl
	ld a, [de]
	and a
	ret z ; no more moves in move set
	inc de
	call ReadMove
	ld a, [W_ENEMYMOVEPOWER]
	and a
	jr nz, .nextMove
	ld a, [W_ENEMYMOVEEFFECT]
	push hl
	push de
	push bc
	ld hl, StatusAilmentMoveEffects
	ld de, $0001
	call IsInArray
	pop bc
	pop de
	pop hl
	jr nc, .nextMove
	ld a, [hl]
	add $5 ; heavily discourage move
	ld [hl], a
	jr .nextMove

StatusAilmentMoveEffects ; 57e2
	db $01 ; unused sleep effect
	db SLEEP_EFFECT
	db POISON_EFFECT
	db PARALYZE_EFFECT
	db $FF

; slightly encourage moves with specific effects.
; in particular, stat-modifying moves and other move effects
; that fall in-bewteen
AIMoveChoiceModification2: ; 397e7 (e:57e7)
	ld hl, wBuffer - 1 ; temp move selection array (-1 byte offset)
	ld de, wEnemyMonMoves ; enemy moves
	ld b, NUM_MOVES + 1
.nextMove
	dec b
	ret z ; processed all 4 moves
	inc hl
	ld a, [de]
	and a
	ret z ; no more moves in move set
	inc de
	call ReadMove
	ld a, [W_ENEMYMOVEEFFECT]
	cp ATTACK_UP1_EFFECT
	jr c, .nextMove
	cp BIDE_EFFECT
	jr c, .preferMove
	cp ATTACK_UP2_EFFECT
	jr c, .nextMove
	cp POISON_EFFECT
	jr c, .preferMove
	jr .nextMove
.preferMove
	dec [hl] ; sligthly encourage this move
	jr .nextMove

; encourages moves that are effective against the player's mon (even if non-damaging).
; discourage damaging moves that are ineffective or not very effective against the player's mon,
; unless there's no damaging move that deals at least neutral damage
AIMoveChoiceModification3: ; 39817 (e:5817)
	ld hl, wBuffer - 1 ; temp move selection array (-1 byte offset)
	ld de, wEnemyMonMoves ; enemy moves
	ld b, $5
.nextMove
	dec b
	ret z ; processed all 4 moves
	inc hl
	ld a, [de]
	and a
	ret z ; no more moves in move set
	inc de
	call ReadMove
	push hl
	push bc
	push de
	callab AIGetTypeEffectiveness
	pop de
	pop bc
	pop hl
	ld a, [wd11e]
	cp $10
	jr z, .nextMove
	jr c, .notEffectiveMove
	dec [hl] ; sligthly encourage this move
	jr .nextMove
.notEffectiveMove ; discourages non-effective moves if better moves are available
	push hl
	push de
	push bc
	ld a, [W_ENEMYMOVETYPE]
	ld d, a
	ld hl, wEnemyMonMoves  ; enemy moves
	ld b, NUM_MOVES + 1
	ld c, $0
.loopMoves
	dec b
	jr z, .done
	ld a, [hli]
	and a
	jr z, .done
	call ReadMove
	ld a, [W_ENEMYMOVEEFFECT]
	cp SUPER_FANG_EFFECT
	jr z, .betterMoveFound ; Super Fang is considered to be a better move
	cp SPECIAL_DAMAGE_EFFECT
	jr z, .betterMoveFound ; any special damage moves are considered to be better moves
	cp FLY_EFFECT
	jr z, .betterMoveFound ; Fly is considered to be a better move
	ld a, [W_ENEMYMOVETYPE]
	cp d
	jr z, .loopMoves
	ld a, [W_ENEMYMOVEPOWER]
	and a
	jr nz, .betterMoveFound ; damaging moves of a different type are considered to be better moves
	jr .loopMoves
.betterMoveFound
	ld c, a
.done
	ld a, c
	pop bc
	pop de
	pop hl
	and a
	jr z, .nextMove
	inc [hl] ; sligthly discourage this move
	jr .nextMove
AIMoveChoiceModification4: ; 39883 (e:5883)
	ret

ReadMove: ; 39884 (e:5884)
	push hl
	push de
	push bc
	dec a
	ld hl,Moves
	ld bc,6
	call AddNTimes
	ld de,W_ENEMYMOVENUM
	call CopyData
	pop bc
	pop de
	pop hl
	ret

; move choice modification methods that are applied for each trainer class
; 0 is sentinel value
TrainerClassMoveChoiceModifications: ; 3989b (e:589b)
	db 0      ; YOUNGSTER
	db 1,0    ; BUG CATCHER
	db 1,0    ; LASS
	db 1,3,0  ; SAILOR
	db 1,0    ; JR__TRAINER_M
	db 1,0    ; JR__TRAINER_F
	db 1,2,3,0; POKEMANIAC
	db 1,2,0  ; SUPER_NERD
	db 1,0    ; HIKER
	db 1,0    ; BIKER
	db 1,3,0  ; BURGLAR
	db 1,0    ; ENGINEER
	db 1,2,0  ; JUGGLER_X
	db 1,3,0  ; FISHER
	db 1,3,0  ; SWIMMER
	db 0      ; CUE_BALL
	db 1,0    ; GAMBLER
	db 1,3,0  ; BEAUTY
	db 1,2,0  ; PSYCHIC_TR
	db 1,3,0  ; ROCKER
	db 1,0    ; JUGGLER
	db 1,0    ; TAMER
	db 1,0    ; BIRD_KEEPER
	db 1,0    ; BLACKBELT
	db 1,0    ; SONY1
	db 1,3,0  ; PROF_OAK
	db 1,2,0  ; CHIEF
	db 1,2,0  ; SCIENTIST
	db 1,3,0  ; GIOVANNI
	db 1,0    ; ROCKET
	db 1,3,0  ; COOLTRAINER_M
	db 1,3,0  ; COOLTRAINER_F
	db 1,0    ; BRUNO
	db 1,0    ; BROCK
	db 1,3,0  ; MISTY
	db 1,3,0  ; LT__SURGE
	db 1,3,0  ; ERIKA
	db 1,3,0  ; KOGA
	db 1,3,0  ; BLAINE
	db 1,3,0  ; SABRINA
	db 1,2,0  ; GENTLEMAN
	db 1,3,0  ; SONY2
	db 1,3,0  ; SONY3
	db 1,2,3,0; LORELEI
	db 1,0    ; CHANNELER
	db 1,0    ; AGATHA
	db 1,3,0  ; LANCE

INCLUDE "engine/battle/trainer_pic_money_pointers.asm"
	
INCLUDE "text/trainer_names.asm"	

INCLUDE "engine/battle/bank_e_misc.asm"

INCLUDE "engine/battle/read_trainer_party.asm"

INCLUDE "data/trainer_moves.asm"

INCLUDE "data/trainer_parties.asm"

;TrainerAI: ; 3a52e (e:652e)
;XXX called at 34964, 3c342, 3c398

SwitchEnemyMon: ; 3a74b (e:674b)

; prepare to withdraw the active monster: copy hp, number, and status to roster

	ld a,[wEnemyMonPartyPos]
	ld hl,wEnemyMon1HP
	ld bc,wEnemyMon2 - wEnemyMon1
	call AddNTimes
	ld d,h
	ld e,l
	ld hl,wEnemyMonHP
	ld bc,4
	call CopyData

	ld hl, AIBattleWithdrawText
	call PrintText

	ld a,1
	ld [wd11d],a
	callab EnemySendOut
	xor a
	ld [wd11d],a

	ld a,[wLinkState]
	cp LINK_STATE_BATTLING
	ret z
	scf
	ret

AIBattleWithdrawText: ; 3a781 (e:6781)
	TX_FAR _AIBattleWithdrawText
	db "@"