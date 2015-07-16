; decrement pp of move at de in player party
DecrementPP: ; 68000 (1a:4000)

; Don't do anything for STRUGGLE
	ld a, [de]
	cp a, STRUGGLE
	ret z
	
	ld a, [H_WHOSETURN]
	and a
	jr nz, .enemyTurn
	
; decrement PP in the battle struct
	ld hl, wBattleMonPP
	call .DecrementPPplayer
	
; decrement PP in the party struct
	ld hl, wPartyMon1PP
	ld a, [wPlayerMonNumber]
	ld bc, wPartyMon2 - wPartyMon1
	call AddNTimes
	jr .DecrementPPplayer

.enemyTurn	
; decrement PP in the battle struct
	ld hl, wEnemyMonPP
	call .DecrementPPenemy
	
; decrement PP in the party struct
	ld hl, wEnemyMon1PP
	ld a, [wEnemyMonPartyPos]
	ld bc, wEnemyMon2 - wEnemyMon1
	call AddNTimes
;	jr .DecrementPPenemy

.DecrementPPenemy
	ld a, [wEnemyMoveListIndex]
	jr .decrementPP
.DecrementPPplayer
; decrement PP of move at wPlayerMoveListIndex
	ld a, [wPlayerMoveListIndex]
.decrementPP	
	ld c, a
	ld b, 0
	add hl ,bc
	dec [hl]
	ret
