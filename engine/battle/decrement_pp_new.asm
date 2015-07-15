; decrement pp of move at de in player party
DecrementPP: ; 68000 (1a:4000)

; Don't do anything for STRUGGLE
	ld a, [de]
	cp a, STRUGGLE
	ret z
	
; decrement PP in the battle struct
	ld hl, wBattleMonPP
	call .DecrementPP    
	
; decrement PP in the party struct
	ld hl, wPartyMon1PP
	ld a, [wPlayerMonNumber]
	ld bc, wPartyMon2 - wPartyMon1
	call AddNTimes

.DecrementPP
; decrement PP of move at wPlayerMoveListIndex
	ld a, [wPlayerMoveListIndex]
	ld c, a
	ld b, 0
	add hl ,bc
	dec [hl]
	ret
