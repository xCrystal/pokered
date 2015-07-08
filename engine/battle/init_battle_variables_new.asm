InitBattleVariables: ; 525af (14:65af)
	ld a, [hTilesetType]
	ld [wd0d4], a
	
	xor a

; clear wcc2b, wcc2c, wcc2d, wPlayerMoveListIndex, wPlayerMonNumber
	ld hl, wcc2b
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a	
	ld [hl], a

; clear all addresses from wccd3 to wEnemyMonEvasionMod (cd33)
; includes sub's HP, p/e selected/used move, Pay Day money, safari factors, 
; p/e disabled move number, wMoveDidntMiss, wEnemyNumHits, 
; enemy Bide accum. damage, p/e unmodified stats, and player mon mods. 
	ld hl, wccd3
	ld b, wEnemyMonEvasionMod + 1 - wccd3
.loop
	ld [hli], a
	dec b
	jr nz, .loop
	
; clear all addresses from W_ENEMYMOVENUM (cfcc) to wBattleMon (d030) 
; includes p/e move structs, enemy mon nick, (cur/wild) enemy mon struct,
; battle mon nick, and battle mon struct.
	ld hl, W_ENEMYMOVENUM
	ld b, W_TRAINERCLASS - W_ENEMYMOVENUM
.loop2
	ld [hli], a
	dec b
	jr nz, .loop2
	
; clear all addresses from wPartyGainExpFlags to wPlayerBideAccumulatedDamage (d074) 
; includes wPartyGainExpFlags, wDamageMultipliers wCriticalHitOrOHKO,
; W_MOVEMISSED, p/e stats to double, p/e battstatuses, 
; p/e confused/toxic counters, p/e disabled move, 
; p/e num hits/attacks left, and player bide accum. damage.
	ld hl, wPartyGainExpFlags
	ld b, wPlayerBideAccumulatedDamage + 1 - wPartyGainExpFlags
.loop3
	ld [hli], a
	dec b
	jr nz, .loop3
	
; clear enemy mons (trainer party) struct	
	ld hl, wEnemyMons
	ld b, wEnemyMon6 - wEnemyMons
.loop4
	ld [hli], a
	dec b
	jr nz, .loop4
	ld hl, wEnemyMon6
	ld b, W_TRAINERHEADERPTR - wEnemyMon6
.loop5
	ld [hli], a
	dec b
	jr nz, .loop5

; clear p/e damage addresses	
	ld hl, W_PLAYERDAMAGE
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hl], a	
	
; clear other addresses
	ld [wcd6a], a
	ld [wBattleResult], a
	ld [wNumRunAttempts], a
	ld [wListScrollOffset], a
	ld [wEscapedFromBattle], a
	ld [wMapPalOffset], a
	ld hl, wcf1d
	ld [hli], a
	ld [hl], a

; init wccd9 to 1
	inc a
	ld [wccd9], a

; test if safari battle	based on player location
	ld a, [W_CURMAP] 
	cp SAFARI_ZONE_EAST
	jr c, .notSafariBattle
	cp SAFARI_ZONE_REST_HOUSE_1
	jr nc, .notSafariBattle
	ld a, $2 ; safari battle
	ld [W_BATTLETYPE], a
.notSafariBattle
	ld hl, PlayBattleMusic
	ld b, BANK(PlayBattleMusic)
	jp Bankswitch
