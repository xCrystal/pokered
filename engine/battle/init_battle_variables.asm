InitBattleVariables: ; 525af (14:65af)
; clear cd6a, cc2b, cc2c, cc2d, cc2e, cf1d, cf1e, ccd3-cd0e,
; wBattleResult, wListScrollOffset, wCriticalHitOrOHKO, wBattleMonSpecies,
; wPartyGainExpFlags, wPlayerMonNumber, wEscapedFromBattle, wMapPalOffset.
; load 1 into ccd9
; check if safari battle
; @@@TODO add wDamage and... 
	ld a, [hTilesetType]
	ld [wd0d4], a
	xor a
	ld [wcd6a], a
	ld [wBattleResult], a
	ld hl, wcc2b
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hl], a
	ld [wListScrollOffset], a
	ld [wCriticalHitOrOHKO], a
	ld [wBattleMonSpecies], a
	ld [wPartyGainExpFlags], a
	ld [wPlayerMonNumber], a 
	ld [wEscapedFromBattle], a
	ld [wMapPalOffset], a
	ld hl, wcf1d
	ld [hli], a
	ld [hl], a
	ld hl, wccd3
	ld b, wPlayerMonUnmodifiedLevel - wccd3
.loop
	ld [hli], a
	dec b
	jr nz, .loop
	inc a
	ld [wccd9], a
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
