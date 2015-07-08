; @@@TODO
; modify encounter values
; possibly add more/different matching tiles (eventually)
; repel prevents encounters if the leading party mon's level is higher than the wild mon

CheckIfCanEncounter:
	ld c, [hl]
	ld a, [W_GRASSTILE]
	cp c
	ld a, [W_GRASSRATE]
	ret z
	ld a, $14 ; in all tilesets with a water tile, this is its id
	cp c
	ld a, [W_WATERRATE]
	ret	

; try to initiate a wild pokemon encounter
; returns success in Z
TryDoWildEncounter: ; 13870 (4:7870)
	ld a, [wNPCMovementScriptPointerTableNum]
	and a
	ret nz
	ld a, [wd736]
	and a
	ret nz
	callab IsPlayerStandingOnDoorTileOrWarpTile
	jr nc, .notStandingOnDoorOrWarpTile
.CantEncounter
	ld a, $1
	and a
	ret
.notStandingOnDoorOrWarpTile
	callab IsPlayerJustOutsideMap
	jr z, .CantEncounter
	ld a, [wRepelRemainingSteps]
	and a
	jr z, .determineIfCanEncounter
	dec a
	jr z, .lastRepelStep
	ld [wRepelRemainingSteps], a
	
.determineIfCanEncounter
; @@@
	hlCoord 9, 9
	call CheckIfCanEncounter
	jr z, .CanEncounter
	hlCoord 8, 8
	call CheckIfCanEncounter
	jr z, .CanEncounter
	
; can encounter pokemon if
; the map is "indoor" and has wild pokemon defined.
; exceptions are Viridian Forest and Safari Zone.
	ld a, [W_CURMAP]
	cp REDS_HOUSE_1F ; is this an indoor map (REDS_HOUSE_1F or higher id)?
	jr c, .CantEncounter2
	
	ld a, [W_CURMAPTILESET]
	cp FOREST ; Viridian Forest/Safari Zone
	jr z, .CantEncounter2
	ld a, [W_GRASSRATE]
	
.CanEncounter
; compare encounter chance with a random number to determine if there will be an encounter
	ld b, a
	ld a, [hRandomAdd]
	cp b
	jr nc, .CantEncounter2
	ld a, [hRandomSub]
	ld b, a
	ld hl, WildMonEncounterSlotChances
.determineEncounterSlot
	ld a, [hli]
	cp b
	jr nc, .gotEncounterSlot
	inc hl
	jr .determineEncounterSlot

.gotEncounterSlot
; determine which wild pokemon (grass or water) can appear in the half-block we're standing in
; @@@
	ld c, [hl]
	ld hl, W_GRASSMONS
	aCoord 9, 9
	cp $14 ; water tile id?
	jr z, .waterEncounter
	aCoord 8, 8
	cp $14 ; water tile id?
	jr nz, .gotWildEncounterType ; else, it's treated as a grass tile by default
.waterEncounter	
	ld hl, W_WATERMONS

; load mon number and level into ram and do repel check	
.gotWildEncounterType
	ld b, $0
	add hl, bc
	ld a, [hli]
	ld [W_CURENEMYLVL], a
	ld a, [hl]
	ld [wcf91], a
	ld [wEnemyMonSpecies2], a

; repel check	
	ld a, [wRepelRemainingSteps]
	and a
	jr z, .willEncounter

; repel prevents encounters if the leading party mon's level is higher than the wild mon	
	ld a, [wPartyMon1Level]
	ld b, a
	ld a, [W_CURENEMYLVL]
	cp b
	jr c, .CantEncounter2
	jr .willEncounter

; print repel is off	
.lastRepelStep
	ld [wRepelRemainingSteps], a
	ld a, $d2
	ld [H_DOWNARROWBLINKCNT2], a
	call EnableAutoTextBoxDrawing
	call DisplayTextID
	
.CantEncounter2 ; return nz 
	ld a, $1
	and a
	ret
.willEncounter ; return z
	xor a
	ret

WildMonEncounterSlotChances: ; 13918 (4:7918)
; There are 10 slots for wild pokemon, and this is the table that defines how common each of
; those 10 slots is. A random number is generated and then the first byte of each pair in this
; table is compared against that random number. If the random number is less than or equal
; to the first byte, then that slot is chosen.  The second byte is double the slot number.
	db $32, $00 ; 51/256 = 19.9% chance of slot 0
	db $65, $02 ; 51/256 = 19.9% chance of slot 1
	db $8C, $04 ; 39/256 = 15.2% chance of slot 2
	db $A5, $06 ; 25/256 =  9.8% chance of slot 3
	db $BE, $08 ; 25/256 =  9.8% chance of slot 4
	db $D7, $0A ; 25/256 =  9.8% chance of slot 5
	db $E4, $0C ; 13/256 =  5.1% chance of slot 6
	db $F1, $0E ; 13/256 =  5.1% chance of slot 7
	db $FC, $10 ; 11/256 =  4.3% chance of slot 8
	db $FF, $12 ;  3/256 =  1.2% chance of slot 9
