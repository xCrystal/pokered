WILD_BATTLE EQU 1
TRAINER_BATTLE EQU 2

;;;BattleCore:

; @@@ not modified anything yet
LoadEnemyMonData: ; 3eb01 (f:6b01)
; this is executed on init wild battle,
; and also when enemy trn sends a new mon
	ld a, [wLinkState]
	cp LINK_STATE_BATTLING ; battling on a link trn battle?
	jp z, LoadEnemyMonFromParty ; load mon from other player party if in a link battle
; not link battle	
	ld a, [wEnemyMonSpecies2]
	ld [wEnemyMonSpecies], a ; inside the enemy mon struct
	ld [wd0b5], a ; "used as a temp storage area for Pokemon Species, and other Pokemon/Battle related things"
; copies the base stat data of a pokemon to W_MONHDEXNUM (W_MONHEADER)
; INPUT:[wd0b5] = pokemon ID	
	call GetMonHeader
	ld a, [W_ENEMYBATTSTATUS3]
	bit Transformed, a ; is enemy mon transformed?
	ld hl, wcceb ; copied DVs from when it used Transform
	ld a, [hli]
	ld b, [hl]
	jr nz, .storeDVs ; jump if bit transformed is set
	                 ; should only be relevant in tranier battles, not wild, 
					 ; W_ENEMYBATTSTATUS3 is cleared on exiting a battle as well as initing trn battle.
					 ; @@@ but when would a mon be sent transformed?
	ld a, [W_ISINBATTLE]
	cp $2 ; is it a trainer battle?
; fixed DVs for trainer mon
; @@@ change this
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
	ld de, wEnemyMonLevel
	ld a, [W_CURENEMYLVL] ; level was loaded at 04:78e7 from TryDoWildEncounter
	ld [de], a
	inc de
	ld b, $0 ; b = consider stat exp? ; 0 = do not consider it @@@
	ld hl, wEnemyMonHP
	push hl
	call CalcStats ; @@@debug this at home.asm
	pop hl
	ld a, [W_ISINBATTLE]
	cp $2 ; is it a trainer battle?
	jr z, .copyHPAndStatusFromPartyData
; wild battle	
	ld a, [W_ENEMYBATTSTATUS3]
	bit Transformed, a ; is enemy mon transformed?
	jr nz, .copyTypes ; if transformed, jump
; if it's a wild mon and not transformed, init the current HP to max HP and the status to 0
	ld a, [wEnemyMonMaxHP]
	ld [hli], a
	ld a, [wEnemyMonMaxHP+1]
	ld [hli], a
	xor a
	inc hl
	ld [hl], a ; init status to 0
	jr .copyTypes
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
	ld a, [wWhichPokemon]
	ld [wEnemyMonPartyPos], a
	inc hl
	ld a, [hl]
	ld [wEnemyMonStatus], a
	jr .copyTypes
.copyTypes
	ld hl, W_MONHTYPES
	ld de, wEnemyMonType
	ld a, [hli]            ; copy type 1
	ld [de], a
	inc de
	ld a, [hli]            ; copy type 2
	ld [de], a
	inc de
	ld a, [hli]            ; copy catch rate
	ld [de], a
	inc de
	ld a, [W_ISINBATTLE]
	cp $2 ; is it a trainer battle?
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
; for a wild mon, first copy default moves from the mon header
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
	predef WriteMonMoves ; get moves based on current level ; @@@ dont get the last four, get them at random
	; and based on exp etc rather than level
.loadMovePPs
	ld hl, wEnemyMonMoves
	ld de, wEnemyMonSpecial + 1
	predef LoadMovePPs
	ld hl, W_MONHBASESTATS
	ld de, wEnemyMonBaseStats
	ld b, $5 ; @@@ spdef
.copyBaseStatsLoop
	ld a, [hli]
	ld [de], a
	inc de
	dec b
	jr nz, .copyBaseStatsLoop
	ld hl, W_MONHCATCHRATE
	ld a, [hli]
	ld [de], a
	inc de
	ld a, [hl] ; base exp
	ld [de], a
	ld a, [wEnemyMonSpecies2]
	ld [wd11e], a
	call GetMonName
	ld hl, wcd6d
	ld de, wEnemyMonNick
	ld bc, $b
	call CopyData
	ld a, [wEnemyMonSpecies2]
	ld [wd11e], a
	predef IndexToPokedex
	ld a, [wd11e]
	dec a
	ld c, a
	ld b, $1
	ld hl, wPokedexSeen
	predef FlagActionPredef ; mark this mon as seen in the pokedex
	ld hl, wEnemyMonLevel
	ld de, wEnemyMonUnmodifiedLevel
	ld bc, $b
	call CopyData
	ld a, $7 ; default stat mod
	ld b, $8 ; number of stat mods
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
	
; clear enemy statuses	
	ld hl,wEnemyStatsToDouble
	ld [hli],a
	ld [hli],a
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
	

; StartBattle:
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