; return selected move at wEnemySelectedMove
SelectEnemyMove: ; 3d564 (f:5564)
	ld a, [wLinkState]
	sub $4 ; LINK_STATE_BATTLING
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
	callab AIEnemyTrainerChooseMoves ; trainer battle @@@ this needs lots of work!
	
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
	ld hl, wPlayerStatsToDouble
	ld [hli], a
	ld [hli], a
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
	jp MainInBattleLoop
	
MainInBattleLoop:

; update mon's hp and status in party struct
	call ReadPlayerMonCurHPAndStatus
	
	ld hl, wBattleMonHP
	ld a, [hli]
	or [hl] ; is battle mon HP 0?
	jp z, HandlePlayerMonFainted  ; if battle mon HP is 0, jump
	
	ld hl, wEnemyMonHP
	ld a, [hli]
	or [hl] ; is enemy mon HP 0?
	jp z, HandleEnemyMonFainted ; if enemy mon HP is 0, jump
	
	call SaveScreenTilesToBuffer1
	xor a
	ld [wd11d], a

	ld a, [wLinkState]
	sub $4 ; LINK_STATE_BATTLING
	jr z, .displayBattleMenu
	
	call SelectEnemyMove ; Not Link Battle Only
	
.displayBattleMenu
; display battle menu to select player move	
	call DisplayBattleMenu
	ret c ; return if player ran from battle
	
; if player wasted his turn switching/sending mon this turn, or attempting to run away, 
; player can't select or execute move
.selectPlayerMove
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
	
;;; ExecutePlayerMove:

;;; ExecuteEnemyMove:
