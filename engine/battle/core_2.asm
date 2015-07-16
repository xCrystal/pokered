BattleCore:

INCLUDE "engine/battle/core_1.asm"

; These are move effects (second value from the Moves table in bank $E).
ResidualEffects1: ; 3c000 (f:4000)
; most non-side effects
	db CONVERSION_EFFECT
	db HAZE_EFFECT
	db SWITCH_AND_TELEPORT_EFFECT
	db MIST_EFFECT
	db FOCUS_ENERGY_EFFECT
	db CONFUSION_EFFECT
	db HEAL_EFFECT
	db TRANSFORM_EFFECT
	db LIGHT_SCREEN_EFFECT
	db REFLECT_EFFECT
	db POISON_EFFECT
	db PARALYZE_EFFECT
	db SUBSTITUTE_EFFECT
	db MIMIC_EFFECT
	db LEECH_SEED_EFFECT
	db SPLASH_EFFECT
	db -1
SetDamageEffects: ; 3c011 (f:4011)
; moves that do damage but not through normal calculations
; e.g., Super Fang, Psywave
	db SUPER_FANG_EFFECT
	db SPECIAL_DAMAGE_EFFECT
	db -1
ResidualEffects2: ; 3c014 (f:4014)
; non-side effects not included in ResidualEffects1
; stat-affecting moves, sleep-inflicting moves, and Bide
; e.g., Meditate, Bide, Hypnosis
	db $01
	db ATTACK_UP1_EFFECT
	db DEFENSE_UP1_EFFECT
	db SPEED_UP1_EFFECT
	db SPECIAL_UP1_EFFECT
	db ACCURACY_UP1_EFFECT
	db EVASION_UP1_EFFECT
	db ATTACK_DOWN1_EFFECT
	db DEFENSE_DOWN1_EFFECT
	db SPEED_DOWN1_EFFECT
	db SPECIAL_DOWN1_EFFECT
	db ACCURACY_DOWN1_EFFECT
	db EVASION_DOWN1_EFFECT
	db BIDE_EFFECT
	db SLEEP_EFFECT
	db ATTACK_UP2_EFFECT
	db DEFENSE_UP2_EFFECT
	db SPEED_UP2_EFFECT
	db SPECIAL_UP2_EFFECT
	db ACCURACY_UP2_EFFECT
	db EVASION_UP2_EFFECT
	db ATTACK_DOWN2_EFFECT
	db DEFENSE_DOWN2_EFFECT
	db SPEED_DOWN2_EFFECT
	db SPECIAL_DOWN2_EFFECT
	db ACCURACY_DOWN2_EFFECT
	db EVASION_DOWN2_EFFECT
	db -1
AlwaysHappenSideEffects: ; 3c030 (f:4030)
; Attacks that aren't finished after they faint the opponent.
	db DRAIN_HP_EFFECT
	db EXPLODE_EFFECT
	db DREAM_EATER_EFFECT
	db PAY_DAY_EFFECT
	db TWO_TO_FIVE_ATTACKS_EFFECT
	db $1E
	db ATTACK_TWICE_EFFECT
	db RECOIL_EFFECT
	db TWINEEDLE_EFFECT
	db RAGE_EFFECT
	db -1
SpecialEffects: ; 3c03b (f:403b)
; Effects from arrays 2, 4, and 5B, minus Twineedle and Rage.
; Includes all effects that do not need to be called at the end of
; ExecutePlayerMove (or ExecuteEnemyMove), because they have already been handled
	db DRAIN_HP_EFFECT
	db EXPLODE_EFFECT
	db DREAM_EATER_EFFECT
	db PAY_DAY_EFFECT
	db SWIFT_EFFECT
	db TWO_TO_FIVE_ATTACKS_EFFECT
	db $1E
	db CHARGE_EFFECT
	db SUPER_FANG_EFFECT
	db SPECIAL_DAMAGE_EFFECT
	db FLY_EFFECT
	db ATTACK_TWICE_EFFECT
	db JUMP_KICK_EFFECT
	db RECOIL_EFFECT
	; fallthrough to Next EffectsArray
SpecialEffectsCont: ; 3c049 (f:4049)
; damaging moves whose effect is executed prior to damage calculation
	db THRASH_PETAL_DANCE_EFFECT
	db TRAPPING_EFFECT
	db -1

SlidePlayerAndEnemySilhouettesOnScreen: ; 3c04c (f:404c)
	; HAX: I switched stuff around with InitBattle_Common. Call 3ec92, THEN GoPAL_SET with b=0.
	; This prevents flickering when entering battle.
	call GoPAL_SET
	ld a, MESSAGE_BOX ; the usual text box at the bottom of the screen
	ld [wTextBoxID], a
	call DisplayTextBoxID
	hlCoord 1, 5
	ld bc, $307
	call ClearScreenArea
	call DisableLCD
	call LoadFontTilePatterns
	call LoadHudAndHpBarAndStatusTilePatterns
	ld hl, vBGMap0
	ld bc, $400
.clearBackgroundLoop
	ld a, $7f
	ld [hli], a
	dec bc
	ld a, b
	or c
	jr nz, .clearBackgroundLoop
; copy the work RAM tile map to VRAM
	ld hl, wTileMap
	ld de, vBGMap0
	ld b, 18 ; number of rows
.copyRowLoop
	ld c, 20 ; number of columns
.copyColumnLoop
	ld a, [hli]
	ld [de], a
	inc e
	dec c
	jr nz, .copyColumnLoop
	ld a, 12 ; number of off screen tiles to the right of screen in VRAM
	add e ; skip the off screen tiles
	ld e, a
	jr nc, .noCarry
	inc d
.noCarry
	dec b
	jr nz, .copyRowLoop
	call EnableLCD
	ld a, $90
	ld [hWY], a
	ld [rWY], a
	xor a
	ld [hTilesetType], a
	ld [hSCY], a
	dec a
	ld [wUpdateSpritesEnabled], a
	call Delay3
	xor a
	ld [H_AUTOBGTRANSFERENABLED], a
	ld b, $70
	ld c, $90
	ld a, c
	ld [hSCX], a
	call DelayFrame
	ld a, %11100100 ; inverted palette for silhouette effect
	ld [rBGP], a
	ld [rOBP0], a
	ld [rOBP1], a
.slideSilhouettesLoop ; slide silhouettes of the player's pic and the enemy's pic onto the screen
	ld h, b
	ld l, $40
	call SetScrollXForSlidingPlayerBodyLeft ; begin background scrolling on line $40
	inc b
	inc b
	ld h, $0
	ld l, $60
	call SetScrollXForSlidingPlayerBodyLeft ; end background scrolling on line $60
	call SlidePlayerHeadLeft
	ld a, c
	ld [hSCX], a
	dec c
	dec c
	jr nz, .slideSilhouettesLoop
	ld a, $1
	ld [H_AUTOBGTRANSFERENABLED], a
	ld a, $31
	ld [$ffe1], a
	hlCoord 1, 5
	predef Func_3f0c6
	xor a
	ld [hWY], a
	ld [rWY], a
	inc a
	ld [H_AUTOBGTRANSFERENABLED], a
	call Delay3
	ld b, $1
	call GoPAL_SET
	call HideSprites
	ld hl, PrintBeginningBattleText
	ld b, BANK(PrintBeginningBattleText)
	jp Bankswitch

; when a battle is starting, silhouettes of the player's pic and the enemy's pic are slid onto the screen
; the lower of the player's pic (his body) is part of the background, but his head is a sprite
; the reason for this is that it shares Y coordinates with the lower part of the enemy pic, so background scrolling wouldn't work for both pics
; instead, the enemy pic is part of the background and uses the scroll register, while the player's head is a sprite and is slid by changing its X coordinates in a loop
SlidePlayerHeadLeft: ; 3c0ff (f:40ff)
	push bc
	ld hl, wOAMBuffer + $01
	ld c, $15 ; number of OAM entries
	ld de, $4 ; size of OAM entry
.loop
	dec [hl] ; decrement X
	dec [hl] ; decrement X
	add hl, de ; next OAM entry
	dec c
	jr nz, .loop
	pop bc
	ret

SetScrollXForSlidingPlayerBodyLeft: ; 3c110 (f:4110)
	ld a, [rLY]
	cp l
	jr nz, SetScrollXForSlidingPlayerBodyLeft
	ld a, h
	ld [rSCX], a
.loop
	ld a, [rLY]
	cp h
	jr z, .loop
	ret

; wild mon or link battle enemy ran from battle
EnemyRan: ; 3c202 (f:4202)
	call LoadScreenTilesFromBuffer1
	ld a, [wLinkState]
	cp LINK_STATE_BATTLING
	ld hl, WildRanText
	jr nz, .printText
; link battle
	xor a
	ld [wBattleResult], a
	ld hl, EnemyRanText
.printText
	call PrintText
	ld a, (SFX_08_44 - SFX_Headers_08) / 3
	call PlaySoundWaitForCurrent
	xor a
	ld [H_WHOSETURN], a
	ld hl, AnimationSlideEnemyMonOut
	ld b, BANK(AnimationSlideEnemyMonOut)
	jp Bankswitch

WildRanText: ; 3c229 (f:4229)
	TX_FAR _WildRanText
	db "@"

EnemyRanText: ; 3c22e (f:422e)
	TX_FAR _EnemyRanText
	db "@"

HandlePoisonBurnLeechSeed: ; 3c3bd (f:43bd)
	ld hl, wBattleMonHP
	ld de, wBattleMonStatus
	ld a, [H_WHOSETURN]
	and a
	jr z, .playersTurn
	ld hl, wEnemyMonHP
	ld de, wEnemyMonStatus
.playersTurn
	ld a, [de]
	and (1 << BRN) | (1 << PSN)
	jr z, .notBurnedOrPoisoned
	push hl
	ld hl, HurtByPoisonText
	ld a, [de]
	and 1 << BRN
	jr z, .poisoned
	ld hl, HurtByBurnText
.poisoned
	call PrintText
	xor a
	ld [wAnimationType], a
	ld a,BURN_PSN_ANIM
	call PlayMoveAnimation   ; play burn/poison animation
	pop hl
	call HandlePoisonBurnLeechSeed_DecreaseOwnHP
.notBurnedOrPoisoned
	ld de, W_PLAYERBATTSTATUS2
	ld a, [H_WHOSETURN]
	and a
	jr z, .playersTurn2
	ld de, W_ENEMYBATTSTATUS2
.playersTurn2
	ld a, [de]
	add a
	jr nc, .notLeechSeeded
	push hl
	ld a, [H_WHOSETURN]
	push af
	xor $1
	ld [H_WHOSETURN], a
	xor a
	ld [wAnimationType], a
	ld a,ABSORB
	call PlayMoveAnimation ; play leech seed animation (from opposing mon)
	pop af
	ld [H_WHOSETURN], a
	pop hl
	call HandlePoisonBurnLeechSeed_DecreaseOwnHP
	call HandlePoisonBurnLeechSeed_IncreaseEnemyHP
	push hl
	ld hl, HurtByLeechSeedText
	call PrintText
	pop hl
.notLeechSeeded
	ld a, [hli]
	or [hl]
	ret nz          ; test if fainted
	call DrawHUDsAndHPBars
	ld c, $14
	call DelayFrames
	xor a
	ret

HurtByPoisonText: ; 3c42e (f:442e)
	TX_FAR _HurtByPoisonText
	db "@"

HurtByBurnText: ; 3c433 (f:4433)
	TX_FAR _HurtByBurnText
	db "@"

HurtByLeechSeedText: ; 3c438 (f:4438)
	TX_FAR _HurtByLeechSeedText
	db "@"

; decreases the mon's current HP by 1/16 of the Max HP (multiplied by number of toxic ticks if active)
; note that the toxic ticks are considered even if the damage is not poison (hence the Leech Seed glitch)
; hl: HP pointer
; bc (out): total damage
HandlePoisonBurnLeechSeed_DecreaseOwnHP: ; 3c43d (f:443d)
	push hl
	push hl
	ld bc, $e      ; skip to max HP
	add hl, bc
	ld a, [hli]    ; load max HP
	ld [wHPBarMaxHP+1], a
	ld b, a
	ld a, [hl]
	ld [wHPBarMaxHP], a
	ld c, a
	srl b
	rr c
	srl b
	rr c
	srl c
	srl c         ; c = max HP/16 (assumption: HP < 1024)
	ld a, c
	and a
	jr nz, .nonZeroDamage
	inc c         ; damage is at least 1
.nonZeroDamage
	ld hl, W_PLAYERBATTSTATUS3
	ld de, W_PLAYERTOXICCOUNTER
	ld a, [H_WHOSETURN]
	and a
	jr z, .playersTurn
	ld hl, W_ENEMYBATTSTATUS3
	ld de, W_ENEMYTOXICCOUNTER
.playersTurn
	bit BadlyPoisoned, [hl]
	jr z, .noToxic
	ld a, [de]    ; increment toxic counter
	inc a
	ld [de], a
	ld hl, $0000
.toxicTicksLoop
	add hl, bc
	dec a
	jr nz, .toxicTicksLoop
	ld b, h       ; bc = damage * toxic counter
	ld c, l
.noToxic
	pop hl
	inc hl
	ld a, [hl]    ; subtract total damage from current HP
	ld [wHPBarOldHP], a
	sub c
	ld [hld], a
	ld [wHPBarNewHP], a
	ld a, [hl]
	ld [wHPBarOldHP+1], a
	sbc b
	ld [hl], a
	ld [wHPBarNewHP+1], a
	jr nc, .noOverkill
	xor a         ; overkill: zero HP
	ld [hli], a
	ld [hl], a
	ld [wHPBarNewHP], a
	ld [wHPBarNewHP+1], a
.noOverkill
	call UpdateCurMonHPBar
	pop hl
	ret

; adds bc to enemy HP
; bc isn't updated if HP substracted was capped to prevent overkill
HandlePoisonBurnLeechSeed_IncreaseEnemyHP: ; 3c4a3 (f:44a3)
	push hl
	ld hl, wEnemyMonMaxHP
	ld a, [H_WHOSETURN]
	and a
	jr z, .playersTurn
	ld hl, wBattleMonMaxHP
.playersTurn
	ld a, [hli]
	ld [wHPBarMaxHP+1], a
	ld a, [hl]
	ld [wHPBarMaxHP], a
	ld de, $fff2
	add hl, de           ; skip back fomr max hp to current hp
	ld a, [hl]
	ld [wHPBarOldHP], a ; add bc to current HP
	add c
	ld [hld], a
	ld [wHPBarNewHP], a
	ld a, [hl]
	ld [wHPBarOldHP+1], a
	adc b
	ld [hli], a
	ld [wHPBarNewHP+1], a
	ld a, [wHPBarMaxHP]
	ld c, a
	ld a, [hld]
	sub c
	ld a, [wHPBarMaxHP+1]
	ld b, a
	ld a, [hl]
	sbc b
	jr c, .noOverfullHeal
	ld a, b                ; overfull heal, set HP to max HP
	ld [hli], a
	ld [wHPBarNewHP+1], a
	ld a, c
	ld [hl], a
	ld [wHPBarNewHP], a
.noOverfullHeal
	ld a, [H_WHOSETURN]
	xor $1
	ld [H_WHOSETURN], a
	call UpdateCurMonHPBar
	ld a, [H_WHOSETURN]
	xor $1
	ld [H_WHOSETURN], a
	pop hl
	ret

UpdateCurMonHPBar: ; 3c4f6 (f:44f6)
	hlCoord 10, 9    ; tile pointer to player HP bar
	ld a, [H_WHOSETURN]
	and a
	ld a, $1
	jr z, .playersTurn
	hlCoord 2, 2    ; tile pointer to enemy HP bar
	xor a
.playersTurn
	push bc
	ld [wHPBarType], a
	predef UpdateHPBar_Hook
	pop bc
	ret

CheckNumAttacksLeft: ; 3c50f (f:450f)
	ld a, [wPlayerNumAttacksLeft]
	and a
	jr nz, .checkEnemy
; player has 0 attacks left
	ld hl, W_PLAYERBATTSTATUS1
	res UsingTrappingMove, [hl] ; player not using multi-turn attack like wrap any more
.checkEnemy
	ld a, [wEnemyNumAttacksLeft]
	and a
	ret nz
; enemy has 0 attacks left
	ld hl, W_ENEMYBATTSTATUS1
	res UsingTrappingMove, [hl] ; enemy not using multi-turn attack like wrap any more
	ret

HandleEnemyMonFainted: ; 3c525 (f:4525)
	xor a
	ld [wccf0], a
	call FaintEnemyPokemon
	call AnyPartyAlive
	ld a, d
	and a
	jp z, HandlePlayerBlackOut ; if no party mons are alive, the player blacks out
	ld hl, wBattleMonHP
	ld a, [hli]
	or [hl] ; is battle mon HP zero?
	call nz, DrawPlayerHUDAndHPBar ; if battle mon HP is not zero, draw player HD and HP bar
	ld a, [W_ISINBATTLE]
	dec a
	ret z ; return if it's a wild battle
	call AnyEnemyPokemonAliveCheck
	jp z, TrainerBattleVictory
	ld hl, wBattleMonHP
	ld a, [hli]
	or [hl] ; does battle mon have 0 HP?
	jr nz, .skipReplacingBattleMon ; if not, skip replacing battle mon
	call DoUseNextMonDialogue ; this call is useless in a trainer battle. it shouldn't be here
	ret c
	call ChooseNextMon
.skipReplacingBattleMon
	ld a, $1
	ld [wcd6a], a
	call ReplaceFaintedEnemyMon
	jp z, EnemyRan
	xor a
	ld [wcd6a], a
	jp MainInBattleLoop

FaintEnemyPokemon ; 0x3c567
	call ReadPlayerMonCurHPAndStatus
	ld a, [W_ISINBATTLE]
	dec a
	jr z, .wild
	ld a, [wEnemyMonPartyPos]
	ld hl, wEnemyMon1HP
	ld bc, wEnemyMon2 - wEnemyMon1
	call AddNTimes
	xor a
	ld [hli], a
	ld [hl], a
.wild
	ld hl, W_PLAYERBATTSTATUS1
	res AttackingMultipleTimes, [hl]
	xor a
	ld [wPlayerNumHits], a
	ld hl, W_ENEMYBATTSTATUS1 ; clear enemy statuses
	ld [hli], a
	ld [hli], a
	ld [hl], a
	ld [W_ENEMYDISABLEDMOVE], a
	ld [wEnemyDisabledMoveNumber], a
	ld [wEnemyHasUsedMinimize], a
	ld hl, wPlayerUsedMove
	ld [hli], a
	ld [hl], a
	ld hl, W_PLAYERDAMAGE
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hl], a
	ld hl, wEnemyBideAccumulatedDamage
	ld [hli], a
	ld [hl], a	
	hlCoord 12, 5
	deCoord 12, 6
	call SlideDownFaintedMonPic
	ld hl, wTileMap
	ld bc, $40b
	call ClearScreenArea
	ld a, [W_ISINBATTLE]
	dec a
	jr z, .sfxplayed
	xor a
	ld [wc0f1], a
	ld [wc0f2], a
	ld a, (SFX_08_48 - SFX_Headers_08) / 3 ; SFX_FALL?
	call PlaySoundWaitForCurrent
.sfxwait
	ld a, [wc02a]
	cp (SFX_08_48 - SFX_Headers_08) / 3
	jr z, .sfxwait
	ld a, (SFX_08_43 - SFX_Headers_08) / 3 ; SFX_DROP
	call PlaySound
.sfxplayed
	ld hl, wBattleMonHP
	ld a, [hli]
	or [hl]
	jr nz, .playermonnotfaint
	ld a, [wccf0]
	and a
	jr nz, .playermonnotfaint
	call RemoveFaintedPlayerMon
.playermonnotfaint
	call AnyPartyAlive
	ld a, d
	and a
	jr nz, .moreAliveMons
	ret
.moreAliveMons
	ld a, [W_ISINBATTLE]
	dec a
	jr nz, .trainer
	
; wild_win
	call WaitForSoundToFinish
	call EndLowHealthAlarm	
	ld a, MUSIC_DEFEATED_WILD_MON
	call PlayBattleVictoryMusic	
.trainer
	ld hl, EnemyMonFaintedText
	call PrintText
	call PrintEmptyString
	call SaveScreenTilesToBuffer1
	xor a
	ld [wBattleResult], a
	ld b, EXP__ALL
	call IsItemInBag
	push af
	jr z, .giveExpToMonsThatFought ; if no exp all, then jump

; the player has exp all
; first, we halve the values that determine exp gain
; the enemy mon base stats are added to stat exp, so they are halved
; the base exp (which determines normal exp) is also halved
	ld hl, wEnemyMonBaseStats
	ld b, $7
.halveExpDataLoop
	srl [hl]
	inc hl
	dec b
	jr nz, .halveExpDataLoop

; give exp (divided evenly) to the mons that actually fought in battle against the enemy mon that has fainted
; if exp all is in the bag, this will be only be half of the stat exp and normal exp, due to the above loop
.giveExpToMonsThatFought
	xor a
	ld [wBoostExpByExpAll], a
	callab GainExperience
	pop af
	ret z ; return if no exp all

; the player has exp all
; now, set the gain exp flag for every party member
; half of the total stat exp and normal exp will divided evenly amongst every party member
	ld a, $1
	ld [wBoostExpByExpAll], a
	ld a, [wPartyCount]
	ld b, 0
.gainExpFlagsLoop
	scf
	rl b
	dec a
	jr nz, .gainExpFlagsLoop
	ld a, b
	ld [wPartyGainExpFlags], a
	ld hl, GainExperience
	ld b, BANK(GainExperience)
	jp Bankswitch

EnemyMonFaintedText: ; 0x3c63e
	TX_FAR _EnemyMonFaintedText
	db "@"

EndLowHealthAlarm: ; 3c643 (f:4643)
	xor a
	ld [wLowHealthAlarm], a ;disable low health alarm
	ld [wc02a], a
	inc a
	ld [wccf6], a
	ret

AnyEnemyPokemonAliveCheck: ; 3c64f (f:464f)
	ld a, [wEnemyPartyCount]
	ld b, a
	xor a
	ld hl, wEnemyMon1HP
	ld de, wEnemyMon2 - wEnemyMon1
.nextPokemon
	or [hl]
	inc hl
	or [hl]
	dec hl
	add hl, de
	dec b
	jr nz, .nextPokemon
	and a
	ret

; stores whether enemy ran in Z flag
ReplaceFaintedEnemyMon: ; 3c664 (f:4664)
	ld hl, wcf1e
	ld e, $30
	call GetBattleHealthBarColor
	callab DrawEnemyPokeballs
	ld a, [wLinkState]
	cp LINK_STATE_BATTLING
	jr nz, .notLinkBattle
; link battle
	call LinkBattleExchangeData
	ld a, [wSerialExchangeNybbleReceiveData]
	cp $f
	ret z
	call LoadScreenTilesFromBuffer1
.notLinkBattle
	call EnemySendOut
	xor a
	ld [W_ENEMYMOVENUM], a
	ld [wcd6a], a
	inc a ; reset Z flag
	ret

TrainerBattleVictory: ; 3c696 (f:4696)
	call EndLowHealthAlarm
	ld b, MUSIC_DEFEATED_GYM_LEADER
	ld a, [W_GYMLEADERNO]
	and a
	jr nz, .gymleader
	ld b, MUSIC_DEFEATED_TRAINER
.gymleader
	ld a, [W_TRAINERCLASS]
	cp SONY3 ; final battle against rival
	jr nz, .notrival
	ld b, MUSIC_DEFEATED_GYM_LEADER
	ld hl, W_FLAGS_D733
	set 1, [hl]
.notrival
	ld a, [wLinkState]
	cp LINK_STATE_BATTLING
	ld a, b
	call nz, PlayBattleVictoryMusic
	ld hl, TrainerDefeatedText
	call PrintText
	ld a, [wLinkState]
	cp LINK_STATE_BATTLING
	ret z
	call ScrollTrainerPicAfterBattle
	ld c, $28
	call DelayFrames
	call PrintEndBattleText
; win money	
	ld hl, MoneyForWinningText
	call PrintText
	ld de, wPlayerMoney + 2
	ld hl, wAmountMoneyWon + 2
	ld c, $3
	predef_jump AddBCDPredef

MoneyForWinningText: ; 3c6e4 (f:46e4)
	TX_FAR _MoneyForWinningText
	db "@"

TrainerDefeatedText: ; 3c6e9 (f:46e9)
	TX_FAR _TrainerDefeatedText
	db "@"

PlayBattleVictoryMusic: ; 3c6ee (f:46ee)
	push af
	ld a, $ff
	ld [wc0ee], a
	call PlaySoundWaitForCurrent
	ld c, BANK(Music_DefeatedTrainer)
	pop af
	call PlayMusic
	jp Delay3

HandlePlayerMonFainted: ; 3c700 (f:4700)
	ld a, $1
	ld [wccf0], a
	call RemoveFaintedPlayerMon
	call AnyPartyAlive     ; test if any more mons are alive
	ld a, d
	and a
	jp z, HandlePlayerBlackOut
	ld hl, wEnemyMonHP
	ld a, [hli]
	or [hl] ; is enemy mon's HP 0?
	jr nz, .doUseNextMonDialogue ; if not, jump
; the enemy mon has 0 HP
	call FaintEnemyPokemon
	ld a, [W_ISINBATTLE]
	dec a
	ret z            ; if wild encounter, battle is over
	call AnyEnemyPokemonAliveCheck
	jp z, TrainerBattleVictory
.doUseNextMonDialogue
	call DoUseNextMonDialogue
	ret c ; return if the player ran from battle
	call ChooseNextMon
	jp nz, MainInBattleLoop ; if the enemy mon has more than 0 HP, go back to battle loop
; the enemy mon has 0 HP
	ld a, $1
	ld [wcd6a], a
	call ReplaceFaintedEnemyMon
	jp z, EnemyRan ; if enemy ran from battle rather than sending out another mon, jump
	xor a
	ld [wcd6a], a
	jp MainInBattleLoop

; resets flags, slides mon's pic down, plays cry, and prints fainted message
RemoveFaintedPlayerMon: ; 3c741 (f:4741)
	ld a, [wPlayerMonNumber]
	ld c, a
	ld hl, wPartyGainExpFlags
	ld b, $0
	predef FlagActionPredef ; clear gain exp flag for fainted mon
	ld hl, W_ENEMYBATTSTATUS1
	res 2, [hl]   ; reset "attacking multiple times" flag
	ld a, [wLowHealthAlarm]
	bit 7, a      ; skip sound flag (red bar (?))
	jr z, .skipWaitForSound
	ld a, $ff
	ld [wLowHealthAlarm], a ;disable low health alarm
	call WaitForSoundToFinish
.skipWaitForSound
	xor a
	ld hl, wPlayerBideAccumulatedDamage
	ld [hli], a
	ld [hl], a
	ld hl, W_PLAYERDAMAGE
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hl], a	
	ld [wBattleMonStatus], a
	call ReadPlayerMonCurHPAndStatus
	hlCoord 9, 7
	ld bc, $50b
	call ClearScreenArea
	hlCoord 1, 10
	deCoord 1, 11
	call SlideDownFaintedMonPic
	ld a, $1
	ld [wBattleResult], a
	ld a, [wccf0]
	and a
	ret z
	ld a, [wBattleMonSpecies]
	call PlayCry
	ld hl, PlayerMonFaintedText
	jp PrintText

PlayerMonFaintedText: ; 3c796 (f:4796)
	TX_FAR _PlayerMonFaintedText
	db "@"

; asks if you want to use next mon
; stores whether you ran in C flag
DoUseNextMonDialogue: ; 3c79b (f:479b)
	call PrintEmptyString
	call SaveScreenTilesToBuffer1
	ld a, [W_ISINBATTLE]
	and a
	dec a
	ret nz ; return if it's a trainer battle
	ld hl, UseNextMonText
	call PrintText
.displayYesNoBox
	hlCoord 13, 9
	ld bc, $a0e
	ld a, TWO_OPTION_MENU
	ld [wTextBoxID], a
	call DisplayTextBoxID
	ld a, [wd12e]
	cp $2 ; did the player choose NO?
	jr z, .tryRunning ; if the player chose NO, try running
	and a ; reset carry
	ret
.tryRunning
	ld a, [wCurrentMenuItem]
	and a
	jr z, .displayYesNoBox ; xxx when does this happen?
	ld hl, wPartyMon1Speed
	ld de, wEnemyMonSpeed
	jp TryRunningFromBattle

UseNextMonText: ; 3c7d3 (f:47d3)
	TX_FAR _UseNextMonText
	db "@"

; choose next player mon to send out
; stores whether enemy mon has no HP left in Z flag
ChooseNextMon: ; 3c7d8 (f:47d8)
	ld a, $2
	ld [wd07d], a
	call DisplayPartyMenu
.checkIfMonChosen
	jr nc, .monChosen
.goBackToPartyMenu
	call GoBackToPartyMenu
	jr .checkIfMonChosen
.monChosen
	call HasMonFainted
	jr z, .goBackToPartyMenu ; if mon fainted, you have to choose another
	ld a, [wLinkState]
	cp LINK_STATE_BATTLING
	jr nz, .notLinkBattle
	inc a
	ld [wcd6a], a
	call LinkBattleExchangeData
.notLinkBattle
	xor a
	ld [wcd6a], a
	call ClearSprites
	ld a, [wWhichPokemon]
	ld [wPlayerMonNumber], a
	ld c, a
	ld hl, wPartyGainExpFlags
	ld b, $1
	push bc
	predef FlagActionPredef
	pop bc
	ld hl, wPartyFoughtCurrentEnemyFlags
	predef FlagActionPredef
	call LoadBattleMonFromParty
	call GBPalWhiteOut
	call LoadHudTilePatterns
	call LoadScreenTilesFromBuffer1
	call GoPAL_SET_CF1C
	call GBPalNormal
	call SendOutMon
	ld hl, wEnemyMonHP
	ld a, [hli]
	or [hl]
	ret

; called when player is out of usable mons.
; prints approriate lose message, sets carry flag if player blacked out (special case for initial rival fight)
HandlePlayerBlackOut: ; 3c837 (f:4837)
	ld a, [wLinkState]
	cp LINK_STATE_BATTLING
	jr z, .notSony1Battle
	ld a, [W_CUROPPONENT]
	cp $c8 + SONY1
	jr nz, .notSony1Battle
	ld hl, wTileMap  ; sony 1 battle
	ld bc, $815
	call ClearScreenArea
	call ScrollTrainerPicAfterBattle
	ld c, $28
	call DelayFrames
	ld hl, Sony1WinText
	call PrintText
	ld a, [W_CURMAP]
	cp OAKS_LAB
	ret z            ; starter battle in oak's lab: don't black out
.notSony1Battle
	ld b, $0
	call GoPAL_SET
	ld hl, PlayerBlackedOutText2
	ld a, [wLinkState]
	cp LINK_STATE_BATTLING
	jr nz, .noLinkBattle
	ld hl, LinkBattleLostText
.noLinkBattle
	call PrintText
	ld a, [wd732]
	res 5, a
	ld [wd732], a
	call ClearScreen
	scf
	ret

Sony1WinText: ; 3c884 (f:4884)
	TX_FAR _Sony1WinText
	db "@"

PlayerBlackedOutText2: ; 3c889 (f:4889)
	TX_FAR _PlayerBlackedOutText2
	db "@"

LinkBattleLostText: ; 3c88e (f:488e)
	TX_FAR _LinkBattleLostText
	db "@"

; slides pic of fainted mon downwards until it disappears
; bug: when this is called, [H_AUTOBGTRANSFERENABLED] is non-zero, so there is screen tearing
SlideDownFaintedMonPic: ; 3c893 (f:4893)
	ld a, [wd730]
	push af
	set 6, a
	ld [wd730], a
	ld b, 7 ; number of times to slide
.slideStepLoop ; each iteration, the mon is slid down one row
	push bc
	push de
	push hl
	ld b, 6 ; number of rows
.rowLoop
	push bc
	push hl
	push de
	ld bc, $7
	call CopyData
	pop de
	pop hl
	ld bc, -20
	add hl, bc
	push hl
	ld h, d
	ld l, e
	add hl, bc
	ld d, h
	ld e, l
	pop hl
	pop bc
	dec b
	jr nz, .rowLoop
	ld bc, 20
	add hl, bc
	ld de, SevenSpacesText
	call PlaceString
	ld c, 2
	call DelayFrames
	pop hl
	pop de
	pop bc
	dec b
	jr nz, .slideStepLoop
	pop af
	ld [wd730], a
	ret

SevenSpacesText: ; 3c8d7 (f:48d7)
	db "       @"

; slides the player or enemy trainer off screen
; a is the number of tiles to slide it horizontally (always 9 for the player trainer or 8 for the enemy trainer)
; if a is 8, the slide is to the right, else it is to the left
; bug: when this is called, [H_AUTOBGTRANSFERENABLED] is non-zero, so there is screen tearing
SlideTrainerPicOffScreen: ; 3c8df (f:48df)
	ld [$FF8B], a
	ld c, a
.slideStepLoop ; each iteration, the trainer pic is slid one tile left/right
	push bc
	push hl
	ld b, 7 ; number of rows
.rowLoop
	push hl
	ld a, [$FF8B]
	ld c, a
.columnLoop
	ld a, [$FF8B]
	cp 8
	jr z, .slideRight
.slideLeft ; slide player sprite off screen
	ld a, [hld]
	ld [hli], a
	inc hl
	jr .nextColumn
.slideRight ; slide enemy trainer sprite off screen
	ld a, [hli]
	ld [hld], a
	dec hl
.nextColumn
	dec c
	jr nz, .columnLoop
	pop hl
	ld de, 20
	add hl, de
	dec b
	jr nz, .rowLoop
	ld c, 2
	call DelayFrames
	pop hl
	pop bc
	dec c
	jr nz, .slideStepLoop
	ret

TrainerAboutToUseText: ; 3ca79 (f:4a79)
	TX_FAR _TrainerAboutToUseText
	db "@"

TrainerSentOutText: ; 3ca7e (f:4a7e)
	TX_FAR _TrainerSentOutText
	db "@"

; tests if the player has any pokemon that are not fainted
; sets d = 0 if all fainted, d != 0 if some mons are still alive
AnyPartyAlive: ; 3ca83 (f:4a83)
	ld a, [wPartyCount]
	ld e, a
	xor a
	ld hl, wPartyMon1HP
	ld bc, wPartyMon2 - wPartyMon1 - 1
.partyMonsLoop
	or [hl]
	inc hl
	or [hl]
	add hl, bc
	dec e
	jr nz, .partyMonsLoop
	ld d, a
	ret

; tests if player mon has fainted
; stores whether mon has fainted in Z flag
HasMonFainted: ; 3ca97 (f:4a97)
	ld a, [wWhichPokemon]
	ld hl, wPartyMon1HP
	ld bc, wPartyMon2 - wPartyMon1
	call AddNTimes
	ld a, [hli]
	or [hl]
	ret nz
	ld a, [wd11d]
	and a
	jr nz, .done
	ld hl, NoWillText
	call PrintText
.done
	xor a
	ret

NoWillText: ; 3cab4 (f:4ab4)
	TX_FAR _NoWillText
	db "@"

; try to run from battle (hl = player speed, de = enemy speed)
; stores whether the attempt was successful in carry flag
TryRunningFromBattle: ; 3cab9 (f:4ab9)
	ld a, [W_BATTLETYPE]
	cp $2
	jp z, .canEscape ; jump if it's a safari battle
	ld a, [wLinkState]
	cp LINK_STATE_BATTLING
	jp z, .canEscape
	ld a, [W_ISINBATTLE]
	dec a
	jr nz, .trainerBattle ; jump if it's a trainer battle
	ld a, [wNumRunAttempts]
	inc a
	ld [wNumRunAttempts], a
	ld a, [hli]
	ld [H_MULTIPLICAND + 1], a
	ld a, [hl]
	ld [H_MULTIPLICAND + 2], a
	ld a, [de]
	ld [$ff8d], a
	inc de
	ld a, [de]
	ld [$ff8e], a
	call LoadScreenTilesFromBuffer1
	ld de, H_MULTIPLICAND + 1
	ld hl, $ff8d
	ld c, $2
	call StringCmp
	jr nc, .canEscape ; jump if player speed greater than enemy speed
	xor a
	ld [H_MULTIPLICAND], a
	ld a, 32
	ld [H_MULTIPLIER], a
	call Multiply ; multiply player speed by 32
	ld a, [H_PRODUCT + 2]
	ld [H_DIVIDEND], a
	ld a, [H_PRODUCT + 3]
	ld [H_DIVIDEND + 1], a
	ld a, [$ff8d]
	ld b, a
	ld a, [$ff8e]
; divide enemy speed by 4
	srl b
	rr a
	srl b
	rr a
	and a
	jr z, .canEscape ; jump if enemy speed divided by 4, mod 256 is 0
	ld [H_DIVISOR], a ; ((enemy speed / 4) % 256)
	ld b, $2
	call Divide ; divide (player speed * 32) by ((enemy speed / 4) % 256)
	ld a, [H_QUOTIENT + 2]
	and a ; is the quotient greater than 256?
	jr nz, .canEscape ; if so, the player can escape
	ld a, [wNumRunAttempts]
	ld c, a
; add 30 to the quotient for each run attempt
.loop
	dec c
	jr z, .compareWithRandomValue
	ld b, 30
	ld a, [H_QUOTIENT + 3]
	add b
	ld [H_QUOTIENT + 3], a
	jr c, .canEscape
	jr .loop
.compareWithRandomValue
	call BattleRandom
	ld b, a
	ld a, [H_QUOTIENT + 3]
	cp b
	jr nc, .canEscape ; if the random value was less than or equal to the quotient 
	                  ; plus 30 times the number of attempts, the player can escape
; can't escape
	ld a, $1
	ld [wcd6a], a
	ld hl, CantEscapeText
	jr .printCantEscapeOrNoRunningText
.trainerBattle
	ld hl, NoRunningText
.printCantEscapeOrNoRunningText
	call PrintText
	ld a, $1
	ld [wd11f], a
	call SaveScreenTilesToBuffer1
	and a ; reset carry
	ret
.canEscape
	ld a, [wLinkState]
	cp LINK_STATE_BATTLING
	ld a, $2
	jr nz, .playSound
; link battle
	call SaveScreenTilesToBuffer1
	xor a
	ld [wcd6a], a
	ld a, $f
	ld [wPlayerMoveListIndex], a
	call LinkBattleExchangeData
	call LoadScreenTilesFromBuffer1
	ld a, [wSerialExchangeNybbleReceiveData]
	cp $f
	ld a, $2
	jr z, .playSound
	dec a
.playSound
	ld [wBattleResult], a
	ld a, (SFX_08_44 - SFX_Headers_08) / 3
	call PlaySoundWaitForCurrent
	ld hl, GotAwayText
	call PrintText
	call WaitForSoundToFinish
	call SaveScreenTilesToBuffer1
	scf ; set carry
	ret

CantEscapeText: ; 3cb97 (f:4b97)
	TX_FAR _CantEscapeText
	db "@"

NoRunningText: ; 3cb9c (f:4b9c)
	TX_FAR _NoRunningText
	db "@"

GotAwayText: ; 3cba1 (f:4ba1)
	TX_FAR _GotAwayText
	db "@"

; copies from party data to battle mon data when sending out a new player mon
LoadBattleMonFromParty: ; 3cba6 (f:4ba6)
	ld a, [wWhichPokemon]
	ld bc, $2c
	ld hl, wPartyMon1Species
	call AddNTimes
	ld de, wBattleMonSpecies
	ld bc, $c
	call CopyData
	ld bc, $f
	add hl, bc
	ld de, wBattleMonDVs
	ld bc, $2
	call CopyData
	ld de, wBattleMonPP
	ld bc, $4
	call CopyData
	ld de, wBattleMonLevel
	ld bc, 1 + NUM_STATS * 2
	call CopyData
	ld a, [wBattleMonSpecies2]
	ld [wd0b5], a
	call GetMonHeader
	ld hl, wPartyMonNicks
	ld a, [wPlayerMonNumber]
	call SkipFixedLengthTextEntries
	ld de, wBattleMonNick
	ld bc, $b
	call CopyData
	ld hl, wBattleMonLevel
	ld de, wPlayerMonUnmodifiedLevel ; block of memory used for unmodified stats
	ld bc, $b
	call CopyData
	call ApplyBurnAndParalysisPenaltiesToPlayer
	call ApplyBadgeStatBoosts
	ld a, $7 ; default stat modifier
	ld b, $8
	ld hl, wPlayerMonAttackMod
.statModLoop
	ld [hli], a
	dec b
	jr nz, .statModLoop
	ret

; copies from enemy party data to current enemy mon data when sending out a new enemy mon
LoadEnemyMonFromParty: ; 3cc13 (f:4c13)
	ld a, [wWhichPokemon]
	ld bc, $2c
	ld hl, wEnemyMons
	call AddNTimes
	ld de, wEnemyMonSpecies
	ld bc, $c
	call CopyData
	ld bc, $f
	add hl, bc
	ld de, wEnemyMonDVs
	ld bc, $2
	call CopyData
	ld de, wEnemyMonPP
	ld bc, $4
	call CopyData
	ld de, wEnemyMonLevel
	ld bc, 1 + NUM_STATS * 2
	call CopyData
	ld a, [wEnemyMonSpecies]
	ld [wd0b5], a
	call GetMonHeader
	ld hl, wEnemyMonNicks
	ld a, [wWhichPokemon]
	call SkipFixedLengthTextEntries
	ld de, wEnemyMonNick
	ld bc, $b
	call CopyData
	ld hl, wEnemyMonLevel
	ld de, wEnemyMonUnmodifiedLevel ; block of memory used for unmodified stats
	ld bc, $b
	call CopyData
	call ApplyBurnAndParalysisPenaltiesToEnemy
	ld hl, W_MONHBASESTATS
	ld de, wEnemyMonBaseStats
	ld b, $5
.copyBaseStatsLoop
	ld a, [hli]
	ld [de], a
	inc de
	dec b
	jr nz, .copyBaseStatsLoop
	ld a, $7 ; default stat modifier
	ld b, $8
	ld hl, wEnemyMonStatMods
.statModLoop
	ld [hli], a
	dec b
	jr nz, .statModLoop
	ld a, [wWhichPokemon]
	ld [wEnemyMonPartyPos], a
	ret

; show 2 stages of the player getting smaller before disappearing
AnimateRetreatingPlayerMon: ; 3ccfa (f:4cfa)
	hlCoord 1, 5
	ld bc, $707
	call ClearScreenArea
	hlCoord 3, 7
	ld bc, $505
	xor a
	ld [wcd6c], a
	ld [H_DOWNARROWBLINKCNT1], a
	predef Func_79aba
	ld c, $4
	call DelayFrames
	call .clearScreenArea
	hlCoord 4, 9
	ld bc, $303
	ld a, $1
	ld [wcd6c], a
	xor a
	ld [H_DOWNARROWBLINKCNT1], a
	predef Func_79aba
	call Delay3
	call .clearScreenArea
	ld a, $4c
	Coorda 5, 11
.clearScreenArea
	hlCoord 1, 5
	ld bc, $707
	jp ClearScreenArea

; reads player's current mon's HP from wBattleMonHP into party struct
ReadPlayerMonCurHPAndStatus: ; 3cd43 (f:4d43)
	ld a, [wPlayerMonNumber]
	ld hl, wPartyMon1HP
	ld bc, wPartyMon2 - wPartyMon1
	call AddNTimes
	ld d, h
	ld e, l
	ld hl, wBattleMonHP
	ld bc, $4               ; 2 bytes HP, 1 byte unknown (unused?), 1 byte status
	jp CopyData

DrawHUDsAndHPBars: ; 3cd5a (f:4d5a)
	call DrawPlayerHUDAndHPBar
	jp DrawEnemyHUDAndHPBar

DrawPlayerHUDAndHPBar: ; 3cd60 (f:4d60)
	xor a
	ld [H_AUTOBGTRANSFERENABLED], a
	hlCoord 9, 7
	ld bc, $50b
	call ClearScreenArea
	callab PlacePlayerHUDTiles
	hlCoord 18, 9
	ld [hl], $73
	ld de, wBattleMonNick
	hlCoord 10, 7
	call PlaceString
	deCoord 17, 11	
	call PrintEXPBar
	ld hl, wBattleMonSpecies
	ld de, wLoadedMon
	ld bc, $c
	call CopyData
	ld hl, wBattleMonLevel
	ld de, wLoadedMonLevel
	ld bc, $b
	call CopyData
	hlCoord 10, 8 ; @@@ 14, 8
	push hl
	inc hl
	ld de, wLoadedMonStatus
	call PrintStatusConditionNotFainted
	pop hl
;	jr nz, .asm_3cdae
	hlCoord 14, 8
	call PrintLevel
.asm_3cdae
	ld a, [wLoadedMonSpecies]
	ld [wcf91], a
	hlCoord 10, 9
	predef DrawHP
	ld a, $1
	ld [H_AUTOBGTRANSFERENABLED], a
	ld hl, wcf1d
	call GetBattleHealthBarColor
	ld hl, wBattleMonHP
	ld a, [hli]
	or [hl]
	jr z, .asm_3cdd9
	ld a, [wccf6]
	and a
	ret nz
	ld a, [wcf1d]
	cp $2
	jr z, .asm_3cde6
.asm_3cdd9
	ld hl, wLowHealthAlarm
	bit 7, [hl] ;low health alarm enabled?
	ld [hl], $0
	ret z
	xor a
	ld [wc02a], a
	ret
.asm_3cde6
	ld hl, wLowHealthAlarm
	set 7, [hl] ;enable low health alarm
	ret

DrawEnemyHUDAndHPBar: ; 3cdec (f:4dec)
	xor a
	ld [H_AUTOBGTRANSFERENABLED], a
	ld hl, wTileMap
	ld bc, $40c
	call ClearScreenArea
	callab PlaceEnemyHUDTiles
	ld de, wEnemyMonNick
	hlCoord 1, 0
	call CenterMonName
	call PlaceString
	hlCoord 2, 1
	push hl
	inc hl
	ld de, wEnemyMonStatus
	call PrintStatusConditionNotFainted
	pop hl
;	jr nz, .skipPrintLevel ; if the mon has a status condition, skip printing the level
	ld a, [wEnemyMonLevel]
	ld [wLoadedMonLevel], a
	hlCoord 6, 1	
	call PrintLevel
.skipPrintLevel
	ld hl, wEnemyMonHP
	ld a, [hli]
	ld [H_MULTIPLICAND + 1], a
	ld a, [hld]
	ld [H_MULTIPLICAND + 2], a
	or [hl] ; is current HP zero?
	jr nz, .hpNonzero
; current HP is 0
; set variables for DrawHPBar
	ld c, a
	ld e, a
	ld d, $6
	jp .drawHPBar
.hpNonzero
	xor a
	ld [H_MULTIPLICAND], a
	ld a, 48
	ld [H_MULTIPLIER], a
	call Multiply ; multiply current HP by 48
	ld hl, wEnemyMonMaxHP
	ld a, [hli]
	ld b, a
	ld a, [hl]
	ld [H_DIVISOR], a
	ld a, b
	and a ; is max HP > 255?
	jr z, .doDivide
; if max HP > 255, scale both (current HP * 48) and max HP by dividing by 4 so that max HP fits in one byte
; (it needs to be one byte so it can be used as the divisor for the Divide function)
	ld a, [H_DIVISOR]
	srl b
	rr a
	srl b
	rr a
	ld [H_DIVISOR], a
	ld a, [H_PRODUCT + 2]
	ld b, a
	srl b
	ld a, [H_PRODUCT + 3]
	rr a
	srl b
	rr a
	ld [H_PRODUCT + 3], a
	ld a, b
	ld [H_PRODUCT + 2], a
.doDivide
	ld a, [H_PRODUCT + 2]
	ld [H_DIVIDEND], a
	ld a, [H_PRODUCT + 3]
	ld [H_DIVIDEND + 1], a
	ld a, $2
	ld b, a
	call Divide ; divide (current HP * 48) by max HP
	ld a, [H_QUOTIENT + 3]
; set variables for DrawHPBar
	ld e, a
	ld a, $6
	ld d, a
	ld c, a
.drawHPBar
	xor a
	ld [wHPBarType], a
	hlCoord 2, 2
	call DrawHPBar
	ld a, $1
	ld [H_AUTOBGTRANSFERENABLED], a
	ld hl, wcf1e

GetBattleHealthBarColor: ; 3ce90 (f:4e90)
	ld b, [hl]
	call GetHealthBarColor
	ld a, [hl]
	cp b
	ret z
	ld b, $1
	jp GoPAL_SET

; center's mon's name on the battle screen
; if the name is 1 or 2 letters long, it is printed 2 spaces more to the right than usual
; (i.e. for names longer than 4 letters)
; if the name is 3 or 4 letters long, it is printed 1 space more to the right than usual
; (i.e. for names longer than 4 letters)
CenterMonName: ; 3ce9c (f:4e9c)
	push de
	inc hl
	inc hl
	ld b, $2
.loop
	inc de
	ld a, [de]
	cp $50
	jr z, .done
	inc de
	ld a, [de]
	cp $50
	jr z, .done
	dec hl
	dec b
	jr nz, .loop
.done
	pop de
	ret

DisplayBattleMenu: ; 3ceb3 (f:4eb3)
	call LoadScreenTilesFromBuffer1 ; restore saved screen
	ld a, [W_BATTLETYPE]
	and a
	jr nz, .nonstandardbattle
	call DrawHUDsAndHPBars
	call PrintEmptyString
	call SaveScreenTilesToBuffer1
.nonstandardbattle
	ld a, [W_BATTLETYPE]
	cp $2 ; safari
	ld a, BATTLE_MENU_TEMPLATE
	jr nz, .menuselected
	ld a, SAFARI_BATTLE_MENU_TEMPLATE
.menuselected
	ld [wTextBoxID], a
	call DisplayTextBoxID
	ld a, [W_BATTLETYPE]
	dec a
	jp nz, .handleBattleMenuInput ; handle menu input if it's not the old man tutorial
; the following happens for the old man tutorial
	ld hl, wPlayerName
	ld de, W_GRASSRATE
	ld bc, $b
	call CopyData  ; temporarily save the player name in unused space,
	               ; which is supposed to get overwritten when entering a
	               ; map with wild Pokémon. Due to an oversight, the data
	               ; may not get overwritten (cinnabar) and the infamous
	               ; Missingno. glitch can show up.
	ld hl, .oldManName
	ld de, wPlayerName
	ld bc, $b
	call CopyData
; the following simulates the keystrokes by drawing menus on screen
	hlCoord 9, 14
	ld [hl], "▶"
	ld c, $50
	call DelayFrames
	ld [hl], $7f
	hlCoord 9, 16
	ld [hl], "▶"
	ld c, $32
	call DelayFrames
	ld [hl], $ec
	ld a, $2 ; select the "ITEM" menu
	jp .upperLeftMenuItemWasNotSelected
.oldManName
	db "OLD MAN@"
.handleBattleMenuInput
	ld a, [wcc2d]
	ld [wCurrentMenuItem], a
	ld [wLastMenuItem], a
	sub 2 ; check if the cursor is in the left column
	jr c, .leftColumn
; cursor is in the right column
	ld [wCurrentMenuItem], a
	ld [wLastMenuItem], a
	jr .rightColumn
.leftColumn ; put cursor in left column of menu
	ld a, [W_BATTLETYPE]
	cp $2
	ld a, " "
	jr z, .safariLeftColumn
; put cursor in left column for normal battle menu (i.e. when it's not a Safari battle)
	Coorda 15, 14 ; clear upper cursor position in right column
	Coorda 15, 16 ; clear lower cursor position in right column
	ld b, $9 ; top menu item X
	jr .leftColumn_WaitForInput
.safariLeftColumn
	Coorda 13, 14
	Coorda 13, 16
	hlCoord 7, 14
	ld de, W_NUMSAFARIBALLS
	ld bc, $102
	call PrintNumber
	ld b, $1 ; top menu item X
.leftColumn_WaitForInput
	ld hl, wTopMenuItemY
	ld a, $e
	ld [hli], a ; wTopMenuItemY
	ld a, b
	ld [hli], a ; wTopMenuItemX
	inc hl
	inc hl
	ld a, $1
	ld [hli], a ; wMaxMenuItem
	ld [hl], D_RIGHT | A_BUTTON ; wMenuWatchedKeys
	call HandleMenuInput
	bit 4, a ; check if right was pressed
	jr nz, .rightColumn
	jr .AButtonPressed ; the A button was pressed
.rightColumn ; put cursor in right column of menu
	ld a, [W_BATTLETYPE]
	cp $2
	ld a, " "
	jr z, .safariRightColumn
; put cursor in right column for normal battle menu (i.e. when it's not a Safari battle)
	Coorda 9, 14 ; clear upper cursor position in left column
	Coorda 9, 16 ; clear lower cursor position in left column
	ld b, $f ; top menu item X
	jr .rightColumn_WaitForInput
.safariRightColumn
	Coorda 1, 14 ; clear upper cursor position in left column
	Coorda 1, 16 ; clear lower cursor position in left column
	hlCoord 7, 14
	ld de, W_NUMSAFARIBALLS
	ld bc, $102
	call PrintNumber
	ld b, $d ; top menu item X
.rightColumn_WaitForInput
	ld hl, wTopMenuItemY
	ld a, $e
	ld [hli], a ; wTopMenuItemY
	ld a, b
	ld [hli], a ; wTopMenuItemX
	inc hl
	inc hl
	ld a, $1
	ld [hli], a ; wMaxMenuItem
	ld a, D_LEFT | A_BUTTON
	ld [hli], a ; wMenuWatchedKeys
	call HandleMenuInput
	bit 5, a ; check if left was pressed
	jr nz, .leftColumn ; if left was pressed, jump
	ld a, [wCurrentMenuItem]
	add $2 ; if we're in the right column, the actual id is +2
	ld [wCurrentMenuItem], a
.AButtonPressed
	call PlaceUnfilledArrowMenuCursor
	ld a, [W_BATTLETYPE]
	cp $2 ; is it a Safari battle?
	ld a, [wCurrentMenuItem]
	ld [wcc2d], a
	jr z, .handleMenuSelection
; not Safari battle
; swap the IDs of the item menu and party menu (this is probably because they swapped the positions
; of these menu items in first generation English versions)
	cp $1 ; was the item menu selected?
	jr nz, .notItemMenu
; item menu was selected
	inc a ; increment a to 2
	jr .handleMenuSelection
.notItemMenu
	cp $2 ; was the party menu selected?
	jr nz, .handleMenuSelection
; party menu selected
	dec a ; decrement a to 1
.handleMenuSelection
	and a
	jr nz, .upperLeftMenuItemWasNotSelected
; the upper left menu item was selected
	ld a, [W_BATTLETYPE]
	cp $2
	jr z, .throwSafariBallWasSelected
; the "FIGHT" menu was selected
	xor a
	ld [wNumRunAttempts], a
	jp LoadScreenTilesFromBuffer1 ; restore saved screen and return
.throwSafariBallWasSelected
	ld a, SAFARI_BALL
	ld [wcf91], a
	jr UseBagItem

.upperLeftMenuItemWasNotSelected ; a menu item other than the upper left item was selected
	cp $2
	jp nz, PartyMenuOrRockOrRun

; either the bag (normal battle) or bait (safari battle) was selected
	ld a, [wLinkState]
	cp LINK_STATE_BATTLING
	jr nz, .notLinkBattle

; can't use items in link battles
	ld hl, ItemsCantBeUsedHereText
	call PrintText
	jp DisplayBattleMenu

.notLinkBattle
	call SaveScreenTilesToBuffer2
	ld a, [W_BATTLETYPE]
	cp $2 ; is it a safari battle?
	jr nz, BagWasSelected

; bait was selected
	ld a, SAFARI_BAIT
	ld [wcf91], a
	jr UseBagItem

BagWasSelected:
	call LoadScreenTilesFromBuffer1
	ld a, [W_BATTLETYPE]
	and a ; is it a normal battle?
	jr nz, .next

; normal battle
	call DrawHUDsAndHPBars
.next
	ld a, [W_BATTLETYPE]
	dec a ; is it the old man tutorial?
	jr nz, DisplayPlayerBag ; no, it is a normal battle
	ld hl, OldManItemList
	ld a, l
	ld [wList], a
	ld a, h
	ld [wList + 1], a
	jr DisplayBagMenu

OldManItemList:
	db 1 ; # items
	db POKE_BALL, 50
	db -1

DisplayPlayerBag:
	; get the pointer to player's bag when in a normal battle
	ld hl, wNumBagItems
	ld a, l
	ld [wList], a
	ld a, h
	ld [wList + 1], a

DisplayBagMenu:
	xor a
	ld [wcf93], a
	ld a, ITEMLISTMENU
	ld [wListMenuID], a
	ld a, [wcc2c]
	ld [wCurrentMenuItem], a
	call DisplayListMenuID
	ld a, [wCurrentMenuItem]
	ld [wcc2c], a
	ld a, $0
	ld [wcc37], a
	ld [wMenuItemToSwap], a
	jp c, DisplayBattleMenu ; go back to battle menu if an item was not selected

UseBagItem:
	; either use an item from the bag or use a safari zone item
	ld a, [wcf91]
	ld [wd11e], a
	call GetItemName
	call CopyStringToCF4B ; copy name
	xor a
	ld [wd152], a
	call UseItem
	call LoadHudTilePatterns
	call ClearSprites
	xor a
	ld [wCurrentMenuItem], a
	ld a, [W_BATTLETYPE]
	cp $2 ; is it a safari battle?
	jr z, .checkIfMonCaptured

	ld a, [wcd6a]
	and a ; was the item used successfully?
	jp z, BagWasSelected ; if not, go back to the bag menu

	ld a, [W_PLAYERBATTSTATUS1]
	bit UsingTrappingMove, a ; is the player using a multi-turn move like wrap?
	jr z, .checkIfMonCaptured
	ld hl, wPlayerNumAttacksLeft
	dec [hl]
	jr nz, .checkIfMonCaptured
	ld hl, W_PLAYERBATTSTATUS1
	res UsingTrappingMove, [hl] ; not using multi-turn move any more

.checkIfMonCaptured
	ld a, [wd11c]
	and a ; was the enemy mon captured with a ball?
	jr nz, .returnAfterCapturingMon

	ld a, [W_BATTLETYPE]
	cp $2 ; is it a safari battle?
	jr z, .returnAfterUsingItem_NoCapture
; not a safari battle
	call LoadScreenTilesFromBuffer1
	call DrawHUDsAndHPBars
	call Delay3
.returnAfterUsingItem_NoCapture

	call GBPalNormal
	and a ; reset carry
	ret

.returnAfterCapturingMon
	call GBPalNormal
	xor a
	ld [wd11c], a
	ld a, $2
	ld [wBattleResult], a
	scf ; set carry
	ret

ItemsCantBeUsedHereText:
	TX_FAR _ItemsCantBeUsedHereText
	db "@"

PartyMenuOrRockOrRun:
	dec a ; was Run selected?
	jp nz, BattleMenu_RunWasSelected
; party menu or rock was selected
	call SaveScreenTilesToBuffer2
	ld a, [W_BATTLETYPE]
	cp $2 ; is it a safari battle?
	jr nz, .partyMenuWasSelected
; safari battle
	ld a, SAFARI_ROCK
	ld [wcf91], a
	jp UseBagItem
.partyMenuWasSelected
	call LoadScreenTilesFromBuffer1
	xor a
	ld [wd07d], a
	ld [wMenuItemToSwap], a
	call DisplayPartyMenu
.checkIfPartyMonWasSelected
	jp nc, .partyMonWasSelected ; if a party mon was selected, jump, else we quit the party menu
.quitPartyMenu
	call ClearSprites
	call GBPalWhiteOut
	call LoadHudTilePatterns
	call LoadScreenTilesFromBuffer2
	call GoPAL_SET_CF1C
	call GBPalNormal
	jp DisplayBattleMenu
.partyMonDeselected
	hlCoord 11, 11
	ld bc, $81
	ld a, $7f
	call FillMemory
	xor a
	ld [wd07d], a
	call GoBackToPartyMenu
	jr .checkIfPartyMonWasSelected
.partyMonWasSelected
	ld a, SWITCH_STATS_CANCEL_MENU_TEMPLATE
	ld [wTextBoxID], a
	call DisplayTextBoxID
	ld hl, wTopMenuItemY
	ld a, $c
	ld [hli], a ; wTopMenuItemY
	ld [hli], a ; wTopMenuItemX
	xor a
	ld [hli], a ; wCurrentMenuItem
	inc hl
	ld a, $2
	ld [hli], a ; wMaxMenuItem
	ld a, B_BUTTON | A_BUTTON
	ld [hli], a ; wMenuWatchedKeys
	xor a
	ld [hl], a ; wLastMenuItem
	call HandleMenuInput
	bit 1, a ; was A pressed?
	jr nz, .partyMonDeselected ; if B was pressed, jump
; A was pressed
	call PlaceUnfilledArrowMenuCursor
	ld a, [wCurrentMenuItem]
	cp $2 ; was Cancel selected?
	jr z, .quitPartyMenu ; if so, quit the party menu entirely
	and a ; was Switch selected?
	jr z, .switchMon ; if so, jump
; Stats was selected
	xor a
	ld [wcc49], a
	ld hl, wPartyMon1
	call ClearSprites
; display the two status screens
	predef StatusScreen
	predef StatusScreen2
; now we need to reload the enemy mon pic
	ld a, [W_ENEMYBATTSTATUS2]
	bit HasSubstituteUp, a ; does the enemy mon have a substitute?
	ld hl, AnimationSubstitute
	jr nz, .doEnemyMonAnimation
; enemy mon doesn't have substitute
	ld a, [wEnemyHasUsedMinimize]
	and a ; has the enemy mon used Minimise?
	ld hl, AnimationMinimizeMon
	jr nz, .doEnemyMonAnimation
; enemy mon is not minimised
	ld a, [wEnemyMonSpecies]
	ld [wcf91], a
	ld [wd0b5], a
	call GetMonHeader
	ld de, vFrontPic
	call LoadMonFrontSprite
	jr .enemyMonPicReloaded
.doEnemyMonAnimation
	ld b, BANK(AnimationSubstitute) ; BANK(AnimationMinimizeMon)
	call Bankswitch
.enemyMonPicReloaded ; enemy mon pic has been reloaded, so return to the party menu
	jp .partyMenuWasSelected
.switchMon
	ld a, [wPlayerMonNumber]
	ld d, a
	ld a, [wWhichPokemon]
	cp d ; check if the mon to switch to is already out
	jr nz, .notAlreadyOut
; mon is already out
	ld hl, AlreadyOutText
	call PrintText
	jp .partyMonDeselected
.notAlreadyOut
	call HasMonFainted
	jp z, .partyMonDeselected ; can't switch to fainted mon
	ld a, $1
	ld [wcd6a], a
	call GBPalWhiteOut
	call ClearSprites
	call LoadHudTilePatterns
	call LoadScreenTilesFromBuffer1
	call GoPAL_SET_CF1C
	call GBPalNormal
; fall through to SwitchPlayerMon

SwitchPlayerMon: ; 3d1ba (f:51ba)
	callab RetreatMon
	ld c, $32
	call DelayFrames
	call AnimateRetreatingPlayerMon
	ld a, [wWhichPokemon]
	ld [wPlayerMonNumber], a
	ld c, a
	ld b, $1
	push bc
	ld hl, wPartyGainExpFlags
	predef FlagActionPredef
	pop bc
	ld hl, wPartyFoughtCurrentEnemyFlags
	predef FlagActionPredef
	call LoadBattleMonFromParty
	call SendOutMon
	call SaveScreenTilesToBuffer1
	ld a, $2
	ld [wCurrentMenuItem], a
	and a
	ret

AlreadyOutText: ; 3d1f5 (f:51f5)
	TX_FAR _AlreadyOutText
	db "@"

BattleMenu_RunWasSelected: ; 3d1fa (f:51fa)
	call LoadScreenTilesFromBuffer1
	ld a, $3
	ld [wCurrentMenuItem], a
	ld hl, wBattleMonSpeed
	ld de, wEnemyMonSpeed
	call TryRunningFromBattle
	ld a, $0
	ld [wd11f], a
	ret c
	ld a, [wcd6a]
	and a
	ret nz
	jp DisplayBattleMenu

MoveSelectionMenu: ; 3d219 (f:5219)
	ld a, [wMoveMenuType]
	dec a
	jr z, .mimicmenu
	dec a
	jr z, .relearnmenu
	jr .regularmenu

.loadmoves
	ld de, wMoves
	ld bc, NUM_MOVES
	call CopyData
	callab FormatMovesString
	ret

.writemoves
	ld de, wMovesString
	ld a, [hFlags_0xFFF6]
	set 2, a
	ld [hFlags_0xFFF6], a
	call PlaceString
	ld a, [hFlags_0xFFF6]
	res 2, a
	ld [hFlags_0xFFF6], a
	ret

.regularmenu
	call AnyMoveToSelect
	ret z
	ld hl, wBattleMonMoves
	call .loadmoves
	hlCoord 4, 12
	ld b, $4
	ld c, $e
	di
	call TextBoxBorder
	hlCoord 4, 12
	ld [hl], $7a
	hlCoord 10, 12
	ld [hl], $7e
	ei
	hlCoord 6, 13
	call .writemoves
	ld b, $5
	ld a, $c
	jr .menuset
.mimicmenu
	ld hl, wEnemyMonMoves
	call .loadmoves
	hlCoord 0, 7
	ld b, $4
	ld c, $e
	call TextBoxBorder
	hlCoord 2, 8
	call .writemoves
	ld b, $1
	ld a, $7
	jr .menuset
.relearnmenu
	ld a, [wWhichPokemon]
	ld hl, wPartyMon1Moves
	ld bc, $2c
	call AddNTimes
	call .loadmoves
	hlCoord 4, 7
	ld b, $4
	ld c, $e
	call TextBoxBorder
	hlCoord 6, 8
	call .writemoves
	ld b, $5
	ld a, $7
.menuset
	ld hl, wTopMenuItemY
	ld [hli], a
	ld a, b
	ld [hli], a
	ld a, [wMoveMenuType]
	cp $1
	jr z, .selectedmoveknown
	ld a, $1
	jr nc, .selectedmoveknown
	ld a, [wPlayerMoveListIndex]
	inc a
.selectedmoveknown
	ld [hli], a
	inc hl ; wTileBehindCursor untouched
	ld a, [wcd6c]
	inc a
	inc a
	ld [hli], a
	ld a, [wMoveMenuType]
	dec a
	ld b, $c1 ; can't use B
	jr z, .matchedkeyspicked
	dec a
	ld b, $c3
	jr z, .matchedkeyspicked
	ld a, [wLinkState]
	cp LINK_STATE_BATTLING
	jr z, .matchedkeyspicked
	ld a, [W_FLAGS_D733]
	bit 0, a
	ld b, $c7
	jr z, .matchedkeyspicked
	ld b, $ff
.matchedkeyspicked
	ld a, b
	ld [hli], a
	ld a, [wMoveMenuType]
	cp $1
	jr z, .movelistindex1
	ld a, [wPlayerMoveListIndex]
	inc a
.movelistindex1
	ld [hl], a
; fallthrough

SelectMenuItem: ; 3d2fe (f:52fe)
	ld a, [wMoveMenuType]
	and a
	jr z, .battleselect
	dec a
	jr nz, .select
	hlCoord 1, 14
	ld de, WhichTechniqueString
	call PlaceString
	jr .select
.battleselect
	ld a, [W_FLAGS_D733]
	bit 0, a
	jr nz, .select
	call PrintMenuItem
	ld a, [wMenuItemToSwap]
	and a
	jr z, .select
	hlCoord 5, 13
	dec a
	ld bc, $14
	call AddNTimes
	ld [hl], $ec
.select
	ld hl, hFlags_0xFFF6
	set 1, [hl]
	call HandleMenuInput
	ld hl, hFlags_0xFFF6
	res 1, [hl]
	bit 6, a
	jp nz, CursorUp ; up
	bit 7, a
	jp nz, CursorDown ; down
	bit 2, a
	jp nz, SwapMovesInMenu ; select
	bit 1, a ; B, but was it reset above?
	push af
	xor a
	ld [wMenuItemToSwap], a
	ld a, [wCurrentMenuItem]
	dec a
	ld [wCurrentMenuItem], a
	ld b, a
	ld a, [wMoveMenuType]
	dec a ; if not mimic
	jr nz, .nob
	pop af
	ret
.nob
	dec a
	ld a, b
	ld [wPlayerMoveListIndex], a
	jr nz, .moveselected
	pop af
	ret
.moveselected
	pop af
	ret nz
	ld hl, wBattleMonPP
	ld a, [wCurrentMenuItem]
	ld c, a
	ld b, $0
	add hl, bc
	ld a, [hl]
	and $3f
	jr z, .nopp
	ld a, [W_PLAYERDISABLEDMOVE]
	swap a
	and $f
	dec a
	cp c
	jr z, .disabled
	ld a, [W_PLAYERBATTSTATUS3]
	bit 3, a ; transformed
	jr nz, .dummy ; game freak derp
.dummy
	ld a, [wCurrentMenuItem]
	ld hl, wBattleMonMoves
	ld c, a
	ld b, $0
	add hl, bc
	ld a, [hl]
	ld [wPlayerSelectedMove], a
	xor a
	ret
.disabled
	ld hl, MoveDisabledText
	jr .print
.nopp
	ld hl, MoveNoPPText
.print
	call PrintText
	call LoadScreenTilesFromBuffer1
	jp MoveSelectionMenu

MoveNoPPText: ; 3d3ae (f:53ae)
	TX_FAR _MoveNoPPText
	db "@"

MoveDisabledText: ; 3d3b3 (f:53b3)
	TX_FAR _MoveDisabledText
	db "@"

WhichTechniqueString: ; 3d3b8 (f:53b8)
	db "WHICH TECHNIQUE?@"

CursorUp: ; 3d3c9 (f:53c9)
	ld a, [wCurrentMenuItem]
	and a
	jp nz, SelectMenuItem
	call EraseMenuCursor
	ld a, [wcd6c]
	inc a
	ld [wCurrentMenuItem], a
	jp SelectMenuItem

CursorDown: ; 3d3dd (f:53dd)
	ld a, [wCurrentMenuItem]
	ld b, a
	ld a, [wcd6c]
	inc a
	inc a
	cp b
	jp nz, SelectMenuItem
	call EraseMenuCursor
	ld a, $1
	ld [wCurrentMenuItem], a
	jp SelectMenuItem

AnyMoveToSelect: ; 3d3f5 (f:53f5)
; return z and Struggle as the selected move if all moves have 0 PP and/or are disabled
	ld a, STRUGGLE
	ld [wPlayerSelectedMove], a
	ld a, [W_PLAYERDISABLEDMOVE]
	and a
	ld hl, wBattleMonPP
	jr nz, .asm_3d40e
	ld a, [hli]
	or [hl]
	inc hl
	or [hl]
	inc hl
	or [hl]
	and $3f
	ret nz
	jr .asm_3d423
.asm_3d40e
	swap a
	and $f
	ld b, a
	ld d, $5
	xor a
.asm_3d416
	dec d
	jr z, .asm_3d421
	ld c, [hl]
	inc hl
	dec b
	jr z, .asm_3d416
	or c
	jr .asm_3d416
.asm_3d421
	and a
	ret nz
.asm_3d423
	ld hl, NoMovesLeftText
	call PrintText
	ld c, $3c
	call DelayFrames
	xor a
	ret

NoMovesLeftText: ; 3d430 (f:5430)
	TX_FAR _NoMovesLeftText
	db "@"

SwapMovesInMenu: ; 3d435 (f:5435)
	ld a, [wMenuItemToSwap]
	and a
	jr z, .noMenuItemSelected
	ld hl, wBattleMonMoves
	call .swapBytes ; swap moves
	ld hl, wBattleMonPP
	call .swapBytes ; swap move PP
; update the index of the disabled move if necessary
	ld hl, W_PLAYERDISABLEDMOVE
	ld a, [hl]
	swap a
	and $f
	ld b, a
	ld a, [wCurrentMenuItem]
	cp b
	jr nz, .next
	ld a, [hl]
	and $f
	ld b, a
	ld a, [wMenuItemToSwap]
	swap a
	add b
	ld [hl], a
	jr .swapMovesInPartyMon
.next
	ld a, [wMenuItemToSwap]
	cp b
	jr nz, .swapMovesInPartyMon
	ld a, [hl]
	and $f
	ld b, a
	ld a, [wCurrentMenuItem]
	swap a
	add b
	ld [hl], a
.swapMovesInPartyMon
	ld hl, wPartyMon1Moves
	ld a, [wPlayerMonNumber]
	ld bc, wPartyMon2 - wPartyMon1
	call AddNTimes
	push hl
	call .swapBytes ; swap moves
	pop hl
	ld bc, $15
	add hl, bc
	call .swapBytes ; swap move PP
	xor a
	ld [wMenuItemToSwap], a ; deselect the item
	jp MoveSelectionMenu
.swapBytes
	push hl
	ld a, [wMenuItemToSwap]
	dec a
	ld c, a
	ld b, 0
	add hl, bc
	ld d, h
	ld e, l
	pop hl
	ld a, [wCurrentMenuItem]
	dec a
	ld c, a
	ld b, 0
	add hl, bc
	ld a, [de]
	ld b, [hl]
	ld [hl], a
	ld a, b
	ld [de], a
	ret
.noMenuItemSelected
	ld a, [wCurrentMenuItem]
	ld [wMenuItemToSwap], a ; select the current menu item for swapping
	jp MoveSelectionMenu

PrintMenuItem: ; 3d4b6 (f:54b6)
	xor a
	ld [H_AUTOBGTRANSFERENABLED], a
	hlCoord 0, 8
	ld b, $3
	ld c, $9
	call TextBoxBorder
	ld a, [W_PLAYERDISABLEDMOVE]
	and a
	jr z, .notDisabled
	swap a
	and $f
	ld b, a
	ld a, [wCurrentMenuItem]
	cp b
	jr nz, .notDisabled
	hlCoord 1, 10
	ld de, DisabledText
	call PlaceString
	jr .moveDisabled
.notDisabled
	ld hl, wCurrentMenuItem
	dec [hl]
	xor a
	ld [H_WHOSETURN], a
	ld hl, wBattleMonMoves
	ld a, [wCurrentMenuItem]
	ld c, a
	ld b, $0 ; which item in the menu is the cursor pointing to? (0-3)
	add hl, bc ; point to the item (move) in memory
	ld a, [hl] 
	ld [wPlayerSelectedMove], a ; update wPlayerSelectedMove even if the move 
	                            ; isn't actually selected (just pointed to by the cursor)
	ld a, [wPlayerMonNumber]
	ld [wWhichPokemon], a
	ld a, $4
	ld [wcc49], a
	callab GetMaxPP
	ld hl, wCurrentMenuItem
	ld c, [hl]
	inc [hl]
	ld b, $0
	ld hl, wBattleMonPP
	add hl, bc
	ld a, [hl]
	and $3f
	ld [wcd6d], a
; print TYPE/<type> and <curPP>/<maxPP>	
	hlCoord 1, 9
	ld de, TypeText
	call PlaceString
	hlCoord 7, 11
	ld [hl], "/"
	hlCoord 5, 9
	ld [hl], "/"
	hlCoord 5, 11
	ld de, wcd6d
	ld bc, $102
	call PrintNumber
	hlCoord 8, 11
	ld de, wd11e
	ld bc, $102
	call PrintNumber
	call GetCurrentMove 
	hlCoord 2, 10
	predef PrintMoveType
.moveDisabled
	ld a, $1
	ld [H_AUTOBGTRANSFERENABLED], a
	jp Delay3

DisabledText: ; 3d555 (f:5555)
IF DEF(_YELLOW)
	db "Disabled!@"
ELSE
	db "disabled!@"
ENDC

TypeText: ; 3d55f (f:555f)
	db "TYPE@"

; this appears to exchange data with the other gameboy during link battles
LinkBattleExchangeData: ; 3d605 (f:5605)
	ld a, $ff
	ld [wSerialExchangeNybbleReceiveData], a
	ld a, [wPlayerMoveListIndex]
	cp $f ; is the player running from battle?
	jr z, .asm_3d630
	ld a, [wcd6a]
	and a
	jr nz, .asm_3d629
	ld a, [wPlayerSelectedMove]
	cp STRUGGLE
	ld b, $e
	jr z, .asm_3d62f
	dec b
	inc a
	jr z, .asm_3d62f
	ld a, [wPlayerMoveListIndex]
	jr .asm_3d630
.asm_3d629
	ld a, [wWhichPokemon]
	add $4
	ld b, a
.asm_3d62f
	ld a, b
.asm_3d630
	ld [wSerialExchangeNybbleSendData], a
	callab PrintWaitingText
.asm_3d63b
	call Serial_ExchangeNybble
	call DelayFrame
	ld a, [wSerialExchangeNybbleReceiveData]
	inc a
	jr z, .asm_3d63b
	ld b, $a
.asm_3d649
	call DelayFrame
	call Serial_ExchangeNybble
	dec b
	jr nz, .asm_3d649
	ld b, $a
.asm_3d654
	call DelayFrame
	call Serial_SendZeroByte
	dec b
	jr nz, .asm_3d654
	ret

FastAsleepText: ; 3da3d (f:5a3d)
	TX_FAR _FastAsleepText
	db "@"

WokeUpText: ; 3da42 (f:5a42)
	TX_FAR _WokeUpText
	db "@"

IsFrozenText: ; 3da47 (f:5a47)
	TX_FAR _IsFrozenText
	db "@"

FullyParalyzedText: ; 3da4c (f:5a4c)
	TX_FAR _FullyParalyzedText
	db "@"

FlinchedText: ; 3da51 (f:5a51)
	TX_FAR _FlinchedText
	db "@"

MustRechargeText: ; 3da56 (f:5a56)
	TX_FAR _MustRechargeText
	db "@"

DisabledNoMoreText: ; 3da5b (f:5a5b)
	TX_FAR _DisabledNoMoreText
	db "@"

IsConfusedText: ; 3da60 (f:5a60)
	TX_FAR _IsConfusedText
	db "@"

HurtItselfText: ; 3da65 (f:5a65)
	TX_FAR _HurtItselfText
	db "@"

ConfusedNoMoreText: ; 3da6a (f:5a6a)
	TX_FAR _ConfusedNoMoreText
	db "@"

SavingEnergyText: ; 3da6f (f:5a6f)
	TX_FAR _SavingEnergyText
	db "@"

UnleashedEnergyText: ; 3da74 (f:5a74)
	TX_FAR _UnleashedEnergyText
	db "@"

ThrashingAboutText: ; 3da79 (f:5a79)
	TX_FAR _ThrashingAboutText
	db "@"

AttackContinuesText: ; 3da7e (f:5a7e)
	TX_FAR _AttackContinuesText
	db "@"

CantMoveText: ; 3da83 (f:5a83)
	TX_FAR _CantMoveText
	db "@"

PrintMoveIsDisabledText: ; 3da88 (f:5a88)
	ld hl, wPlayerSelectedMove
	ld de, W_PLAYERBATTSTATUS1
	ld a, [H_WHOSETURN]
	and a
	jr z, .removeChargingUp
	inc hl
	ld de, W_ENEMYBATTSTATUS1
.removeChargingUp
	ld a, [de]
	res ChargingUp, a ; end the pokemon's 
	ld [de], a
	ld a, [hl]
	ld [wd11e], a
	call GetMoveName
	ld hl, MoveIsDisabledText
	jp PrintText

MoveIsDisabledText: ; 3daa8 (f:5aa8)
	TX_FAR _MoveIsDisabledText
	db "@"

HandleSelfConfusionDamage: ; 3daad (f:5aad)
	ld hl, HurtItselfText
	call PrintText
	ld hl, wEnemyMonDefense
	ld a, [hli]
	push af
	ld a, [hld]
	push af
	ld a, [wBattleMonDefense]
	ld [hli], a
	ld a, [wBattleMonDefense + 1]
	ld [hl], a
	ld hl, W_PLAYERMOVEEFFECT
	push hl
	ld a, [hl]
	push af
	xor a
	ld [hli], a
	ld [wCriticalHitOrOHKO], a ; self-inflicted confusion damage can't be a Critical Hit
	ld a, 40 ; 40 base power
	ld [hli], a
	xor a
	ld [hl], a
	call GetDamageVarsForPlayerAttack
	call CalculateDamage ; ignores AdjustDamageForMoveType (type-less damage), RandomizeDamage,
	                     ; and MoveHitTest (always hits)
	pop af
	pop hl
	ld [hl], a
	ld hl, wEnemyMonDefense + 1
	pop af
	ld [hld], a
	pop af
	ld [hl], a
	xor a
	ld [wAnimationType], a
	inc a
	ld [H_WHOSETURN], a
	call PlayMoveAnimation
	call DrawPlayerHUDAndHPBar
	xor a
	ld [H_WHOSETURN], a
	jp ApplyDamageToPlayerPokemon

PrintMonName1Text: ; 3daf5 (f:5af5)
	ld hl, MonName1Text
	jp PrintText

MonName1Text: ; 3dafb (f:5afb)
	TX_FAR _MonName1Text
	db $08 ; asm
	ld a, [H_WHOSETURN]
	and a
	ld a, [W_PLAYERMOVENUM]
	ld hl, wPlayerUsedMove
	jr z, .asm_3db11
	ld a, [W_ENEMYMOVENUM]
	ld hl, wEnemyUsedMove
.asm_3db11
	ld [hl], a
	ld [wd11e], a
	call Func_3db85
	ld hl, Used2Text
	ld a, [wd11e]
	cp 3
	ld hl, Used2Text
	ret c
	ld hl, Used1Text
	ret

Used1Text: ; 3db2d (f:5b2d)
	TX_FAR _Used1Text
	db $08 ; asm
	jr PrintInsteadText

Used2Text: ; 3db34 (f:5b34)
	TX_FAR _Used2Text
	db $08 ; asm

PrintInsteadText: ; 3db39 (f:5b39)
	jr PrintCF4BText

InsteadText: ; 3db43 (f:5b43)
	TX_FAR _InsteadText
	db $08 ; asm

PrintCF4BText: ; 3db48 (f:5b48)
	ld hl, CF4BText
	ret

CF4BText: ; 3db4c (f:5b4c)
	TX_FAR _CF4BText
	db $08 ; asm
	ld hl, ExclamationPointPointerTable
	ld a, [wd11e]
	add a
	push bc
	ld b, $0
	ld c, a
	add hl, bc
	pop bc
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ret

ExclamationPointPointerTable: ; 3db62 (f:5b62)
	dw ExclamationPoint1Text
	dw ExclamationPoint2Text
	dw ExclamationPoint3Text
	dw ExclamationPoint4Text
	dw ExclamationPoint5Text

ExclamationPoint1Text: ; 3db6c (f:5b6c)
	TX_FAR _ExclamationPoint1Text
	db "@"

ExclamationPoint2Text: ; 3db71 (f:5b71)
	TX_FAR _ExclamationPoint2Text
	db "@"

ExclamationPoint3Text: ; 3db76 (f:5b76)
	TX_FAR _ExclamationPoint3Text
	db "@"

ExclamationPoint4Text: ; 3db7b (f:5b7b)
	TX_FAR _ExclamationPoint4Text
	db "@"

ExclamationPoint5Text: ; 3db80 (f:5b80)
	TX_FAR _ExclamationPoint5Text
	db "@"

Func_3db85: ; 3db85 (f:5b85)
	push bc
	ld a, [wd11e] ; move number
	ld c, a
	ld b, $0
	ld hl, UnknownMovesList_3dba3
.asm_3db8f
	ld a, [hli]
	cp $ff
	jr z, .asm_3db9d
	cp c
	jr z, .asm_3db9d
	and a
	jr nz, .asm_3db8f
	inc b
	jr .asm_3db8f
.asm_3db9d
	ld a, b
	ld [wd11e], a
	pop bc
	ret

UnknownMovesList_3dba3: ; 3dba3 (f:5ba3)
	db SWORDS_DANCE, GROWTH
	db $00
	db RECOVER, BIDE, SELFDESTRUCT, AMNESIA
	db $00
	db MEDITATE, AGILITY, TELEPORT, MIMIC, DOUBLE_TEAM, BARRAGE
	db $00
	db POUND, SCRATCH, VICEGRIP, WING_ATTACK, FLY, BIND, SLAM, HORN_ATTACK, BODY_SLAM
	db WRAP, THRASH, TAIL_WHIP, LEER, BITE, GROWL, ROAR, SING, PECK, COUNTER
	db STRENGTH, ABSORB, STRING_SHOT, EARTHQUAKE, FISSURE, DIG, TOXIC, SCREECH, HARDEN
	db MINIMIZE, WITHDRAW, DEFENSE_CURL, METRONOME, LICK, CLAMP, CONSTRICT, POISON_GAS
	db LEECH_LIFE, BUBBLE, FLASH, SPLASH, ACID_ARMOR, FURY_SWIPES, REST, SHARPEN, SLASH, SUBSTITUTE
	db $00
	db $FF ; terminator

PrintMoveFailureText: ; 3dbe2 (f:5be2)
	ld de, W_PLAYERMOVEEFFECT
	ld a, [H_WHOSETURN]
	and a
	jr z, .playersTurn
	ld de, W_ENEMYMOVEEFFECT
.playersTurn
	ld hl, DoesntAffectMonText
	ld a, [wDamageMultipliers]
	and $7f
	jr z, .gotTextToPrint
	ld hl, AttackMissedText
	ld a, [wCriticalHitOrOHKO]
	cp $ff
	jr nz, .gotTextToPrint
	ld hl, UnaffectedText
.gotTextToPrint
	push de
	call PrintText
	xor a
	ld [wCriticalHitOrOHKO], a
	pop de
	ld a, [de]
	cp JUMP_KICK_EFFECT
	ret nz

	; if you get here, the mon used jump kick or hi jump kick and missed
	ld hl, W_PLAYERDAMAGE ; since the move missed, W_PLAYERDAMAGE will always contain 0 at this point.
	                ; Thus, recoil damage will always be equal to 1 
	                ; even if it was intended to be potential damage/8.
	ld a, [hli]
	ld b, [hl]
	srl a
	rr b
	srl a
	rr b
	srl a
	rr b
	ld [hl], b
	dec hl
	ld [hli], a
	or b
	jr nz, .applyRecoil
	inc a
	ld [hl], a
.applyRecoil
	ld hl, KeptGoingAndCrashedText
	call PrintText
	ld b, $4
	predef Func_48125
	ld a, [H_WHOSETURN]
	and a
	jr nz, .enemyTurn
	jp ApplyDamageToPlayerPokemon
.enemyTurn
	jp ApplyDamageToEnemyPokemon

AttackMissedText: ; 3dc42 (f:5c42)
	TX_FAR _AttackMissedText
	db "@"

KeptGoingAndCrashedText: ; 3dc47 (f:5c47)
	TX_FAR _KeptGoingAndCrashedText
	db "@"

UnaffectedText: ; 3dc4c (f:5c4c)
	TX_FAR _UnaffectedText
	db "@"

PrintDoesntAffectText: ; 3dc51 (f:5c51)
	ld hl, DoesntAffectMonText
	jp PrintText

DoesntAffectMonText: ; 3dc57 (f:5c57)
	TX_FAR _DoesntAffectMonText
	db "@"

; if there was a critical hit or an OHKO was successful, print the corresponding text
PrintCriticalOHKOText: ; 3dc5c (f:5c5c)
	ld a, [wCriticalHitOrOHKO]
	and a
	jr z, .done ; do nothing if there was no critical hit or successful OHKO
	dec a
	add a
	ld hl, CriticalOHKOTextPointers
	ld b, $0
	ld c, a
	add hl, bc
	ld a, [hli]
	ld h, [hl]
	ld l, a
	call PrintText
	xor a
	ld [wCriticalHitOrOHKO], a
.done
	ld c, $14
	jp DelayFrames

CriticalOHKOTextPointers: ; 3dc7a (f:5c7a)
	dw CriticalHitText
	dw OHKOText

CriticalHitText: ; 3dc7e (f:5c7e)
	TX_FAR _CriticalHitText
	db "@"

OHKOText: ; 3dc83 (f:5c83)
	TX_FAR _OHKOText
	db "@"

; function to determine if Counter hits and if so, how much damage it does
HandleCounterMove: ; 3e093 (f:6093)
; The variables checked by Counter are updated whenever the cursor points to a new move in the battle selection menu.
; This is irrelevant for the opponent's side outside of link battles, since the move selection is controlled by the AI.
; However, in the scenario where the player switches out and the opponent uses Counter,
; the outcome may be affected by the player's actions in the move selection menu prior to switching the Pokemon.
; This might also lead to desync glitches in link battles.

	ld a,[H_WHOSETURN] ; whose turn
	and a
; player's turn
	ld hl,wEnemySelectedMove
	ld de,W_ENEMYMOVEPOWER
	ld a,[wPlayerSelectedMove]
	jr z,.next
; enemy's turn
	ld hl,wPlayerSelectedMove
	ld de,W_PLAYERMOVEPOWER
	ld a,[wEnemySelectedMove]
.next
	cp a,COUNTER
	ret nz ; return if not using Counter
	ld a,$01
	ld [W_MOVEMISSED],a ; initialize the move missed variable to true (it is set to false below if the move hits)
	ld a,[hl]
	cp a,COUNTER
	ret z ; miss if the opponent's last selected move is Counter.
	ld a,[de]
	and a
	ret z ; miss if the opponent's last selected move's Base Power is 0.
; check if the move the target last selected was Normal or Fighting type
	inc de
	ld a,[de]
	and a ; normal type
	jr z,.counterableType
	cp a,FIGHTING
	jr z,.counterableType
; if the move wasn't Normal or Fighting type, miss
	xor a
	ret
.counterableType
	ld hl,W_PLAYERDAMAGE
	ld a,[hli]
	or [hl]
	ret z ; If we made it here, Counter still misses if the last move used in battle did no damage to its target.
	      ; W_PLAYERDAMAGE is shared by both players, so Counter may strike back damage dealt by the Counter user itself 
	      ; if the conditions meet, even though 99% of the times damage will come from the target.
; if it did damage, double it
	ld a,[hl]
	add a
	ldd [hl],a
	ld a,[hl]
	adc a
	ld [hl],a
	jr nc,.noCarry
; damage is capped at 0xFFFF
	ld a,$ff
	ld [hli],a
	ld [hl],a
.noCarry
	xor a
	ld [W_MOVEMISSED],a
	call MoveHitTest ; do the normal move hit test in addition to Counter's special rules
	xor a
	ret


; this function raises the attack modifier of a pokemon using Rage when that pokemon is attacked
HandleBuildingRage: ; 3e2b6 (f:62b6)
; values for the player turn
	ld hl,W_ENEMYBATTSTATUS2
	ld de,wEnemyMonStatMods
	ld bc,W_ENEMYMOVENUM
	ld a,[H_WHOSETURN]
	and a
	jr z,.next
; values for the enemy turn
	ld hl,W_PLAYERBATTSTATUS2
	ld de,wPlayerMonStatMods
	ld bc,W_PLAYERMOVENUM
.next
	bit UsingRage,[hl] ; is the pokemon being attacked under the effect of Rage?
	ret z ; return if not
	ld a,[de]
	cp a,$0d ; maximum stat modifier value
	ret z ; return if attack modifier is already maxed
	ld a,[H_WHOSETURN]
	xor a,$01 ; flip turn for the stat modifier raising function
	ld [H_WHOSETURN],a
; temporarily change the target pokemon's move to $00 and the effect to the one
; that causes the attack modifier to go up one stage
	ld h,b
	ld l,c
	ld [hl],$00 ; null move number
	inc hl
	ld [hl],ATTACK_UP1_EFFECT
	push hl
	ld hl,BuildingRageText
	call PrintText
	call StatModifierUpEffect ; stat modifier raising function
	pop hl
	xor a
	ldd [hl],a ; null move effect
	ld a,RAGE
	ld [hl],a ; restore the target pokemon's move number to Rage
	ld a,[H_WHOSETURN]
	xor a,$01 ; flip turn back to the way it was
	ld [H_WHOSETURN],a
	ret

BuildingRageText: ; 3e2f8 (f:62f8)
	TX_FAR _BuildingRageText
	db "@"

; copy last move for Mirror Move
; sets zero flag on failure and unsets zero flag on success
MirrorMoveCopyMove: ; 3e2fd (f:62fd)
; Mirror Move makes use of ccf1 (wPlayerUsedMove) and ccf2 (wEnemyUsedMove) addresses,
; which are mainly used to print the "[Pokemon] used [Move]" text.
; Both are set to 0 whenever a new Pokemon is sent out
; ccf1 is also set to 0 whenever the player is fast asleep or frozen solid.
; ccf2 is also set to 0 whenever the enemy is fast asleep or frozen solid.

	ld a,[H_WHOSETURN]
	and a
; values for player turn
	ld a,[wEnemyUsedMove]
	ld hl,wPlayerSelectedMove
	ld de,W_PLAYERMOVENUM
	jr z,.next
; values for enemy turn
	ld a,[wPlayerUsedMove]
	ld de,W_ENEMYMOVENUM
	ld hl,wEnemySelectedMove
.next
	ld [hl],a
	cp a,MIRROR_MOVE ; did the target Pokemon last use Mirror Move, and miss?
	jr z,.mirrorMoveFailed
	and a ; has the target selected any move yet?
	jr nz,ReloadMoveData
.mirrorMoveFailed
	ld hl,MirrorMoveFailedText
	call PrintText
	xor a
	ret

MirrorMoveFailedText: ; 3e324 (f:6324)
	TX_FAR _MirrorMoveFailedText
	db "@"

; function used to reload move data for moves like Mirror Move and Metronome
ReloadMoveData: ; 3e329 (f:6329)
	ld [wd11e],a
	dec a
	ld hl,Moves
	ld bc,$0006
	call AddNTimes
	ld a,BANK(Moves)
	call FarCopyData ; copy the move's stats
	call IncrementMovePP
; the follow two function calls are used to reload the move name
	call GetMoveName
	call CopyStringToCF4B
	ld a,$01
	and a
	ret

; function that picks a random move for metronome
MetronomePickMove: ; 3e348 (f:6348)
	xor a
	ld [wAnimationType],a
	ld a,METRONOME
	call PlayMoveAnimation ; play Metronome's animation
; values for player turn
	ld de,W_PLAYERMOVENUM
	ld hl,wPlayerSelectedMove
	ld a,[H_WHOSETURN]
	and a
	jr z,.pickMoveLoop
; values for enemy turn
	ld de,W_ENEMYMOVENUM
	ld hl,wEnemySelectedMove
; loop to pick a random number in the range [1, $a5) to be the move used by Metronome
.pickMoveLoop
	call BattleRandom
	and a
	jr z,.pickMoveLoop
	cp a,NUM_ATTACKS + 1 ; max normal move number + 1 (this is Struggle's move number)
	jr nc,.pickMoveLoop
	cp a,METRONOME
	jr z,.pickMoveLoop
	ld [hl],a
	jr ReloadMoveData

; this function increments the current move's PP
; it's used to prevent moves that run another move within the same turn
; (like Mirror Move and Metronome) from losing 2 PP
IncrementMovePP: ; 3e373 (f:6373)
	ld a,[H_WHOSETURN]
	and a
; values for player turn
	ld hl,wBattleMonPP
	ld de,wPartyMon1PP
	ld a,[wPlayerMoveListIndex]
	jr z,.next
; values for enemy turn
	ld hl,wEnemyMonPP
	ld de,wEnemyMon1PP
	ld a,[wEnemyMoveListIndex]
.next
	ld b,$00
	ld c,a
	add hl,bc
	inc [hl] ; increment PP in the currently battling pokemon memory location
	ld h,d
	ld l,e
	add hl,bc
	ld a,[H_WHOSETURN]
	and a
	ld a,[wPlayerMonNumber] ; value for player turn
	jr z,.updatePP
	ld a,[wEnemyMonPartyPos] ; value for enemy turn
.updatePP
	ld bc,wEnemyMon2 - wEnemyMon1
	call AddNTimes
	inc [hl] ; increment PP in the party memory location
	ret

; function to tell how effective the type of an enemy attack is on the player's current pokemon
; this doesn't take into account the effects that dual types can have
; (e.g. 4x weakness / resistance, weaknesses and resistances canceling)
; the result is stored in [wd11e]
; ($05 is not very effective, $10 is neutral, $14 is super effective)
; as far is can tell, this is only used once in some AI code to help decide which move to use
AIGetTypeEffectiveness: ; 3e449 (f:6449)
	ld a,[W_ENEMYMOVETYPE]
	ld d,a                 ; d = type of enemy move
	ld hl,wBattleMonType
	ld b,[hl]              ; b = type 1 of player's pokemon
	inc hl
	ld c,[hl]              ; c = type 2 of player's pokemon
	ld a,$10
	ld [wd11e],a           ; initialize [wd11e] to neutral effectiveness
	ld hl,TypeEffects
.loop
	ld a,[hli]
	cp a,$ff
	ret z
	cp d                   ; match the type of the move
	jr nz,.nextTypePair1
	ld a,[hli]
	cp b                   ; match with type 1 of pokemon
	jr z,.done
	cp c                   ; or match with type 2 of pokemon
	jr z,.done
	jr .nextTypePair2
.nextTypePair1
	inc hl
.nextTypePair2
	inc hl
	jr .loop
.done
	ld a,[hl]
	ld [wd11e],a           ; store damage multiplier
	ret

INCLUDE "data/type_effects.asm"

; values for player turn
CalcHitChance: ; 3e624 (f:6624)
	ld hl,W_PLAYERMOVEACCURACY
	ld a,[H_WHOSETURN]
	and a
	ld a,[wPlayerMonAccuracyMod]
	ld b,a
	ld a,[wEnemyMonEvasionMod]
	ld c,a
	jr z,.next
; values for enemy turn
	ld hl,W_ENEMYMOVEACCURACY
	ld a,[wEnemyMonAccuracyMod]
	ld b,a
	ld a,[wPlayerMonEvasionMod]
	ld c,a
.next
	ld a,$0e
	sub c
	ld c,a ; c = 14 - EVASIONMOD (this "reflects" the value over 7, so that an increase in the target's evasion
	       ; decreases the hit chance instead of increasing the hit chance)
; zero the high bytes of the multiplicand
	xor a
	ld [H_MULTIPLICAND],a
	ld [H_MULTIPLICAND + 1],a
	ld a,[hl]
	ld [H_MULTIPLICAND + 2],a ; set multiplicand to move accuracy
	push hl
	ld d,$02 ; loop has two iterations
; loop to do the calculations, the first iteration multiplies by the accuracy ratio and
; the second iteration multiplies by the evasion ratio
.loop
	push bc
	ld hl, StatModifierRatios  ; $76cb ; stat modifier ratios
	dec b
	sla b
	ld c,b
	ld b,$00
	add hl,bc ; hl = address of stat modifier ratio
	pop bc
	ld a,[hli]
	ld [H_MULTIPLIER],a ; set multiplier to the numerator of the ratio
	call Multiply
	ld a,[hl]
	ld [H_DIVISOR],a ; set divisor to the the denominator of the ratio
	                 ; (the dividend is the product of the previous multiplication)
	ld b,$04 ; number of bytes in the dividend
	call Divide
	ld a,[H_QUOTIENT + 3]
	ld b,a
	ld a,[H_QUOTIENT + 2]
	or b
	jp nz,.nextCalculation
; make sure the result is always at least one
	ld [H_QUOTIENT + 2],a
	ld a,$01
	ld [H_QUOTIENT + 3],a
.nextCalculation
	ld b,c
	dec d
	jr nz,.loop
	ld a,[H_QUOTIENT + 2]
	and a ; is the calculated hit chance over 0xFF?
	ld a,[H_QUOTIENT + 3]
	jr z,.storeAccuracy
; if calculated hit chance over 0xFF
	ld a,$ff ; set the hit chance to 0xFF
.storeAccuracy
	pop hl
	ld [hl],a ; store the hit chance in the move accuracy variable
	ret

HitXTimesText: ; 3e887 (f:6887)
	TX_FAR _HitXTimesText
	db "@"

GetCurrentMove: ; 3eabe (f:6abe)
	ld a, [H_WHOSETURN]
	and a
	jp z, .player
	ld de, W_ENEMYMOVENUM
	ld a, [wEnemySelectedMove]
	jr .selected
.player
	ld de, W_PLAYERMOVENUM
	ld a, [wPlayerSelectedMove]
.selected
	ld [wd0b5], a
	dec a
	ld hl, Moves
	ld bc, $6
	call AddNTimes
	ld a, BANK(Moves)
	call FarCopyData

	ld a, BANK(MoveNames)
	ld [wPredefBank], a
	ld a, MOVE_NAME
	ld [wNameListType], a
	call GetName
	ld de, wcd6d
	jp CopyStringToCF4B	

; calls BattleTransition to show the battle transition animation and initializes some battle variables
DoBattleTransition: ; 3ec32 (f:6c32)
	ld a, [wLinkState]
	cp LINK_STATE_BATTLING
	jr nz, .next
; link battle
	xor a
	ld [wMenuJoypadPollCount], a
	callab DisplayLinkBattleVersusTextBox
	ld a, $1
	ld [wUpdateSpritesEnabled], a
	call ClearScreen
.next
	call DelayFrame
	predef BattleTransition
	callab LoadHudAndHpBarAndStatusTilePatterns
	ld a, $1
	ld [H_AUTOBGTRANSFERENABLED], a
	ld a, $ff
	ld [wUpdateSpritesEnabled], a
	call ClearSprites
	call ClearScreen
	xor a
	ld [H_AUTOBGTRANSFERENABLED], a
	ld [hWY], a
	ld [rWY], a
	ld [hTilesetType], a
	ret

; swaps the level values of the BattleMon and EnemyMon structs
SwapPlayerAndEnemyLevels: ; 3ec81 (f:6c81)
	push bc
	ld a, [wBattleMonLevel]
	ld b, a
	ld a, [wEnemyMonLevel]
	ld [wBattleMonLevel], a
	ld a, b
	ld [wEnemyMonLevel], a
	pop bc
	ret

; loads either red back pic or old man back pic
; also writes OAM data and loads tile patterns for the Red or Old Man back sprite's head
; (for use when scrolling the player sprite and enemy's silhouettes on screen)
LoadPlayerBackPic: ; 3ec92 (f:6c92)
	ld a, [W_BATTLETYPE]
	dec a ; is it the old man tutorial?
	ld de, RedPicBack
	jr nz, .next
	ld de, OldManPic
.next
	ld a, BANK(RedPicBack)
	call UncompressSpriteFromDE
	call LoadMonBackSpriteHook ; No pixelated backsprites
	ld hl, wOAMBuffer
	xor a
	ld [$FF8B], a ; initial tile number
	ld b, $7 ; 7 columns
	ld e, $a0 ; X for the left-most column
.loop ; each loop iteration writes 3 OAM entries in a vertical column
	ld c, $3 ; 3 tiles per column
	ld d, $38 ; Y for the top of each column
.innerLoop ; each loop iteration writes 1 OAM entry in the column
	ld [hl], d ; OAM Y
	inc hl
	ld [hl], e ; OAM X
	ld a, $8 ; height of tile
	add d ; increase Y by height of tile
	ld d, a
	inc hl
	ld a, [$FF8B]
	ld [hli], a ; OAM tile number
	inc a ; increment tile number
	ld [$FF8B], a
	inc hl
	dec c
	jr nz, .innerLoop
	ld a, [$FF8B]
	add $4 ; increase tile number by 4
	ld [$FF8B], a
	ld a, $8 ; width of tile
	add e ; increase X by width of tile
	ld e, a
	dec b
	jr nz, .loop	
	ld a, $a
	ld [$0], a
	xor a
	ld [$4000], a
	ld hl, vSprites
	ld de, S_SPRITEBUFFER1
	ld a, [H_LOADEDROMBANK]
	ld b, a
	ld c, 7 * 7
	call CopyVideoData
	xor a
	ld [$0], a
	ld a, $31
	ld [$ffe1], a
	hlCoord 1, 5
	predef_jump Func_3f0c6
	
UnusedPredef: ; 3ed02 (f:6d02)
	ret

ScrollTrainerPicAfterBattle: ; 3ed12 (f:6d12)
	ld hl, _ScrollTrainerPicAfterBattle
	ld b, BANK(_ScrollTrainerPicAfterBattle)
	jp Bankswitch

ApplyBurnAndParalysisPenaltiesToPlayer: ; 3ed1a (f:6d1a)
	ld a, $1
	jr ApplyBurnAndParalysisPenalties

ApplyBurnAndParalysisPenaltiesToEnemy: ; 3ed1e (f:6d1e)
	xor a

ApplyBurnAndParalysisPenalties: ; 3ed1f (f:6d1f)
	ld [H_WHOSETURN], a
	call QuarterSpeedDueToParalysis
	jp HalveAttackDueToBurn

QuarterSpeedDueToParalysis: ; 3ed27 (f:6d27)
	ld a, [H_WHOSETURN]
	and a
	jr z, .playerTurn
.enemyTurn ; quarter the player's speed
	ld a, [wBattleMonStatus]
	and 1 << PAR
	ret z ; return if player not paralysed
	ld hl, wBattleMonSpeed + 1
	ld a, [hld]
	ld b, a
	ld a, [hl]
	srl a
	rr b
	srl a
	rr b
	ld [hli], a
	or b
	jr nz, .storePlayerSpeed
	ld b, 1 ; give the player a minimum of at least one speed point
.storePlayerSpeed
	ld [hl], b
	ret
.playerTurn ; quarter the enemy's speed
	ld a, [wEnemyMonStatus]
	and 1 << PAR
	ret z ; return if enemy not paralysed
	ld hl, wEnemyMonSpeed + 1
	ld a, [hld]
	ld b, a
	ld a, [hl]
	srl a
	rr b
	srl a
	rr b
	ld [hli], a
	or b
	jr nz, .storeEnemySpeed
	ld b, 1 ; give the enemy a minimum of at least one speed point
.storeEnemySpeed
	ld [hl], b
	ret

HalveAttackDueToBurn: ; 3ed64 (f:6d64)
	ld a, [H_WHOSETURN]
	and a
	jr z, .playerTurn
.enemyTurn ; halve the player's attack
	ld a, [wBattleMonStatus]
	and 1 << BRN
	ret z ; return if player not burnt
	ld hl, wBattleMonAttack + 1
	ld a, [hld]
	ld b, a
	ld a, [hl]
	srl a
	rr b
	ld [hli], a
	or b
	jr nz, .storePlayerAttack
	ld b, 1 ; give the player a minimum of at least one attack point
.storePlayerAttack
	ld [hl], b
	ret
.playerTurn ; halve the enemy's attack
	ld a, [wEnemyMonStatus]
	and 1 << BRN
	ret z ; return if enemy not burnt
	ld hl, wEnemyMonAttack + 1
	ld a, [hld]
	ld b, a
	ld a, [hl]
	srl a
	rr b
	ld [hli], a
	or b
	jr nz, .storeEnemyAttack
	ld b, 1 ; give the enemy a minimum of at least one attack point
.storeEnemyAttack
	ld [hl], b
	ret

CalculateModifiedStats: ; 3ed99 (f:6d99)
	ld c, 0
.loop
	call CalculateModifiedStat
	inc c
	ld a, c
	cp 4
	jr nz, .loop
	ret

; calculate modified stat for stat c (0 = attack, 1 = defense, 2 = speed, 3 = special)
CalculateModifiedStat: ; 3eda5 (f:6da5)
	push bc
	push bc
	ld a, [wd11e]
	and a
	ld a, c
	ld hl, wBattleMonAttack
	ld de, wPlayerMonUnmodifiedAttack
	ld bc, wPlayerMonAttackMod
	jr z, .next
	ld hl, wEnemyMonAttack
	ld de, wEnemyMonUnmodifiedAttack
	ld bc, wEnemyMonStatMods
.next
	add c
	ld c, a
	jr nc, .noCarry1
	inc b
.noCarry1
	ld a, [bc]
	pop bc
	ld b, a
	push bc
	sla c
	ld b, 0
	add hl, bc
	ld a, c
	add e
	ld e, a
	jr nc, .noCarry2
	inc d
.noCarry2
	pop bc
	push hl
	ld hl, StatModifierRatios
	dec b
	sla b
	ld c, b
	ld b, 0
	add hl, bc
	xor a
	ld [H_MULTIPLICAND], a
	ld a, [de]
	ld [H_MULTIPLICAND + 1], a
	inc de
	ld a, [de]
	ld [H_MULTIPLICAND + 2], a
	ld a, [hli]
	ld [H_MULTIPLIER], a
	call Multiply
	ld a, [hl]
	ld [H_DIVISOR], a
	ld b, $4
	call Divide
	pop hl
	ld a, [H_DIVIDEND + 3]
	sub 999 % $100
	ld a, [H_DIVIDEND + 2]
	sbc 999 / $100
	jp c, .storeNewStatValue
; cap the stat at 999
	ld a, 999 / $100
	ld [H_DIVIDEND + 2], a
	ld a, 999 % $100
	ld [H_DIVIDEND + 3], a
.storeNewStatValue
	ld a, [H_DIVIDEND + 2]
	ld [hli], a
	ld b, a
	ld a, [H_DIVIDEND + 3]
	ld [hl], a
	or b
	jr nz, .done
	inc [hl] ; if the stat is 0, bump it up to 1
.done
	pop bc
	ret

ApplyBadgeStatBoosts: ; 3ee19 (f:6e19)
	ld a, [wLinkState]
	cp LINK_STATE_BATTLING
	ret z ; return if link battle
	ld a, [W_OBTAINEDBADGES]
	ld b, a
	ld hl, wBattleMonAttack
	ld c, $4
; the boost is applied for badges whose bit position is even
; the order of boosts matches the order they are laid out in RAM
; Boulder (bit 0) - attack
; Thunder (bit 2) - defense
; Soul (bit 4) - speed
; Volcano (bit 6) - special
.loop
	srl b
	call c, .applyBoostToStat
	inc hl
	inc hl
	srl b
	dec c
	jr nz, .loop
	ret

; multiply stat at hl by 1.125
; cap stat at 999
.applyBoostToStat
	ld a, [hli]
	ld d, a
	ld e, [hl]
	srl d
	rr e
	srl d
	rr e
	srl d
	rr e
	ld a, [hl]
	add e
	ld [hld], a
	ld a, [hl]
	adc d
	ld [hli], a
	ld a, [hld]
	sub 999 % $100
	ld a, [hl]
	sbc 999 / $100
	ret c
	ld a, 999 / $100
	ld [hli], a
	ld a, 999 % $100
	ld [hld], a
	ret

LoadHudAndHpBarAndStatusTilePatterns: ; 3ee58 (f:6e58)
	call LoadHpBarAndStatusTilePatterns

LoadHudTilePatterns: ; 3ee5b (f:6e5b)
	ld a, [rLCDC]
	add a ; is LCD disabled?
	jr c, .lcdEnabled
.lcdDisabled
	ld hl, BattleHudTiles1
	ld de, vChars2 + $6d0
	ld bc, $18
	ld a, BANK(BattleHudTiles1)
	call FarCopyDataDouble
	ld hl, BattleHudTiles2
	ld de, vChars2 + $730
	ld bc, $30
	ld a, BANK(BattleHudTiles2)
	jp FarCopyDataDouble
.lcdEnabled
	ld de, BattleHudTiles1
	ld hl, vChars2 + $6d0
	ld bc, (BANK(BattleHudTiles1) << 8) + $03
	call CopyVideoDataDouble
	ld de, BattleHudTiles2
	ld hl, vChars2 + $730
	ld bc, (BANK(BattleHudTiles2) << 8) + $06
	jp CopyVideoDataDouble

PrintEmptyString: ; 3ee94 (f:6e94)
	ld hl, .emptyString
	jp PrintText
.emptyString
	db "@"


BattleRandom:
; Link battles use a shared PRNG.

	ld a, [wLinkState]
	cp LINK_STATE_BATTLING
	jp nz, Random

	push hl
	push bc
	ld a, [wLinkBattleRandomNumberListIndex]
	ld c, a
	ld b, 0
	ld hl, wLinkBattleRandomNumberList
	add hl, bc
	inc a
	ld [wLinkBattleRandomNumberListIndex], a
	cp 9
	ld a, [hl]
	pop bc
	pop hl
	ret c

; if we picked the last seed, we need to recalculate the nine seeds
	push hl
	push bc
	push af
	
; point to seed 0 so we pick the first number the next time	
	xor a
	ld [wLinkBattleRandomNumberListIndex], a

	ld hl, wLinkBattleRandomNumberList
	ld b, 9
.loop
	ld a, [hl]
	ld c, a
; multiply by 5	
	add a
	add a
	add c
; add 1	
	inc a
	ld [hli], a
	dec b
	jr nz, .loop

	pop af
	pop bc
	pop hl
	ret


HandleExplodingAnimation: ; 3eed3 (f:6ed3)
	ld a, [H_WHOSETURN]
	and a
	ld hl, wEnemyMonType1 ; wcfea
	ld de, W_ENEMYBATTSTATUS1
	ld a, [W_PLAYERMOVENUM]
	jr z, .asm_3eeea
	ld hl, wBattleMonType1 ; wd019
	ld de, W_ENEMYBATTSTATUS1
	ld a, [W_ENEMYMOVENUM]
.asm_3eeea
	cp SELFDESTRUCT
	jr z, .asm_3eef1
	cp EXPLOSION
	ret nz
.asm_3eef1
	ld a, [de]
	bit Invulnerable, a ; fly/dig
	ret nz
	ld a, [hli]
	cp GHOST
	ret z
	ld a, [hl]
	cp GHOST
	ret z
	ld a, [W_MOVEMISSED]
	and a
	ret nz
	ld a, 5
	ld [wAnimationType], a

PlayMoveAnimation: ; 3ef07 (f:6f07)
	ld [W_ANIMATIONID],a
	call Delay3
	predef_jump MoveAnimation
	
_LoadTrainerPic: ; 3f04b (f:704b)
; wd033-wd034 contain pointer to pic
	ld a, [wTrainerPicPointer] ; wd033
	ld e, a
	ld a, [wTrainerPicPointer + 1] ; wd034
	ld d, a ; de contains pointer to trainer pic
	ld a, [wLinkState]
	and a
	ld a, Bank(TrainerPics) ; this is where all the trainer pics are (not counting Red's)
	jr z, .loadSprite
	ld a, Bank(RedPicFront)
.loadSprite
	call UncompressSpriteFromDE
	ld de, vFrontPic
	ld a, $77
	ld c, a
	jp LoadUncompressedSpriteData

; unreferenced
Func_3f069: ; 3f069 (f:7069)
	xor a
	ld [wc0f1], a
	ld [wc0f2], a
	jp PlaySound

Func_3f073: ; 3f073 (f:7073)
	ld a, [wPredefRegisters]
	ld h, a
	ld a, [wPredefRegisters + 1]
	ld l, a
	ld a, [$ffe1]
	ld [H_DOWNARROWBLINKCNT1], a
	ld b, $4c
	ld a, [W_ISINBATTLE]
	and a
	jr z, .asm_3f0bc
	add b
	ld [hl], a
	call Delay3
	ld bc, -41
	add hl, bc
	ld a, $1
	ld [wcd6c], a
	ld bc, $303
	predef Func_79aba
	ld c, $4
	call DelayFrames
	ld bc, -41
	add hl, bc
	xor a
	ld [wcd6c], a
	ld bc, $505
	predef Func_79aba
	ld c, $5
	call DelayFrames
	ld bc, -41
	jr .asm_3f0bf
.asm_3f0bc
	ld bc, -123
.asm_3f0bf
	add hl, bc
	ld a, [H_DOWNARROWBLINKCNT1]
	add $31
	jr asm_3f0d0

Func_3f0c6: ; 3f0c6 (f:70c6)
	ld a, [wPredefRegisters]
	ld h, a
	ld a, [wPredefRegisters + 1]
	ld l, a
	ld a, [$ffe1]
asm_3f0d0: ; 3f0d0 (f:70d0)
	ld bc, $707
	ld de, $14
	push af
	ld a, [W_SPRITEFLIPPED]
	and a
	jr nz, .asm_3f0ed
	pop af
.asm_3f0de
	push bc
	push hl
.asm_3f0e0
	ld [hl], a
	add hl, de
	inc a
	dec c
	jr nz, .asm_3f0e0
	pop hl
	inc hl
	pop bc
	dec b
	jr nz, .asm_3f0de
	ret
	
.asm_3f0ed
	push bc
	ld b, $0
	dec c
	add hl, bc
	pop bc
	pop af
.asm_3f0f4
	push bc
	push hl
.asm_3f0f6
	ld [hl], a
	add hl, de
	inc a
	dec c
	jr nz, .asm_3f0f6
	pop hl
	dec hl
	pop bc
	dec b
	jr nz, .asm_3f0f4
	ret

LoadMonBackPic: ; 3f103 (f:7103)
; Assumes the monster's attributes have
; been loaded with GetMonHeader.
	ld a, [wBattleMonSpecies2]
	ld [wcf91], a
	hlCoord 1, 5
	ld b, $7
	ld c, $8
	call ClearScreenArea
	ld hl,  W_MONHBACKSPRITE - W_MONHEADER
	call UncompressMonSprite
	ld a, $3
	call LoadMonBackSpriteHook
	ld hl, vSprites
	ld de, vBackPic
	ld c, (2*SPRITEBUFFERSIZE)/16 ; count of 16-byte chunks to be copied
	ld a, [H_LOADEDROMBANK]
	ld b, a
	jp CopyVideoData

JumpMoveEffect: ; 3f132 (f:7132)
	call _JumpMoveEffect
	ld b, $1
	ret

_JumpMoveEffect: ; 3f138 (f:7138)
	ld a, [H_WHOSETURN]
	and a
	ld a, [W_PLAYERMOVEEFFECT]
	jr z, .next1
	ld a, [W_ENEMYMOVEEFFECT]
.next1
	dec a ; subtract 1, there is no special effect for 00
	add a ; x2, 16bit pointers
	ld hl, MoveEffectPointerTable
	ld b, 0
	ld c, a
	add hl, bc
	ld a, [hli]
	ld h, [hl]
	ld l, a
	jp [hl] ; jump to special effect handler

MoveEffectPointerTable: ; 3f150 (f:7150)
	 dw SleepEffect               ; unused effect
	 dw PoisonEffect              ; POISON_SIDE_EFFECT1
	 dw DrainHPEffect             ; DRAIN_HP_EFFECT
	 dw FreezeBurnParalyzeEffect  ; BURN_SIDE_EFFECT1
	 dw FreezeBurnParalyzeEffect  ; FREEZE_SIDE_EFFECT
	 dw FreezeBurnParalyzeEffect  ; PARALYZE_SIDE_EFFECT1
	 dw ExplodeEffect             ; EXPLODE_EFFECT
	 dw DrainHPEffect             ; DREAM_EATER_EFFECT
	 dw $0000                     ; MIRROR_MOVE_EFFECT
	 dw StatModifierUpEffect      ; ATTACK_UP1_EFFECT
	 dw StatModifierUpEffect      ; DEFENSE_UP1_EFFECT
	 dw StatModifierUpEffect      ; SPEED_UP1_EFFECT
	 dw StatModifierUpEffect      ; SPECIAL_UP1_EFFECT
	 dw StatModifierUpEffect      ; ACCURACY_UP1_EFFECT
	 dw StatModifierUpEffect      ; EVASION_UP1_EFFECT
	 dw PayDayEffect              ; PAY_DAY_EFFECT
	 dw $0000                     ; SWIFT_EFFECT
	 dw StatModifierDownEffect    ; ATTACK_DOWN1_EFFECT
	 dw StatModifierDownEffect    ; DEFENSE_DOWN1_EFFECT
	 dw StatModifierDownEffect    ; SPEED_DOWN1_EFFECT
	 dw StatModifierDownEffect    ; SPECIAL_DOWN1_EFFECT
	 dw StatModifierDownEffect    ; ACCURACY_DOWN1_EFFECT
	 dw StatModifierDownEffect    ; EVASION_DOWN1_EFFECT
	 dw ConversionEffect          ; CONVERSION_EFFECT
	 dw HazeEffect                ; HAZE_EFFECT
	 dw BideEffect                ; BIDE_EFFECT
	 dw ThrashPetalDanceEffect    ; THRASH_PETAL_DANCE_EFFECT
	 dw SwitchAndTeleportEffect   ; SWITCH_AND_TELEPORT_EFFECT
	 dw TwoToFiveAttacksEffect    ; TWO_TO_FIVE_ATTACKS_EFFECT
	 dw TwoToFiveAttacksEffect    ; unused effect
	 dw FlinchSideEffect           ; FLINCH_SIDE_EFFECT1
	 dw SleepEffect               ; SLEEP_EFFECT
	 dw PoisonEffect              ; POISON_SIDE_EFFECT2
	 dw FreezeBurnParalyzeEffect  ; BURN_SIDE_EFFECT2
	 dw FreezeBurnParalyzeEffect  ; unused effect
	 dw FreezeBurnParalyzeEffect  ; PARALYZE_SIDE_EFFECT2
	 dw FlinchSideEffect           ; FLINCH_SIDE_EFFECT2
	 dw OneHitKOEffect            ; OHKO_EFFECT
	 dw ChargeEffect              ; CHARGE_EFFECT
	 dw $0000                     ; SUPER_FANG_EFFECT
	 dw $0000                     ; SPECIAL_DAMAGE_EFFECT
	 dw TrappingEffect            ; TRAPPING_EFFECT
	 dw ChargeEffect              ; FLY_EFFECT
	 dw TwoToFiveAttacksEffect    ; ATTACK_TWICE_EFFECT
	 dw $0000                     ; JUMP_KICK_EFFECT
	 dw MistEffect                ; MIST_EFFECT
	 dw FocusEnergyEffect         ; FOCUS_ENERGY_EFFECT
	 dw RecoilEffect              ; RECOIL_EFFECT
	 dw ConfusionEffect           ; CONFUSION_EFFECT
	 dw StatModifierUpEffect      ; ATTACK_UP2_EFFECT
	 dw StatModifierUpEffect      ; DEFENSE_UP2_EFFECT
	 dw StatModifierUpEffect      ; SPEED_UP2_EFFECT
	 dw StatModifierUpEffect      ; SPECIAL_UP2_EFFECT
	 dw StatModifierUpEffect      ; ACCURACY_UP2_EFFECT
	 dw StatModifierUpEffect      ; EVASION_UP2_EFFECT
	 dw HealEffect                ; HEAL_EFFECT
	 dw TransformEffect           ; TRANSFORM_EFFECT
	 dw StatModifierDownEffect    ; ATTACK_DOWN2_EFFECT
	 dw StatModifierDownEffect    ; DEFENSE_DOWN2_EFFECT
	 dw StatModifierDownEffect    ; SPEED_DOWN2_EFFECT
	 dw StatModifierDownEffect    ; SPECIAL_DOWN2_EFFECT
	 dw StatModifierDownEffect    ; ACCURACY_DOWN2_EFFECT
	 dw StatModifierDownEffect    ; EVASION_DOWN2_EFFECT
	 dw ReflectLightScreenEffect  ; LIGHT_SCREEN_EFFECT
	 dw ReflectLightScreenEffect  ; REFLECT_EFFECT
	 dw PoisonEffect              ; POISON_EFFECT
	 dw ParalyzeEffect            ; PARALYZE_EFFECT
	 dw StatModifierDownEffect    ; ATTACK_DOWN_SIDE_EFFECT
	 dw StatModifierDownEffect    ; DEFENSE_DOWN_SIDE_EFFECT
	 dw StatModifierDownEffect    ; SPEED_DOWN_SIDE_EFFECT
	 dw StatModifierDownEffect    ; SPECIAL_DOWN_SIDE_EFFECT
	 dw StatModifierDownEffect    ; unused effect
	 dw StatModifierDownEffect    ; unused effect
	 dw StatModifierDownEffect    ; unused effect
	 dw StatModifierDownEffect    ; unused effect
	 dw ConfusionSideEffect       ; CONFUSION_SIDE_EFFECT
	 dw TwoToFiveAttacksEffect    ; TWINEEDLE_EFFECT
	 dw $0000                     ; unused effect
	 dw SubstituteEffect          ; SUBSTITUTE_EFFECT
	 dw HyperBeamEffect           ; HYPER_BEAM_EFFECT
	 dw RageEffect                ; RAGE_EFFECT
	 dw MimicEffect               ; MIMIC_EFFECT
	 dw $0000                     ; METRONOME_EFFECT
	 dw LeechSeedEffect           ; LEECH_SEED_EFFECT
	 dw SplashEffect              ; SPLASH_EFFECT
	 dw DisableEffect             ; DISABLE_EFFECT

SleepEffect: ; 3f1fc (f:71fc)
	ld de, wEnemyMonStatus
	ld bc, W_ENEMYBATTSTATUS2
	ld a, [H_WHOSETURN]
	and a
	jp z, .sleepEffect
	ld de, wBattleMonStatus
	ld bc, W_PLAYERBATTSTATUS2

.sleepEffect
	ld a, [bc]
	bit NeedsToRecharge, a ; does the target need to recharge? (hyper beam)
	res NeedsToRecharge, a ; target no longer needs to recharge
	ld [bc], a
	jr nz, .setSleepCounter ; if the target had to recharge, all hit tests will be skipped
	                        ; including the event where the target already has another status
	ld a, [de]
	ld b, a
	and $7
	jr z, .notAlreadySleeping ; can't affect a mon that is already asleep
	ld hl, AlreadyAsleepText
	jp PrintText
.notAlreadySleeping
	ld a, b
	and a
	jr nz, .didntAffect ; can't affect a mon that is already statused
	push de
	call MoveHitTest ; apply accuracy tests
	pop de
	ld a, [W_MOVEMISSED]
	and a
	jr nz, .didntAffect
.setSleepCounter
; set target's sleep counter to a random number between 1 and 7
	call BattleRandom
	and $7
	jr z, .setSleepCounter
	ld [de], a
	call PlayCurrentMoveAnimation2
	ld hl, FellAsleepText
	jp PrintText
.didntAffect
	jp PrintDidntAffectText

FellAsleepText: ; 3f245 (f:7245)
	TX_FAR _FellAsleepText
	db "@"

AlreadyAsleepText: ; 3f24a (f:724a)
	TX_FAR _AlreadyAsleepText
	db "@"

PoisonEffect: ; 3f24f (f:724f)
	ld hl, wEnemyMonStatus
	ld de, W_PLAYERMOVEEFFECT
	ld a, [H_WHOSETURN]
	and a
	jr z, .poisonEffect
	ld hl, wBattleMonStatus
	ld de, W_ENEMYMOVEEFFECT
.poisonEffect
	call CheckTargetSubstitute
	jr nz, .noEffect ; can't posion a substitute target
	ld a, [hli]
	ld b, a
	and a
	jr nz, .noEffect ; miss if target is already statused
	ld a, [hli]
	cp POISON ; can't posion a poison-type target
	jr z, .noEffect
	ld a, [hld]
	cp POISON ; can't posion a poison-type target
	jr z, .noEffect
	ld a, [de]
	cp POISON_SIDE_EFFECT1
	ld b, $34 ; ~20% chance of poisoning
	jr z, .sideEffectTest
	cp POISON_SIDE_EFFECT2
	ld b, $67 ; ~40% chance of poisoning
	jr z, .sideEffectTest
	push hl
	push de
	call MoveHitTest ; apply accuracy tests
	pop de
	pop hl
	ld a, [W_MOVEMISSED]
	and a
	jr nz, .didntAffect
	jr .inflictPoison
.sideEffectTest
	call BattleRandom
	cp b ; was side effect successful?
	ret nc
.inflictPoison
	dec hl
	set 3, [hl] ; mon is now poisoned
	push de
	dec de
	ld a, [H_WHOSETURN]
	and a
	ld b, ANIM_C7
	ld hl, W_PLAYERBATTSTATUS3
	ld a, [de]
	ld de, W_PLAYERTOXICCOUNTER
	jr nz, .ok
	ld b, ANIM_A9
	ld hl, W_ENEMYBATTSTATUS3
	ld de, W_ENEMYTOXICCOUNTER
.ok
	cp TOXIC
	jr nz, .normalPoison ; done if move is not Toxic
	set BadlyPoisoned, [hl] ; else set Toxic battstatus
	xor a
	ld [de], a
	ld hl, BadlyPoisonedText
	jr .asm_3f2c0
.normalPoison
	ld hl, PoisonedText
.asm_3f2c0
	pop de
	ld a, [de]
	cp POISON_EFFECT
	jr z, .asm_3f2cd
	ld a, b
	call PlayBattleAnimation2
	jp PrintText
.asm_3f2cd
	call PlayCurrentMoveAnimation2
	jp PrintText
.noEffect
	ld a, [de]
	cp POISON_EFFECT
	ret nz
.didntAffect
	ld c, $32
	call DelayFrames
	jp PrintDidntAffectText

PoisonedText: ; 3f2df (f:72df)
	TX_FAR _PoisonedText
	db "@"

BadlyPoisonedText: ; 3f2e4 (f:72e4)
	TX_FAR _BadlyPoisonedText
	db "@"

DrainHPEffect: ; 3f2e9 (f:72e9)
	ld hl, DrainHPEffect_
	ld b, BANK(DrainHPEffect_)
	jp Bankswitch

ExplodeEffect: ; 3f2f1 (f:72f1)
	ld hl, wBattleMonHP
	ld de, W_PLAYERBATTSTATUS2
	ld a, [H_WHOSETURN]
	and a
	jr z, .faintUser
	ld hl, wEnemyMonHP
	ld de, W_ENEMYBATTSTATUS2
.faintUser
	xor a
	ld [hli], a ; set the mon's HP to 0
	ld [hli], a
	inc hl
	ld [hl], a ; set mon's status to 0
	ld a, [de]
	res Seeded, a ; clear mon's leech seed status
	ld [de], a
	ret

FreezeBurnParalyzeEffect: ; 3f30c (f:730c)
	xor a
	ld [wAnimationType], a
	call CheckTargetSubstitute ; test bit 4 of d063/d068 flags [target has substitute flag]
	ret nz ; return if they have a substitute, can't effect them
	ld a, [H_WHOSETURN]
	and a
	jp nz, opponentAttacker
	ld a, [wEnemyMonStatus]
	and a
	jp nz, CheckDefrost ; can't inflict status if opponent is already statused
	ld a, [W_PLAYERMOVETYPE]
	ld b, a
	ld a, [wEnemyMonType1]
	cp b ; do target type 1 and move type match?
	ret z  ; return if they match (an ice move can't freeze an ice-type, body slam can't paralyze a normal-type, etc.)
	ld a, [wEnemyMonType2]
	cp b ; do target type 2 and move type match?
	ret z  ; return if they match
	ld a, [W_PLAYERMOVEEFFECT]
	cp a, PARALYZE_SIDE_EFFECT1 + 1 ; 10% status effects are 04, 05, 06 so 07 will set carry for those
	ld b, $1a ; 0x1A/0x100 or 26/256 = 10.2%~ chance
	jr c, .next1 ; branch ahead if this is a 10% chance effect..
	ld b, $4d ; else use 0x4D/0x100 or 77/256 = 30.1%~ chance
	sub a, $1e ; subtract $1E to map to equivalent 10% chance effects
.next1
	push af
	call BattleRandom ; get random 8bit value for probability test
	cp b
	pop bc
	ret nc ; do nothing if random value is >= 1A or 4D [no status applied]
	ld a, b ; what type of effect is this?
	cp a, BURN_SIDE_EFFECT1
	jr z, .burn
	cp a, FREEZE_SIDE_EFFECT
	jr z, .freeze
; .paralyze
	ld a, 1 << PAR
	ld [wEnemyMonStatus], a
	call QuarterSpeedDueToParalysis ; quarter speed of affected mon
	ld a, ANIM_A9
	call PlayBattleAnimation
	jp PrintMayNotAttackText ; print paralysis text
.burn
	ld a, 1 << BRN
	ld [wEnemyMonStatus], a
	call HalveAttackDueToBurn ; halve attack of affected mon
	ld a, ANIM_A9
	call PlayBattleAnimation
	ld hl, BurnedText
	jp PrintText
.freeze
	call ClearHyperBeam ; resets hyper beam (recharge) condition from target
	ld a, 1 << FRZ
	ld [wEnemyMonStatus], a
	ld a, ANIM_A9
	call PlayBattleAnimation
	ld hl, FrozenText
	jp PrintText
opponentAttacker: ; 3f382 (f:7382)
	ld a, [wBattleMonStatus] ; mostly same as above with addresses swapped for opponent
	and a
	jp nz, CheckDefrost
	ld a, [W_ENEMYMOVETYPE]
	ld b, a
	ld a, [wBattleMonType1]
	cp b
	ret z
	ld a, [wBattleMonType2]
	cp b
	ret z
	ld a, [W_ENEMYMOVEEFFECT]
	cp a, PARALYZE_SIDE_EFFECT1 + 1
	ld b, $1a
	jr c, .next1
	ld b, $4d
	sub a, $1e
.next1
	push af
	call BattleRandom
	cp b
	pop bc
	ret nc
	ld a, b
	cp a, BURN_SIDE_EFFECT1
	jr z, .burn
	cp a, FREEZE_SIDE_EFFECT
	jr z, .freeze
	ld a, 1 << PAR
	ld [wBattleMonStatus], a
	call QuarterSpeedDueToParalysis
	jp PrintMayNotAttackText
.burn
	ld a, 1 << BRN
	ld [wBattleMonStatus], a
	call HalveAttackDueToBurn
	ld hl, BurnedText
	jp PrintText
.freeze
; hyper beam bits aren't reseted for opponent's side
	ld a, 1 << FRZ
	ld [wBattleMonStatus], a
	ld hl, FrozenText
	jp PrintText

BurnedText: ; 3f3d8 (f:73d8)
	TX_FAR _BurnedText
	db "@"

FrozenText: ; 3f3dd (f:73dd)
	TX_FAR _FrozenText
	db "@"

CheckDefrost: ; 3f3e2 (f:73e2)
; any fire-type move that has a chance inflict burn (all but Fire Spin) will defrost a frozen target
	and a, 1 << FRZ	; are they frozen?
	ret z ; return if so
	ld a, [H_WHOSETURN]
	and a
	jr nz, .opponent
	;player [attacker]
	ld a, [W_PLAYERMOVETYPE]
	sub a, FIRE
	ret nz ; return if type of move used isn't fire
	ld [wEnemyMonStatus], a	; set opponent status to 00 ["defrost" a frozen monster]
	ld hl, wEnemyMon1Status
	ld a, [wEnemyMonPartyPos]
	ld bc, wEnemyMon2 - wEnemyMon1
	call AddNTimes
	xor a
	ld [hl], a ; clear status in roster
	ld hl, FireDefrostedText
	jr .common
.opponent
	ld a, [W_ENEMYMOVETYPE]	; same as above with addresses swapped
	sub a, FIRE
	ret nz
	ld [wBattleMonStatus], a
	ld hl, wPartyMon1Status
	ld a, [wPlayerMonNumber]
	ld bc, wPartyMon2 - wPartyMon1
	call AddNTimes
	xor a
	ld [hl], a
	ld hl, FireDefrostedText
.common
	jp PrintText

FireDefrostedText: ; 3f423 (f:7423)
	TX_FAR _FireDefrostedText
	db "@"

StatModifierUpEffect: ; 3f428 (f:7428)
	ld hl, wPlayerMonStatMods
	ld de, W_PLAYERMOVEEFFECT
	ld a, [H_WHOSETURN]
	and a
	jr z, .statModifierUpEffect
	ld hl, wEnemyMonStatMods
	ld de, W_ENEMYMOVEEFFECT
.statModifierUpEffect
	ld a, [de]
	sub ATTACK_UP1_EFFECT
	cp EVASION_UP1_EFFECT + $3 - ATTACK_UP1_EFFECT ; covers all +1 effects
	jr c, .incrementStatMod
	sub ATTACK_UP2_EFFECT - ATTACK_UP1_EFFECT ; map +2 effects to equivalent +1 effect
.incrementStatMod
	ld c, a
	ld b, $0
	add hl, bc
	ld b, [hl]
	inc b ; increment corresponding stat mod
	ld a, $d
	cp b ; can't raise stat past +6 ($d or 13)
	jp c, PrintNothingHappenedText
	ld a, [de]
	cp ATTACK_UP1_EFFECT + $8 ; is it a +2 effect?
	jr c, .ok
	inc b ; if so, increment stat mod again
	ld a, $d
	cp b ; unless it's already +6
	jr nc, .ok
	ld b, a
.ok
	ld [hl], b
	ld a, c
	cp $4
	jr nc, UpdateStatDone ; jump if mod affected is evasion/accuracy
	push hl
	ld hl, wBattleMonAttack + 1
	ld de, wPlayerMonUnmodifiedAttack
	ld a, [H_WHOSETURN]
	and a
	jr z, .pointToStats
	ld hl, wEnemyMonAttack + 1
	ld de, wEnemyMonUnmodifiedAttack
.pointToStats
	push bc
	sla c
	ld b, $0
	add hl, bc ; hl = modified stat
	ld a, c
	add e
	ld e, a
	jr nc, .checkIf999
	inc d ; de = unmodified (original) stat
.checkIf999
	pop bc
	ld a, [hld]
	sub 999 % $100 ; check if stat is already 999
	jr nz, .recalculateStat
	ld a, [hl]
	sbc 999 / $100
	jp z, RestoreOriginalStatModifier
.recalculateStat ; recalculate affected stat
                 ; paralysis and burn penalties, as well as badge boosts are ignored
	push hl
	push bc
	ld hl, StatModifierRatios
	dec b
	sla b
	ld c, b
	ld b, $0
	add hl, bc
	pop bc
	xor a
	ld [H_MULTIPLICAND], a 
	ld a, [de]
	ld [H_MULTIPLICAND + 1], a
	inc de
	ld a, [de]
	ld [H_MULTIPLICAND + 2], a
	ld a, [hli]
	ld [H_MULTIPLIER], a 
	call Multiply
	ld a, [hl]
	ld [H_DIVISOR], a 
	ld b, $4
	call Divide
	pop hl
; cap at 999
	ld a, [H_PRODUCT + 3]
	sub 999 % $100
	ld a, [H_PRODUCT + 2]
	sbc 999 / $100
	jp c, UpdateStat
	ld a, 999 / $100
	ld [H_MULTIPLICAND + 1], a
	ld a, 999 % $100
	ld [H_MULTIPLICAND + 2], a

UpdateStat: ; 3f4c3 (f:74c3)
	ld a, [H_PRODUCT + 2]
	ld [hli], a
	ld a, [H_PRODUCT + 3]
	ld [hl], a
	pop hl
UpdateStatDone: ; 3f4ca (f:74ca)
	ld b, c
	inc b
	call Func_3f688
	ld hl, W_PLAYERBATTSTATUS2
	ld de, W_PLAYERMOVENUM
	ld bc, wPlayerHasUsedMinimize
	ld a, [H_WHOSETURN]
	and a
	jr z, .asm_3f4e6
	ld hl, W_ENEMYBATTSTATUS2
	ld de, W_ENEMYMOVENUM
	ld bc, wEnemyHasUsedMinimize
.asm_3f4e6
	ld a, [de]
	cp MINIMIZE
	jr nz, .asm_3f4f9
	bit HasSubstituteUp, [hl] ; substitute
	push af
	push bc
	ld hl, Func_79747
	ld b, BANK(Func_79747)
	push de
	call nz, Bankswitch ; play Minimize animation unless there's Substitute involved
	pop de
.asm_3f4f9
	call PlayCurrentMoveAnimation
	ld a, [de]
	cp MINIMIZE
	jr nz, .applyBadgeBoostsAndStatusPenalties
	pop bc
	ld a, $1
	ld [bc], a
	ld hl, Func_79771
	ld b, BANK(Func_79771)
	pop af
	call nz, Bankswitch
.applyBadgeBoostsAndStatusPenalties
	ld a, [H_WHOSETURN]
	and a
	call z, ApplyBadgeStatBoosts ; whenever the player uses a stat-up move, badge boosts get reapplied again to every stat,
	                             ; even to those not affected by the stat-up move (will be boosted further)
	ld hl, MonsStatsRoseText
	call PrintText

; these shouldn't be here
	call QuarterSpeedDueToParalysis ; apply speed penalty to the player whose turn is not, if it's paralyzed
	jp HalveAttackDueToBurn ; apply attack penalty to the player whose turn is not, if it's burned

RestoreOriginalStatModifier: ; 3f520 (f:7520)
	pop hl
	dec [hl]

PrintNothingHappenedText: ; 3f522 (f:7522)
	ld hl, NothingHappenedText
	jp PrintText

MonsStatsRoseText: ; 3f528 (f:7528)
	TX_FAR _MonsStatsRoseText
	db $08 ; asm
	ld hl, GreatlyRoseText
	ld a, [H_WHOSETURN]
	and a
	ld a, [W_PLAYERMOVEEFFECT]
	jr z, .asm_3f53b
	ld a, [W_ENEMYMOVEEFFECT]
.asm_3f53b
	cp ATTACK_DOWN1_EFFECT
	ret nc
	ld hl, RoseText
	ret

GreatlyRoseText: ; 3f542 (f:7542)
	db $0a
	TX_FAR _GreatlyRoseText

RoseText: ; 3f547 (f:7547)
	TX_FAR _RoseText
	db "@"

StatModifierDownEffect: ; 3f54c (f:754c)
	ld hl, wEnemyMonStatMods
	ld de, W_PLAYERMOVEEFFECT
	ld bc, W_ENEMYBATTSTATUS1
	ld a, [H_WHOSETURN]
	and a
	jr z, .statModifierDownEffect
	ld hl, wPlayerMonStatMods
	ld de, W_ENEMYMOVEEFFECT
	ld bc, W_PLAYERBATTSTATUS1
	ld a, [wLinkState]
	cp LINK_STATE_BATTLING
	jr z, .statModifierDownEffect
	call BattleRandom
	cp $40 ; 1/4 chance to miss by in regular battle
	jp c, MoveMissed
.statModifierDownEffect
	call CheckTargetSubstitute ; can't hit through substitute
	jp nz, MoveMissed
	ld a, [de]
	cp ATTACK_DOWN_SIDE_EFFECT
	jr c, .nonSideEffect
	call BattleRandom
	cp $55 ; 85/256 chance for side effects
	jp nc, CantLowerAnymore
	ld a, [de]
	sub ATTACK_DOWN_SIDE_EFFECT ; map each stat to 0-3
	jr .decrementStatMod
.nonSideEffect ; non-side effects only
	push hl
	push de
	push bc
	call MoveHitTest ; apply accuracy tests
	pop bc
	pop de
	pop hl
	ld a, [W_MOVEMISSED]
	and a
	jp nz, MoveMissed
	ld a, [bc]
	bit Invulnerable, a ; fly/dig
	jp nz, MoveMissed
	ld a, [de]
	sub ATTACK_DOWN1_EFFECT
	cp EVASION_DOWN1_EFFECT + $3 - ATTACK_DOWN1_EFFECT ; covers al -1 effects
	jr c, .decrementStatMod
	sub ATTACK_DOWN2_EFFECT - ATTACK_DOWN1_EFFECT ; map -2 effects to corresponding -1 effect
.decrementStatMod
	ld c, a
	ld b, $0
	add hl, bc
	ld b, [hl]
	dec b ; dec corresponding stat mod
	jp z, CantLowerAnymore ; if stat mod is 1 (-6), can't lower anymore
	ld a, [de]
	cp ATTACK_DOWN2_EFFECT - $16 ; $24
	jr c, .ok
	cp EVASION_DOWN2_EFFECT + $5 ; $44
	jr nc, .ok
	dec b ; stat down 2 effects only (dec mod again)
	jr nz, .ok
	inc b ; increment mod to 1 (-6) if it would become 0 (-7)
.ok
	ld [hl], b ; save modified mod
	ld a, c
	cp $4
	jr nc, UpdateLoweredStatDone ; jump for evasion/accuracy
	push hl
	push de
	ld hl, wEnemyMonAttack + 1
	ld de, wEnemyMonUnmodifiedAttack
	ld a, [H_WHOSETURN]
	and a
	jr z, .pointToStat
	ld hl, wBattleMonAttack + 1
	ld de, wPlayerMonUnmodifiedAttack
.pointToStat
	push bc
	sla c
	ld b, $0
	add hl, bc ; hl = modified stat
	ld a, c
	add e
	ld e, a
	jr nc, .asm_3f5e4
	inc d ; de = unmodified stat
.asm_3f5e4
	pop bc
	ld a, [hld]
	sub $1 ; can't lower stat below 1 (-6)
	jr nz, .recalculateStat
	ld a, [hl]
	and a
	jp z, Func_3f64d
.recalculateStat
; recalculate affected stat
; paralysis and burn penalties, as well as badge boosts are ignored
	push hl
	push bc
	ld hl, StatModifierRatios
	dec b
	sla b
	ld c, b
	ld b, $0
	add hl, bc
	pop bc
	xor a
	ld [H_MULTIPLICAND], a 
	ld a, [de]
	ld [H_MULTIPLICAND + 1], a
	inc de
	ld a, [de]
	ld [H_MULTIPLICAND + 2], a
	ld a, [hli]
	ld [H_MULTIPLIER], a 
	call Multiply
	ld a, [hl]
	ld [H_DIVISOR], a 
	ld b, $4
	call Divide
	pop hl
	ld a, [H_PRODUCT + 3]
	ld b, a
	ld a, [H_PRODUCT + 2]
	or b
	jp nz, UpdateLoweredStat
	ld [H_MULTIPLICAND + 1], a
	ld a, $1
	ld [H_MULTIPLICAND + 2], a

UpdateLoweredStat: ; 3f624 (f:7624)
	ld a, [H_PRODUCT + 2]
	ld [hli], a
	ld a, [H_PRODUCT + 3]
	ld [hl], a
	pop de
	pop hl
UpdateLoweredStatDone: ; 3f62c (f:762c)
	ld b, c
	inc b
	push de
	call Func_3f688
	pop de
	ld a, [de]
	cp $44
	jr nc, .ApplyBadgeBoostsAndStatusPenalties
	call PlayCurrentMoveAnimation2
.ApplyBadgeBoostsAndStatusPenalties
	ld a, [H_WHOSETURN]
	and a
	call nz, ApplyBadgeStatBoosts ; whenever the player uses a stat-down move, badge boosts get reapplied again to every stat,
	                              ; even to those not affected by the stat-up move (will be boosted further)
	ld hl, MonsStatsFellText
	call PrintText

; These where probably added given that a stat-down move affecting speed or attack will override
; the stat penalties from paralysis and burn respectively.
; But they are always called regardless of the stat affected by the stat-down move.
	call QuarterSpeedDueToParalysis
	jp HalveAttackDueToBurn

Func_3f64d: ; 3f64d (f:764d)
	pop de
	pop hl
	inc [hl]

CantLowerAnymore: ; 3f650 (f:7650)
	ld a, [de]
	cp ATTACK_DOWN_SIDE_EFFECT
	ret nc
	ld hl, NothingHappenedText
	jp PrintText

MoveMissed: ; 3f65a (f:765a)
	ld a, [de]
	cp $44
	ret nc
	jp ConditionalPrintButItFailed

MonsStatsFellText: ; 3f661 (f:7661)
	TX_FAR _MonsStatsFellText
	db $08 ; asm
	ld hl, FellText
	ld a, [H_WHOSETURN]
	and a
	ld a, [W_PLAYERMOVEEFFECT]
	jr z, .asm_3f674
	ld a, [W_ENEMYMOVEEFFECT]
.asm_3f674
	cp $1a
	ret c
	cp $44
	ret nc
	ld hl, GreatlyFellText
	ret

GreatlyFellText: ; 3f67e (f:767e)
	db $0a
	TX_FAR _GreatlyFellText

FellText: ; 3f683 (f:7683)
	TX_FAR _FellText
	db "@"

Func_3f688: ; 3f688 (f:7688)
	ld hl, StatsTextStrings
	ld c, $50
.asm_3f68d
	dec b
	jr z, .asm_3f696
.asm_3f690
	ld a, [hli]
	cp c
	jr z, .asm_3f68d
	jr .asm_3f690
.asm_3f696
	ld de, wcf4b
	ld bc, $a
	jp CopyData

StatsTextStrings: ; 3f69f (f:769f)
	db "ATTACK@"
	db "DEFENSE@"
	db "SPEED@"
	db "SPECIAL@"
	db "ACCURACY@"
	db "EVADE@"

StatModifierRatios: ; 3f6cb (f:76cb)
; first byte is numerator, second byte is denominator
	db 25, 100  ; 0.25
	db 28, 100  ; 0.28
	db 33, 100  ; 0.33
	db 40, 100  ; 0.40
	db 50, 100  ; 0.50
	db 66, 100  ; 0.66
	db  1,   1  ; 1.00
	db 15,  10  ; 1.50
	db  2,   1  ; 2.00
	db 25,  10  ; 2.50
	db  3,   1  ; 3.00
	db 35,  10  ; 3.50
	db  4,   1  ; 4.00

BideEffect: ; 3f6e5 (f:76e5)
	ld hl, W_PLAYERBATTSTATUS1
	ld de, wPlayerBideAccumulatedDamage
	ld bc, wPlayerNumAttacksLeft
	ld a, [H_WHOSETURN]
	and a
	jr z, .bideEffect
	ld hl, W_ENEMYBATTSTATUS1
	ld de, wEnemyBideAccumulatedDamage
	ld bc, wEnemyNumAttacksLeft
.bideEffect
	set StoringEnergy, [hl] ; mon is now using bide
	xor a
	ld [de], a
	inc de
	ld [de], a
	ld [W_PLAYERMOVEEFFECT], a
	ld [W_ENEMYMOVEEFFECT], a
	call BattleRandom
	and $1
	inc a
	inc a
	ld [bc], a ; set Bide counter to 2 or 3 at random
	ld a, [H_WHOSETURN]
	add XSTATITEM_ANIM
	jp PlayBattleAnimation2

ThrashPetalDanceEffect: ; 3f717 (f:7717)
	ld hl, W_PLAYERBATTSTATUS1
	ld de, wPlayerNumAttacksLeft
	ld a, [H_WHOSETURN]
	and a
	jr z, .thrashPetalDanceEffect
	ld hl, W_ENEMYBATTSTATUS1
	ld de, wEnemyNumAttacksLeft
.thrashPetalDanceEffect
	set ThrashingAbout, [hl] ; mon is now using thrash/petal dance
	call BattleRandom
	and $1
	inc a
	inc a
	ld [de], a ; set thrash/petal dance counter to 2 or 3 at random
	ld a, [H_WHOSETURN]
	add ANIM_B0
	jp PlayBattleAnimation2

SwitchAndTeleportEffect: ; 3f739 (f:7739)
	ld a, [H_WHOSETURN]
	and a
	jr nz, .asm_3f791
	ld a, [W_ISINBATTLE]
	dec a
	jr nz, .asm_3f77e
	ld a, [W_CURENEMYLVL]
	ld b, a
	ld a, [wBattleMonLevel]
	cp b
	jr nc, .asm_3f76e
	add b
	ld c, a
	inc c
.asm_3f751
	call BattleRandom
	cp c
	jr nc, .asm_3f751
	srl b
	srl b
	cp b
	jr nc, .asm_3f76e
	ld c, $32
	call DelayFrames
	ld a, [W_PLAYERMOVENUM]
	cp TELEPORT
	jp nz, PrintDidntAffectText
	jp PrintButItFailedText_
.asm_3f76e
	call ReadPlayerMonCurHPAndStatus
	xor a
	ld [wAnimationType], a
	inc a
	ld [wEscapedFromBattle], a
	ld a, [W_PLAYERMOVENUM]
	jr .asm_3f7e4
.asm_3f77e
	ld c, $32
	call DelayFrames
	ld hl, IsUnaffectedText
	ld a, [W_PLAYERMOVENUM]
	cp TELEPORT
	jp nz, PrintText
	jp PrintButItFailedText_
.asm_3f791
	ld a, [W_ISINBATTLE]
	dec a
	jr nz, .asm_3f7d1
	ld a, [wBattleMonLevel]
	ld b, a
	ld a, [W_CURENEMYLVL]
	cp b
	jr nc, .asm_3f7c1
	add b
	ld c, a
	inc c
.asm_3f7a4
	call BattleRandom
	cp c
	jr nc, .asm_3f7a4
	srl b
	srl b
	cp b
	jr nc, .asm_3f7c1
	ld c, $32
	call DelayFrames
	ld a, [W_ENEMYMOVENUM]
	cp TELEPORT
	jp nz, PrintDidntAffectText
	jp PrintButItFailedText_
.asm_3f7c1
	call ReadPlayerMonCurHPAndStatus
	xor a
	ld [wAnimationType], a
	inc a
	ld [wEscapedFromBattle], a
	ld a, [W_ENEMYMOVENUM]
	jr .asm_3f7e4
.asm_3f7d1
	ld c, $32
	call DelayFrames
	ld hl, IsUnaffectedText
	ld a, [W_ENEMYMOVENUM]
	cp TELEPORT
	jp nz, PrintText
	jp ConditionalPrintButItFailed
.asm_3f7e4
	push af
	call PlayBattleAnimation
	ld c, $14
	call DelayFrames
	pop af
	ld hl, RanFromBattleText
	cp TELEPORT
	jr z, .asm_3f7ff
	ld hl, RanAwayScaredText
	cp ROAR
	jr z, .asm_3f7ff
	ld hl, WasBlownAwayText
.asm_3f7ff
	jp PrintText

RanFromBattleText: ; 3f802 (f:7802)
	TX_FAR _RanFromBattleText
	db "@"

RanAwayScaredText: ; 3f807 (f:7807)
	TX_FAR _RanAwayScaredText
	db "@"

WasBlownAwayText: ; 3f80c (f:780c)
	TX_FAR _WasBlownAwayText
	db "@"

TwoToFiveAttacksEffect: ; 3f811 (f:7811)
	ld hl, W_PLAYERBATTSTATUS1
	ld de, wPlayerNumAttacksLeft
	ld bc, wPlayerNumHits
	ld a, [H_WHOSETURN]
	and a
	jr z, .twoToFiveAttacksEffect
	ld hl, W_ENEMYBATTSTATUS1
	ld de, wEnemyNumAttacksLeft
	ld bc, wEnemyNumHits
.twoToFiveAttacksEffect
	bit AttackingMultipleTimes, [hl] ; is mon attacking multiple times?
	ret nz
	set AttackingMultipleTimes, [hl] ; mon is now attacking multiple times
	ld hl, W_PLAYERMOVEEFFECT
	ld a, [H_WHOSETURN]
	and a
	jr z, .setNumberOfHits
	ld hl, W_ENEMYMOVEEFFECT
.setNumberOfHits
	ld a, [hl]
	cp TWINEEDLE_EFFECT
	jr z, .twineedle
	cp ATTACK_TWICE_EFFECT
	ld a, $2 ; number of hits it's always 2 for ATTACK_TWICE_EFFECT
	jr z, .saveNumberOfHits
; for TWO_TO_FIVE_ATTACKS_EFFECT 3/8 chance for 2 and 3 hits, and 1/8 chance for 4 and 5 hits
	call BattleRandom
	and $3
	cp $2
	jr c, .asm_3f851
	call BattleRandom
	and $3
.asm_3f851
	inc a
	inc a
.saveNumberOfHits
	ld [de], a
	ld [bc], a
	ret
.twineedle
	ld a, POISON_SIDE_EFFECT1
	ld [hl], a ; set Twineedle's effect to poison effect
	jr .saveNumberOfHits

FlinchSideEffect: ; 3f85b (f:785b)
	call CheckTargetSubstitute
	ret nz
	ld hl, W_ENEMYBATTSTATUS1
	ld de, W_PLAYERMOVEEFFECT
	ld a, [H_WHOSETURN]
	and a
	jr z, .flinchSideEffect
	ld hl, W_PLAYERBATTSTATUS1
	ld de, W_ENEMYMOVEEFFECT
.flinchSideEffect
	ld a, [de]
	cp FLINCH_SIDE_EFFECT1
	ld b, $1a ; ~10% chance of flinch
	jr z, .gotEffectChance
	ld b, $4d ; ~30% chance of flinch
.gotEffectChance
	call BattleRandom
	cp b
	ret nc
	set Flinched, [hl] ; set mon's status to flinching
	call ClearHyperBeam
	ret

OneHitKOEffect: ; 3f884 (f:7884)
	ld hl, OneHitKOEffect_
	ld b, BANK(OneHitKOEffect_)
	jp Bankswitch

ChargeEffect: ; 3f88c (f:788c)
	ld hl, W_PLAYERBATTSTATUS1
	ld de, W_PLAYERMOVEEFFECT
	ld a, [H_WHOSETURN]
	and a
	ld b, XSTATITEM_ANIM
	jr z, .chargeEffect
	ld hl, W_ENEMYBATTSTATUS1
	ld de, W_ENEMYMOVEEFFECT
	ld b, ANIM_AF
.chargeEffect
	set ChargingUp, [hl]
	ld a, [de]
	dec de ; de contains enemy or player MOVENUM
	cp FLY_EFFECT
	jr nz, .notFly
	set Invulnerable, [hl] ; mon is now invulnerable to typical attacks (fly/dig)
	ld b, TELEPORT ; load Teleport's animation
.notFly
	ld a, [de]
	cp DIG
	jr nz, .notDigOrFly
	set Invulnerable, [hl] ; mon is now invulnerable to typical attacks (fly/dig)
	ld b, ANIM_C0
.notDigOrFly
	xor a
	ld [wAnimationType], a
	ld a, b
	call PlayBattleAnimation
	ld a, [de]
	ld [wWhichTrade], a
	ld hl, ChargeMoveEffectText
	jp PrintText

ChargeMoveEffectText: ; 3f8c8 (f:78c8)
	TX_FAR _ChargeMoveEffectText
	db $08 ; asm
	ld a, [wWhichTrade]
	cp RAZOR_WIND
	ld hl, MadeWhirlwindText
	jr z, .asm_3f8f8
	cp SOLARBEAM
	ld hl, TookInSunlightText
	jr z, .asm_3f8f8
	cp SKULL_BASH
	ld hl, LoweredItsHeadText
	jr z, .asm_3f8f8
	cp SKY_ATTACK
	ld hl, SkyAttackGlowingText
	jr z, .asm_3f8f8
	cp FLY
	ld hl, FlewUpHighText
	jr z, .asm_3f8f8
	cp DIG
	ld hl, DugAHoleText
.asm_3f8f8
	ret

MadeWhirlwindText: ; 3f8f9 (f:78f9)
	TX_FAR _MadeWhirlwindText
	db "@"

TookInSunlightText: ; 3f8fe (f:78fe)
	TX_FAR _TookInSunlightText
	db "@"

LoweredItsHeadText: ; 3f903 (f:7903)
	TX_FAR _LoweredItsHeadText
	db "@"

SkyAttackGlowingText: ; 3f908 (f:7908)
	TX_FAR _SkyAttackGlowingText
	db "@"

FlewUpHighText: ; 3f90d (f:790d)
	TX_FAR _FlewUpHighText
	db "@"

DugAHoleText: ; 3f912 (f:7912)
	TX_FAR _DugAHoleText
	db "@"

TrappingEffect: ; 3f917 (f:7917)
	ld hl, W_PLAYERBATTSTATUS1
	ld de, wPlayerNumAttacksLeft
	ld a, [H_WHOSETURN]
	and a
	jr z, .trappingEffect
	ld hl, W_ENEMYBATTSTATUS1
	ld de, wEnemyNumAttacksLeft
.trappingEffect
	bit UsingTrappingMove, [hl]
	ret nz
	call ClearHyperBeam ; since this effect is called before testing whether the move will hit, 
                        ; the target won't need to recharge even if the trapping move missed 	
	set UsingTrappingMove, [hl] ; mon is now using a trapping move
	call BattleRandom ; 3/8 chance for 2 and 3 attacks, and 1/8 chance for 4 and 5 attacks
	and $3
	cp $2
	jr c, .setTrappingCounter
	call BattleRandom
	and $3
.setTrappingCounter
	inc a
	ld [de], a
	ret

MistEffect: ; 3f941 (f:7941)
	ld hl, MistEffect_
	ld b, BANK(MistEffect_)
	jp Bankswitch

FocusEnergyEffect: ; 3f949 (f:7949)
	ld hl, FocusEnergyEffect_
	ld b, BANK(FocusEnergyEffect_)
	jp Bankswitch

RecoilEffect: ; 3f951 (f:7951)
	ld hl, RecoilEffect_
	ld b, BANK(RecoilEffect_)
	jp Bankswitch

ConfusionSideEffect: ; 3f959 (f:7959)
	call BattleRandom
	cp $19
	ret nc
	jr ConfusionSideEffectSuccess

ConfusionEffect: ; 3f961 (f:7961)
	call CheckTargetSubstitute
	jr nz, ConfusionEffectFailed
	call MoveHitTest
	ld a, [W_MOVEMISSED]
	and a
	jr nz, ConfusionEffectFailed

ConfusionSideEffectSuccess: ; 3f96f (f:796f)
	ld a, [H_WHOSETURN]
	and a
	ld hl, W_ENEMYBATTSTATUS1
	ld bc, W_ENEMYCONFUSEDCOUNTER
	ld a, [W_PLAYERMOVEEFFECT]
	jr z, .confuseTarget
	ld hl, W_PLAYERBATTSTATUS1
	ld bc, W_PLAYERCONFUSEDCOUNTER
	ld a, [W_ENEMYMOVEEFFECT]
.confuseTarget
	bit Confused, [hl] ; is mon confused?
	jr nz, ConfusionEffectFailed
	set Confused, [hl] ; mon is now confused
	push af
	call BattleRandom
	and $3 
	inc a
	inc a
	ld [bc], a ; confusion status will last 2-5 turns
	pop af
	cp CONFUSION_SIDE_EFFECT
	call nz, PlayCurrentMoveAnimation2
	ld hl, BecameConfusedText
	jp PrintText

BecameConfusedText: ; 3f9a1 (f:79a1)
	TX_FAR _BecameConfusedText
	db "@"

ConfusionEffectFailed: ; 3f9a6 (f:79a6)
	cp CONFUSION_SIDE_EFFECT
	ret z
	ld c, $32
	call DelayFrames
	jp ConditionalPrintButItFailed

ParalyzeEffect: ; 3f9b1 (f:79b1)
	ld hl, ParalyzeEffect_
	ld b, BANK(ParalyzeEffect_)
	jp Bankswitch

SubstituteEffect: ; 3f9b9 (f:79b9)
	ld hl, SubstituteEffect_
	ld b, BANK(SubstituteEffect_)
	jp Bankswitch

HyperBeamEffect: ; 3f9c1 (f:79c1)
	ld hl, W_PLAYERBATTSTATUS2
	ld a, [H_WHOSETURN]
	and a
	jr z, .hyperBeamEffect
	ld hl, W_ENEMYBATTSTATUS2
.hyperBeamEffect
	set NeedsToRecharge, [hl] ; mon now needs to recharge
	ret

ClearHyperBeam: ; 3f9cf (f:79cf)
	push hl
	ld hl, W_ENEMYBATTSTATUS2
	ld a, [H_WHOSETURN]
	and a
	jr z, .asm_3f9db
	ld hl, W_PLAYERBATTSTATUS2
.asm_3f9db
	res NeedsToRecharge, [hl] ; mon no longer needs to recharge
	pop hl
	ret

RageEffect: ; 3f9df (f:79df)
	ld hl, W_PLAYERBATTSTATUS2
	ld a, [H_WHOSETURN]
	and a
	jr z, .player
	ld hl, W_ENEMYBATTSTATUS2
.player
	set UsingRage, [hl] ; mon is now in "rage" mode
	ret

MimicEffect: ; 3f9ed (f:79ed)
	ld c, $32
	call DelayFrames
	call MoveHitTest
	ld a, [W_MOVEMISSED]
	and a
	jr nz, .asm_3fa74
	ld a, [H_WHOSETURN]
	and a
	ld hl, wBattleMonMoves
	ld a, [W_PLAYERBATTSTATUS1]
	jr nz, .asm_3fa13
	ld a, [wLinkState]
	cp LINK_STATE_BATTLING
	jr nz, .asm_3fa3a
	ld hl, wEnemyMonMoves
	ld a, [W_ENEMYBATTSTATUS1]
.asm_3fa13
	bit Invulnerable, a
	jr nz, .asm_3fa74
.asm_3fa17
	push hl
	call BattleRandom
	and $3
	ld c, a
	ld b, $0
	add hl, bc
	ld a, [hl]
	pop hl
	and a
	jr z, .asm_3fa17
	ld d, a
	ld a, [H_WHOSETURN]
	and a
	ld hl, wBattleMonMoves
	ld a, [wPlayerMoveListIndex]
	jr z, .asm_3fa5f
	ld hl, wEnemyMonMoves
	ld a, [wEnemyMoveListIndex]
	jr .asm_3fa5f
.asm_3fa3a
	ld a, [W_ENEMYBATTSTATUS1]
	bit Invulnerable, a
	jr nz, .asm_3fa74
	ld a, [wCurrentMenuItem]
	push af
	ld a, $1
	ld [wMoveMenuType], a
	call MoveSelectionMenu
	call LoadScreenTilesFromBuffer1
	ld hl, wEnemyMonMoves
	ld a, [wCurrentMenuItem]
	ld c, a
	ld b, $0
	add hl, bc
	ld d, [hl]
	pop af
	ld hl, wBattleMonMoves
.asm_3fa5f
	ld c, a
	ld b, $0
	add hl, bc
	ld a, d
	ld [hl], a
	ld [wd11e], a
	call GetMoveName
	call PlayCurrentMoveAnimation
	ld hl, MimicLearnedMoveText
	jp PrintText
.asm_3fa74
	jp PrintButItFailedText_

MimicLearnedMoveText: ; 3fa77 (f:7a77)
	TX_FAR _MimicLearnedMoveText
	db "@"

LeechSeedEffect: ; 3fa7c (f:7a7c)
	ld hl, LeechSeedEffect_
	ld b, BANK(LeechSeedEffect_)
	jp Bankswitch

SplashEffect: ; 3fa84 (f:7a84)
	call PlayCurrentMoveAnimation
	jp PrintNoEffectText

DisableEffect: ; 3fa8a (f:7a8a)
	call MoveHitTest
	ld a, [W_MOVEMISSED]
	and a
	jr nz, .moveMissed
	ld de, W_ENEMYDISABLEDMOVE
	ld hl, wEnemyMonMoves
	ld a, [H_WHOSETURN]
	and a
	jr z, .disableEffect
	ld de, W_PLAYERDISABLEDMOVE
	ld hl, wBattleMonMoves
.disableEffect
; no effect if target already has a move disabled
	ld a, [de]
	and a
	jr nz, .moveMissed
.pickMoveToDisable
	push hl
	call BattleRandom
	and $3
	ld c, a
	ld b, $0
	add hl, bc
	ld a, [hl]
	pop hl
	and a
	jr z, .pickMoveToDisable ; loop until a non-00 move slot is found
	ld [wd11e], a ; store move number
	push hl
	ld a, [H_WHOSETURN]
	and a
	ld hl, wBattleMonPP
	jr nz, .enemyTurn 
	ld a, [wLinkState]
	cp LINK_STATE_BATTLING
	pop hl ; wEnemyMonMoves
	jr nz, .playerTurnNotLinkBattle
; .playerTurnLinkBattle	
	push hl
	ld hl, wEnemyMonPP
.enemyTurn
	push hl
	ld a, [hli]
	or [hl]
	inc hl
	or [hl]
	inc hl
	or [hl]
	and $3f
	pop hl ; wBattleMonPP or wEnemyMonPP
	jr z, .moveMissedPopHL ; nothing to do if all moves have no PP left
	add hl, bc
	ld a, [hl]
	pop hl
	and a
	jr z, .pickMoveToDisable ; pick another move if this one had 0 PP
.playerTurnNotLinkBattle
; non-link battle enemies have unlimited PP so the previous checks aren't needed
	call BattleRandom
	and $7
	inc a ; 1-8 turns disabled
	inc c ; move 1-4 will be disabled
	swap c 
	add c ; map disabled move to high nibble of W_ENEMYDISABLEDMOVE / W_PLAYERDISABLEDMOVE
	ld [de], a
	call PlayCurrentMoveAnimation2
	ld hl, wPlayerDisabledMoveNumber
	ld a, [H_WHOSETURN]
	and a
	jr nz, .printDisableText
	inc hl ; wEnemyDisabledMoveNumber
.printDisableText
	ld a, [wd11e] ; move number
	ld [hl], a  
	call GetMoveName
	ld hl, MoveWasDisabledText
	jp PrintText
.moveMissedPopHL
	pop hl
.moveMissed
	jp PrintButItFailedText_

MoveWasDisabledText: ; 3fb09 (f:7b09)
	TX_FAR _MoveWasDisabledText
	db "@"

PayDayEffect: ; 3fb0e (f:7b0e)
	ld hl, PayDayEffect_
	ld b, BANK(PayDayEffect_)
	jp Bankswitch

ConversionEffect: ; 3fb16 (f:7b16)
	ld hl, ConversionEffect_
	ld b, BANK(ConversionEffect_)
	jp Bankswitch

HazeEffect: ; 3fb1e (f:7b1e)
	ld hl, HazeEffect_
	ld b, BANK(HazeEffect_)
	jp Bankswitch

HealEffect: ; 3fb26 (f:7b26)
	ld hl, HealEffect_
	ld b, BANK(HealEffect_)
	jp Bankswitch

TransformEffect: ; 3fb2e (f:7b2e)
	ld hl, TransformEffect_
	ld b, BANK(TransformEffect_)
	jp Bankswitch

ReflectLightScreenEffect: ; 3fb36 (f:7b36)
	ld hl, ReflectLightScreenEffect_
	ld b, BANK(ReflectLightScreenEffect_)
	jp Bankswitch

NothingHappenedText: ; 3fb3e (f:7b3e)
	TX_FAR _NothingHappenedText
	db "@"

PrintNoEffectText: ; 3fb43 (f:7b43)
	ld hl, NoEffectText
	jp PrintText

NoEffectText: ; 3fb49 (f:7b49)
	TX_FAR _NoEffectText
	db "@"

ConditionalPrintButItFailed: ; 3fb4e (f:7b4e)
	ld a, [wMoveDidntMiss]
	and a
	ret nz ; return if the side effect failed, yet the attack was successful

PrintButItFailedText_: ; 3fb53 (f:7b53)
	ld hl, ButItFailedText
	jp PrintText

ButItFailedText: ; 3fb59 (f:7b59)
	TX_FAR _ButItFailedText
	db "@"

PrintDidntAffectText: ; 3fb5e (f:7b5e)
	ld hl, DidntAffectText
	jp PrintText

DidntAffectText: ; 3fb64 (f:7b64)
	TX_FAR _DidntAffectText
	db "@"

IsUnaffectedText: ; 3fb69 (f:7b69)
	TX_FAR _IsUnaffectedText
	db "@"

PrintMayNotAttackText: ; 3fb6e (f:7b6e)
	ld hl, ParalyzedMayNotAttackText
	jp PrintText

ParalyzedMayNotAttackText: ; 3fb74 (f:7b74)
	TX_FAR _ParalyzedMayNotAttackText
	db "@"

CheckTargetSubstitute: ; 3fb79 (f:7b79)
	push hl
	ld hl, W_ENEMYBATTSTATUS2
	ld a, [H_WHOSETURN]   
	and a
	jr z, .next1
	ld hl, W_PLAYERBATTSTATUS2
.next1
	bit HasSubstituteUp, [hl]         
	pop hl
	ret

PlayCurrentMoveAnimation2: ; 3fb89 (f:7b89)
; animation at MOVENUM will be played unless MOVENUM is 0
; plays wAnimationType 3 or 6
	ld a, [H_WHOSETURN]
	and a
	ld a, [W_PLAYERMOVENUM]
	jr z, .notEnemyTurn
	ld a, [W_ENEMYMOVENUM]
.notEnemyTurn
	and a
	ret z

PlayBattleAnimation2: ; 3fb96 (f:7b96)
; play animation ID at a and animation type 6 or 3
	ld [W_ANIMATIONID], a
	ld a, [H_WHOSETURN]
	and a
	ld a, $6
	jr z, .storeAnimationType
	ld a, $3
.storeAnimationType
	ld [wAnimationType], a
	jp PlayBattleAnimationGotID

PlayCurrentMoveAnimation: ; 3fba8 (f:7ba8)
; animation at MOVENUM will be played unless MOVENUM is 0
; resets wAnimationType
	xor a
	ld [wAnimationType], a
	ld a, [H_WHOSETURN]
	and a
	ld a, [W_PLAYERMOVENUM]
	jr z, .notEnemyTurn
	ld a, [W_ENEMYMOVENUM]
.notEnemyTurn
	and a
	ret z

PlayBattleAnimation: ; 3fbb9 (f:7bb9)
; play animation ID at a and predefined animation type
	ld [W_ANIMATIONID], a

PlayBattleAnimationGotID: ; 3fbbc (f:7bbc)
; play animation at W_ANIMATIONID
	push hl
	push de
	push bc
	predef MoveAnimation
	pop bc
	pop de
	pop hl
	ret

; HAX	
LoadMonBackSpriteHook:
	ld a,$66
	ld de,vBackPic
	ld c,a
	jp LoadUncompressedSpriteData

PrintEXPBar:
	push de
	call CalcEXPBarPixelLength
	ld a, [H_QUOTIENT + 3] ; pixel length
	ld [wEXPBarPixelLength], a
	ld b, a
	ld c, $08
	ld d, $08
	pop hl
.loop
	ld a, b
	sub c
	jr nc, .skip
	ld c, b
	jr .loop
.skip
	ld b, a
	ld a, $c0
	add c
.loop2
	ld [hld], a
	dec d
	ret z
	ld a, b
	and a
	jr nz, .loop
	ld a, $c0
	jr .loop2

CalcEXPBarPixelLength:
	ld hl, wEXPBarKeepFullFlag
	bit 0, [hl]
	jr z, .start
	res 0, [hl]
	ld a, $40
	ld [H_QUOTIENT + 3], a
	ret

.start
	ld hl, wd72c
	bit 1, [hl]
	jr z, .isBattleScreen
	ld hl, wLoadedMonSpecies
	jr .skip

.isBattleScreen
	; get the base exp needed for the current level
	ld a, [W_PLAYERBATTSTATUS3]
	ld hl, wBattleMonSpecies
	bit 3, a
	jr z, .skip
	ld hl, wPartyMon1
	call BattleMonPartyAttr
.skip
	ld a, [hl]
	ld [wd0b5], a
	call GetMonHeader
	ld a, [wBattleMonLevel]	
	ld d, a
	callab CalcExperience
	ld hl, H_MULTIPLICAND
	ld de, wEXPBarBaseEXP
	ld a, [hli]
	ld [de], a
	inc de
	ld a, [hli]
	ld [de], a
	inc de
	ld a, [hl]
	ld [de], a

	; get the exp needed to gain a level
	ld a, [wBattleMonLevel]
	ld d, a
	inc d
	callab CalcExperience

	; get the address of the active Pokemon's current experience
	ld hl, wd72c
	bit 1, [hl]
	jr z, .isBattleScreen2
	ld hl, wLoadedMonExp
	jr .skip2
.isBattleScreen2
	ld hl, wPartyMon1Exp
	call BattleMonPartyAttr
.skip2
	; current exp - base exp
	ld b, h
	ld c, l
	ld hl, wEXPBarBaseEXP
	ld de, wEXPBarCurEXP
	call SubThreeByteNum

	; exp needed - base exp
	ld bc, H_MULTIPLICAND
	ld hl, wEXPBarBaseEXP
	ld de, wEXPBarNeededEXP
	call SubThreeByteNum

	; make the divisor an 8-bit number
	ld hl, wEXPBarNeededEXP
	ld de, wEXPBarCurEXP + 1
	ld a, [hli]
	and a
	jr z, .twoBytes
	ld a, [hli]
	ld [hld], a
	dec hl
	ld a, [hli]
	ld [hld], a
	ld a, [de]
	inc de
	ld [de], a
	dec de
	dec de
	ld a, [de]
	inc de
	ld [de], a
	dec de
	xor a
	ld [hli], a
	ld [de], a
	inc de
.twoBytes
	ld a, [hl]
	and a
	jr z, .oneByte
	srl a
	ld [hli], a
	ld a, [hl]
	rr a
	ld [hld], a
	ld a, [de]
	srl a
	ld [de], a
	inc de
	ld a, [de]
	rr a
	ld [de], a
	dec de
	jr .twoBytes
.oneByte

	; current exp * (8 tiles * 8 pixels)
	ld hl, H_MULTIPLICAND
	ld de, wEXPBarCurEXP
	ld a, [de]
	inc de
	ld [hli], a
	ld a, [de]
	inc de
	ld [hli], a
	ld a, [de]
	ld [hl], a
	ld a, $40
	ld [H_MULTIPLIER], a
	call Multiply

	; product / needed exp = pixel length
	ld a, [wEXPBarNeededEXP + 2]
	ld [H_DIVISOR], a
	ld b, $04
	jp Divide

; calculates the three byte number starting at [bc]
; minus the three byte number starting at [hl]
; and stores it into the three bytes starting at [de]
; assumes that [hl] is smaller than [bc]
SubThreeByteNum:
	call .subByte
	call .subByte
.subByte
	ld a, [bc]
	inc bc
	sub [hl]
	inc hl
	ld [de], a
	jr nc, .noCarry
	dec de
	ld a, [de]
	dec a
	ld [de], a
	inc de
.noCarry
	inc de
	ret

; return the address of the BattleMon's party struct attribute in hl
BattleMonPartyAttr:
	ld a, [wPlayerMonNumber]
	ld bc, wPartyMon2 - wPartyMon1
	jp AddNTimes
