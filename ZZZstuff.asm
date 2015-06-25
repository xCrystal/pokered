AbraBaseStats: ; 38aa6 (e:4aa6)
db DEX_ABRA ; pokedex id
db 25 ; base hp
db 20 ; base attack
db 15 ; base defense
db 90 ; base speed
db 105 ; base special
db PSYCHIC ; species type 1
db PSYCHIC ; species type 2
db 200 ; catch rate
db 73 ; base exp yield
INCBIN "pic/bmon/abra.pic",0,1 ; 55, sprite dimensions
dw AbraPicFront
dw AbraPicBack
; attacks known at lvl 0
db TELEPORT
db 0
db 0
db 0
db 3 ; growth rate
; learnset
db %10110001
db %00000011
db %00001111
db %11110000
db %10000111
db %00111000
db %01000011
db BANK(AbraPicFront)

; this needs spdef
; merge both types in one byte
; i'll only want/need 40 tm flags
; use the bank byte and move it to a separate table

;************************;

; i don't want daycare at all there's lots of space to take from there

box_struct_length EQU 25 + NUM_MOVES * 2
box_struct: MACRO
\1Species::    db
\1HP::         dw
\1BoxLevel::   db
\1Status::     db
\1Type::
\1Type1::      db
\1Type2::      db
\1CatchRate::  db
\1Moves::      ds NUM_MOVES
\1OTID::       dw
\1Exp::        ds 3
\1HPExp::      dw
\1AttackExp::  dw
\1DefenseExp:: dw
\1SpeedExp::   dw
\1SpecialExp:: dw
\1DVs::        ds 2
\1PP::         ds NUM_MOVES
ENDM

party_struct: MACRO
	box_struct \1
\1Level::      db
\1Stats::
\1MaxHP::      dw
\1Attack::     dw
\1Defense::    dw
\1Speed::      dw
\1Special::    dw
ENDM

; so i need:
	; dw Spdef stat
	; dw SpdefHPDV
	; dw (db?) Spdef stat exp
	; db happiness (maybe 6-7 bits could do)
	; db flags for propensity to evolution, learn moves, randomly learnable moves?
	; db ITEM if they are only 64 items that can be held, this could go in six bits
; store stat exp in single bytes and possible use another byte for values up to 512
	; i could make stat exp gains random since 256 leaves little room and i dont like adv style
; types could be merged	

battle_struct: MACRO
\1Species::    db
\1HP::         dw
\1BoxLevel::   db
\1Status::     db
\1Type::
\1Type1::      db
\1Type2::      db
\1CatchRate::  db
\1Moves::      ds NUM_MOVES
\1DVs::        ds 2
\1Level::      db
\1MaxHP::      dw
\1Attack::     dw
\1Defense::    dw
\1Speed::      dw
\1Special::    dw
\1PP::         ds NUM_MOVES
ENDM

; here i nedd:
	; db item
	; dw spdef stat
	
	
;********************************;

; these would need an extra byte
wcceb:: ds 1 ; used to save the dvs of a mon when it uses transform
wccec:: ds 1 ; also used with above case	

; player unmodified special defense
wPlayerMonUnmodifiedMaxHP:: ; cd10
	ds 2

wInGameTradeTextPointerTableIndex:: ; cd12

wPlayerMonUnmodifiedAttack:: ; cd12
	ds 1
wInGameTradeGiveMonName:: ; cd13
	ds 1
wPlayerMonUnmodifiedDefense:: ; cd14
	ds 2
wPlayerMonUnmodifiedSpeed:: ; cd16
	ds 2
wPlayerMonUnmodifiedSpecial:: ; cd18
	ds 2
	
; special defense mods for player
wPlayerMonStatMods::
wPlayerMonAttackMod:: ; cd1a

	ds 1

	ds 3

; enemy unmodified special defense	
wEnemyMonUnmodifiedLevel:: ; cd23
	ds 1
wEnemyMonUnmodifiedMaxHP:: ; cd24
	ds 2
wEnemyMonUnmodifiedAttack:: ; cd26
	ds 2
wEnemyMonUnmodifiedDefense:: ; cd28
	ds 1

; sp def mod for enemy	
wEnemyMonStatMods::
wEnemyMonAttackMod:: ; cd2e	

; this party struct would need ~9 more bytes
; LoadMonData copies mon data here
wLoadedMon:: party_struct wLoadedMon ; cf98

; enemy mon is a battle struct, so 3 more 
; it will probably end up fully in one of the sections depends what i remove
wEnemyMon:: ; cfe5
; The wEnemyMon struct reaches past 0xcfff,
; the end of wram bank 0 on cgb.
; This has no significance on dmg, where wram
; isn't banked (c000-dfff is contiguous).
; However, recent versions of rgbds have replaced
; dmg-style wram with cgb wram banks.

; Until this is fixed, this struct will have
; to be declared manually.

; base spdef stat
wEnemyMonBaseStats:: ds 5

; these can be gone
wPlayerStatsToDouble:: ; d060
; always 0
	ds 1

wPlayerStatsToHalve:: ; d061
; always 0
	ds 1
	
wEnemyStatsToDouble:: ; d065
; always 0
	ds 1

wEnemyStatsToHalve:: ; d066
; always 0
	ds 1

; spdef here (this is a copy of base stats data)
W_MONHEADER:: ; d0b8
W_MONHDEXNUM:: ; d0b8
	ds 1

W_MONHBASESTATS:: ; d0b9
W_MONHBASEHP:: ; d0b9
	ds 1

; party mon structs ~9 * 6 more bytes
wPartyMons::
wPartyMon1:: party_struct wPartyMon1 ; d16b
wPartyMon2:: party_struct wPartyMon2 ; d197
wPartyMon3:: party_struct wPartyMon3 ; d1c3
wPartyMon4:: party_struct wPartyMon4 ; d1ef
wPartyMon5:: party_struct wPartyMon5 ; d21b
wPartyMon6:: party_struct wPartyMon6 ; d247

; enemy mon structs ~9 * 6 more bytes
wEnemyMons:: ; d8a4
wEnemyMon1:: party_struct wEnemyMon1
wEnemyMon2:: party_struct wEnemyMon2
wEnemyMon3:: party_struct wEnemyMon3
wEnemyMon4:: party_struct wEnemyMon4
wEnemyMon5:: party_struct wEnemyMon5
wEnemyMon6:: party_struct wEnemyMon6

; i dont think i want daycare
W_DAYCARE_IN_USE:: ; da48
; 0 if no pokemon is in the daycare
; 1 if pokemon is in the daycare
	ds 1

W_DAYCAREMONNAME:: ds 11 ; da49
W_DAYCAREMONOT::   ds 11 ; da54

wDayCareMon:: box_struct wDayCareMon ; da5f

; 20 mons per box
; but I want 10
; why is OT ds 11 is max len is 7+1?
W_NUMINBOX::  ds 1 ; da80
wBoxSpecies:: ds MONS_PER_BOX + 1

wBoxMons::
wBoxMon1:: box_struct wBoxMon1 ; da96
wBoxMon2:: ds box_struct_length * (MONS_PER_BOX + -1) ; dab7

wBoxMonOT::    ds 11 * MONS_PER_BOX ; dd2a
wBoxMonNicks:: ds 11 * MONS_PER_BOX ; de06
wBoxMonNicksEnd:: ; dee2