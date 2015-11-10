;; sourcepawn-mode.el - SourcePawn major mode for emacs
;; Copyright (c) 2010, Aaron Griffith <aargri@gmail.com>
;; This file is licensed under the GNU GPL -- see below.
;;
;; SourcePawn is a scripting language for SourceMod, which can be
;; found at <http://www.sourcemod.net/>,
;;
;; More (and nicer) documentation for sourcepawn-mode may be found at
;; <http://gamma-level.com/teamfortress2/sourcepawn-mode>.
;;
;; Suggestions, improvements, and bug reports are welcome. Please
;; contact me at the email address above!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           INSTALLATION                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: the file `proto-sourcepawn-mode.el` is used to GENERATE the
;; file `sourcepawn-mode.el`, which is what you should install. DO NOT
;; USE the proto version: it will not work. Instead, see the README
;; for how to generate the real file, or get a pregenerated file from
;; my website, linked above.
;;
;; Installation instructions:
;;
;; 1. Put this file somewhere in your emacs load path OR add the
;;    following to your .emacs file (modifying the path
;;    appropriately):
;;
;;    (add-to-list 'load-path "/home/agrif/emacsinclude")
;;
;; 2. Add the following to your .emacs file to load this file
;;    automatically when needed, and to make this autoload *.sp files:
;;
;;    (autoload 'sourcepawn-mode "sourcepawn-mode" nil t)
;;    (add-to-list 'auto-mode-alist '(".sp\\'" . sourcepawn-mode))
;;
;; 3. (Optional) Customize SourcePawn mode with your own hooks.  Below
;;    is a sample which automatically untabifies when you save:
;;
;;    (defun my-sourcepawn-mode-hook ()
;;      (add-hook 'local-write-file-hooks 'auto-untabify-on-save))
;;
;;    (add-hook 'sourcepawn-mode-hook 'my-sourcepawn-mode-hook)
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              LICENSE                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file is released under the GNU GPL.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       END OF DOCUMENTATION                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; STILL TODO - customize, compile? menus? tags?

;; define the mode hook list
(defvar sourcepawn-mode-hook
  nil
  "A list for storing sourcepawn-mode hooks.")

;; breaking down the variable name match regexp into parts
(defvar sourcepawn-mode-font-lock-regexp-variable-names
  "\\(?:\\(?:\\sw\\)+:\\)?\\(\\(?:\\sw\\)+\\)\\(?:[ \t]*\\[[^,;\n]*\\]\\)?\\(?:[ \t]*=[ \t]*\\(?:\\sw\\|\\s_\\|[ \t]\\)+\\)?"
  "A regexp that matches the list part of SourcePawn variable declarations, e.g. 'String:test[256] = \"test\"'. The variable name must be in group 1.")

(defvar sourcepawn-mode-font-lock-regexp-variable-names-prefix
  "\\(?:new\\|decl\\)[ \t]+"
  "A regexp that matches the part of a variable declaration before the variable names list. Must have no numbered groups.")

(defvar sourcepawn-mode-font-lock-regexp-variable-names-seperator
  "[ \t]*,[ \t]*"
  "A regexp that matches the seperator between a variable declaration list element. Must have no numbered groups.")

;; helper to tell us when our last match succeeded
(defvar sourcepawn-mode-font-lock-flag-inside-variable-declaration
  nil
  "A flag that, when t, means sourcepawn-mode-font-lock-matcher-variable-names is inside a variable declaration.")

;; the function to match variable names
(defun sourcepawn-mode-font-lock-matcher-variable-names (limit)
  "A font lock matcher function for SourcePawn variable declarations."
  (let ((start-regexp (concat sourcepawn-mode-font-lock-regexp-variable-names-prefix sourcepawn-mode-font-lock-regexp-variable-names))
		(list-regexp (concat sourcepawn-mode-font-lock-regexp-variable-names-seperator sourcepawn-mode-font-lock-regexp-variable-names)))
	(if (not (and sourcepawn-mode-font-lock-flag-inside-variable-declaration (looking-at list-regexp)))
		(setq sourcepawn-mode-font-lock-flag-inside-variable-declaration (re-search-forward start-regexp limit t))
	  (setq sourcepawn-mode-font-lock-flag-inside-variable-declaration (re-search-forward list-regexp limit t)))))

;; set up the syntax table
(defvar sourcepawn-mode-syntax-table
  (let ((st (make-syntax-table)))
	;; make _ a words character, so tokens == words, and word movement commands make sense
	(modify-syntax-entry ?_ "w" st)
	
	;; syntax classes used in C and C++ style comments
	(modify-syntax-entry ?/ "_ 124b" st)
	(modify-syntax-entry ?* "_ 23" st)
	(modify-syntax-entry (string-to-char "\n") "> b")
	st)
  "Syntax table for sourcepawn-mode.")

;; tells us when we are directly after a non-braced if/while/for statement
;; returns nil if not, or the indentation of the if/while/for if it is
(defun sourcepawn-mode-single-line-block-p ()
  "Tells us if the current line is a single-expression block, and returns the indentation of the start of that block."
  (save-excursion
	(beginning-of-line)
	;; can't be a special block if we start with a brace!
	(if (or (bobp) (looking-at "[ \t]*{"))
		nil
	  (forward-line -1)
	  (beginning-of-line)
	  (let ((limit-point (point)))
		(setq limit-point (save-excursion
							;; put point at the end of the line, or the start of a comment
							(if (re-search-forward "\\(//\\|/\\*\\)" (line-end-position) t)
								(match-beginning 1)
							  (line-end-position))))
		(if (and 
			 (re-search-forward "[ \t]*\\<\\(?:if\\|while\\|for\\)\\>[ \t]*([^\n]*)[ \t]*" limit-point t)
			 (equal (point) limit-point))
			(current-indentation)
		  nil)))))

;; a replacement for indent-line-to with sane point management
(defun sourcepawn-mode-indent-line-to (column)
  "Like indent-line-to but with sane point management."
  (if (string-match "^[ \\t]*$" (buffer-substring (point-at-bol) (point)))
	  (indent-line-to column)
	(save-excursion (indent-line-to column))))

;; our indentation function
(defun sourcepawn-mode-indent-line ()
  "Indent the current line as SourcePawn code."
  (interactive)
  ;; set ret to 'noindent to signal indentation cannot be done
  ;; endbrace-count stores how many "}" we see right at the beginning of the line
  (let (ret (endbrace-count 0))
	(sourcepawn-mode-indent-line-to
	 ;; make sure we don't indent to a negative
	 (max 0
		  (save-excursion
			(beginning-of-line)
			(if (bobp)
				0 ;; first line
			  ;; not first line, what should we indent to?
			  (let ((special-indent (sourcepawn-mode-single-line-block-p)))
				(if (not (null special-indent))
					(+ default-tab-width special-indent) ;; indent once, we're special
				  ;; we're not special :(
				  ;; check our relative matching-parens ()[]{} depth in the last line
				  ;; and indent in or out that much relative to last line's indentation
				  ;; count how many "}" there are on the line we will indent
				  ;; DECREMENT because we want these to act like the end of the last line
				  (save-excursion
					(while (and (looking-at "[ \t]*}") (re-search-forward "[ \t]*}" (line-end-position) t))
					  (setq endbrace-count (- endbrace-count 1)))
					;; first find last non-blank line, non-special line
					(forward-line -1)
					(while (and (not (bobp)) (or (looking-at "[ \t]*$") (sourcepawn-mode-single-line-block-p)))
					  (forward-line -1))
					;; count how many "}" there are at the beginning of the line (which is currently the last line)
					;; INCREMENT because these are working against what parse-partial-sexp finds
					(while (and (looking-at "[ \t]*}") (re-search-forward "[ \t]*}" (line-end-position) t))
					  (setq endbrace-count (+ endbrace-count 1)))
					;; add in the indentation for this S-EXP level
					(+ (current-indentation)
					   (* default-tab-width
						  (+ (car (parse-partial-sexp (line-beginning-position) (line-end-position)))
							 endbrace-count))))))))))
	ret))

;; Symbol lists auto-generated from SourcePawn includes. As such, there may be errors.
;; DO NOT CHANGE the parts after this comment, before the end -!- comment
;; IT WILL JUST CHANGE BACK
;; change the appropriate sp-reserved-keywords files in the source package
;; -!- start generated keywords

(defvar sourcepawn-mode-font-lock-regexp-keywords
  "\\<\\(assert\\|b\\(?:egin\\|reak\\)\\|c\\(?:ase\\|ellsof\\|hars\\|on\\(?:st\\|tinue\\)\\)\\|d\\(?:e\\(?:cl\\|fault\\)\\|o\\)\\|e\\(?:lse\\|num\\|xit\\)\\|f\\(?:or\\(?:ward\\)?\\|unc\\(?:enum\\|tag\\)\\)\\|if\\|n\\(?:ative\\|ew\\)\\|operator\\|public\\|return\\|s\\(?:izeof\\|t\\(?:atic\\|ock\\|ruct\\)\\|witch\\)\\|tagof\\|while\\)\\>"
  "An optimized regexp of SourcePawn keywords.")

(defvar sourcepawn-mode-font-lock-regexp-constants
  "\\<\\(c\\(?:ell\\(?:bits\\|m\\(?:ax\\|in\\)\\)\\|har\\(?:bits\\|m\\(?:ax\\|in\\)\\)\\)\\|debug\\|false\\|true\\|ucharmax\\)\\>"
  "An optimized regexp of SourcePawn constants.")

(defvar sourcepawn-mode-font-lock-regexp-types
  "\\<\\(F\\(?:ixed\\|loat\\)\\|String\\|bool\\)\\>"
  "An optimized regexp of SourcePawn types.")

(defvar sourcepawn-mode-font-lock-regexp-preprocessor
  "\\<\\(assert\\|ctrlchar\\|d\\(?:efined?\\|ynamic\\)\\|e\\(?:lse\\(?:if\\)?\\|mit\\|nd\\(?:i\\(?:f\\|nput\\)\\|script\\)\\|rror\\)\\|i\\(?:f\\|nclude\\)\\|library\\|p\\(?:ack\\|ragma\\)\\|rational\\|semicolon\\|t\\(?:\\(?:absiz\\|ryinclud\\)e\\)\\|un\\(?:def\\|used\\)\\)\\>"
  "An optimized regexp of SourcePawn preprocessor.")

(defvar sourcepawn-mode-font-lock-regexp-generated-constants
  "\\<\\(A\\(?:DM\\(?:FLAG_\\(?:BAN\\|C\\(?:H\\(?:A\\(?:NGEMAP\\|T\\)\\|EATS\\)\\|ON\\(?:FIG\\|VARS\\)\\|USTOM[1-6]\\)\\|GENERIC\\|KICK\\|PASSWORD\\|R\\(?:CON\\|ESERVATION\\|OOT\\)\\|SLAY\\|UNBAN\\|VOTE\\)\\|INMENU_\\(?:\\(?:PLAYER\\|SERVER\\|VOTING\\)COMMANDS\\)\\)\\|LL_VISIBLE_CONTENTS\\|PLRes_\\(?:Failure\\|S\\(?:ilentFailure\\|uccess\\)\\)\\|UT\\(?:HMETHOD_\\(?:IP\\|NAME\\|STEAM\\)\\|OLOAD_EXTENSIONS\\)\\|ccess_\\(?:Effective\\|Real\\)\\|d\\(?:dress_\\(?:MinimumValid\\|Null\\)\\|min\\(?:Cache_\\(?:\\(?:Admin\\|Group\\|Override\\)s\\)\\|Flags_TOTAL\\|_\\(?:Ban\\|C\\(?:h\\(?:a\\(?:ngemap\\|t\\)\\|eats\\)\\|on\\(?:fig\\|vars\\)\\|ustom[1-6]\\)\\|Generic\\|Kick\\|Password\\|R\\(?:CON\\|eservation\\|oot\\)\\|Slay\\|Unban\\|Vote\\)\\)\\)\\)\\|BANFLAG_\\(?:AUT\\(?:HID\\|O\\)\\|IP\\|NOKICK\\)\\|C\\(?:O\\(?:MMAND_\\(?:FILTER_\\(?:ALIVE\\|CONNECTED\\|DEAD\\|NO_\\(?:BOTS\\|IMMUNITY\\|MULTI\\)\\)\\|TARGET_\\(?:AMBIGUOUS\\|EMPTY_FILTER\\|IMMUNE\\|NO\\(?:NE\\|T_\\(?:ALIVE\\|DEAD\\|HUMAN\\|IN_GAME\\)\\)\\)\\)\\|NTENTS_\\(?:A\\(?:REAPORTAL\\|UX\\)\\|CURRENT_\\(?:0\\|180\\|270\\|90\\|DOWN\\|UP\\)\\|DE\\(?:BRIS\\|TAIL\\)\\|EMPTY\\|GRATE\\|HITBOX\\|IGNORE_NODRAW_OPAQUE\\|LADDER\\|M\\(?:IST\\|O\\(?:NSTER\\(?:CLIP\\)?\\|VEABLE\\)\\)\\|O\\(?:PAQUE\\|RIGIN\\)\\|PLAYERCLIP\\|S\\(?:LIME\\|OLID\\)\\|T\\(?:E\\(?:AM[12]\\|STFOGVOLUME\\)\\|RANSLUCENT\\)\\|UNUSED[56]\\|W\\(?:ATER\\|INDOW\\)\\)\\)\\|S\\(?:RoundEnd_\\(?:BombDefused\\|CT\\(?:S\\(?:toppedEscape\\|urrender\\)\\|Win\\|sReachedHostage\\)\\|Draw\\|GameStart\\|Hostages\\(?:\\(?:Not\\)?Rescued\\)\\|T\\(?:arget\\(?:\\(?:Bomb\\|Sav\\)ed\\)\\|errorist\\(?:Win\\|s\\(?:Escaped\\|NotEscaped\\|Planted\\|S\\(?:topped\\|urrender\\)\\)\\)\\)\\|VIP\\(?:\\(?:Escap\\|Kill\\|NotEscap\\)ed\\)\\)\\|Weapon_\\(?:A\\(?:K47\\|SSAULTSUIT\\|UG\\|WP\\)\\|BIZON\\|C4\\|DE\\(?:AGLE\\|COY\\|FUSER\\)\\|ELITE\\|F\\(?:AMAS\\|IVESEVEN\\|LASHBANG\\)\\|G\\(?:3SG1\\|ALIL\\(?:AR\\)?\\|LOCK\\)\\|H\\(?:EGRENADE\\|KP2000\\)\\|INCGRENADE\\|K\\(?:EVLAR\\|NIFE\\(?:_GG\\)?\\)\\|M\\(?:249\\|3\\|4A1\\|A\\(?:C10\\|G7\\)\\|OLOTOV\\|P\\(?:5NAVY\\|[79]\\)\\)\\|N\\(?:EGEV\\|IGHTVISION\\|O\\(?:NE\\|VA\\)\\)\\|P\\(?:2\\(?:28\\|50\\)\\|90\\)\\|S\\(?:AWEDOFF\\|C\\(?:AR\\(?:17\\|20\\)\\|OUT\\)\\|G55[026]\\|HIELD\\|MOKEGRENADE\\|SG08\\)\\|T\\(?:ASER\\|EC9\\|MP\\)\\|U\\(?:MP45\\|SP\\)\\|XM1014\\)\\|_\\(?:DMG_HEADSHOT\\|SLOT_\\(?:C4\\|GRENADE\\|KNIFE\\|\\(?:PRIM\\|SECOND\\)ARY\\)\\|TEAM_\\(?:CT\\|NONE\\|SPECTATOR\\|T\\)\\)\\)\\|o\\(?:mmand_\\(?:Allow\\|Deny\\)\\|nVar\\(?:Bound_\\(?:\\(?:Low\\|Upp\\)er\\)\\|Query_\\(?:Not\\(?:\\(?:Foun\\|Vali\\)d\\)\\|Okay\\|Protected\\)\\)\\|okie\\(?:Access_P\\(?:r\\(?:ivate\\|otected\\)\\|ublic\\)\\|Menu\\(?:Action_\\(?:\\(?:Display\\|Select\\)Option\\)\\|_\\(?:OnOff\\(?:_Int\\)?\\|YesNo\\(?:_Int\\)?\\)\\)\\)\\)\\)\\|D\\(?:B\\(?:Bind_\\(?:Float\\|Int\\|String\\)\\|Prio_\\(?:High\\|Low\\|Normal\\)\\|Val_\\(?:Data\\|Error\\|Null\\|TypeMismatch\\)\\)\\|MG_\\(?:A\\(?:CID\\|IRBOAT\\|LWAYSGIB\\)\\|B\\(?:LAST\\(?:_SURFACE\\)?\\|U\\(?:CKSHOT\\|LLET\\|RN\\)\\)\\|C\\(?:LUB\\|R\\(?:IT\\|USH\\)\\)\\|D\\(?:I\\(?:RECT\\|SSOLVE\\)\\|ROWN\\(?:RECOVER\\)?\\)\\|ENERGYBEAM\\|FALL\\|GENERIC\\|NE\\(?:RVEGAS\\|VERGIB\\)\\|P\\(?:ARALYZE\\|HYSGUN\\|LASMA\\|OISON\\|REVENT_PHYSICS_FORCE\\)\\|R\\(?:ADIATION\\|EMOVENORAGDOLL\\)\\|S\\(?:HOCK\\|L\\(?:ASH\\|OWBURN\\)\\|ONIC\\)\\|VEHICLE\\)\\|ialogType_\\(?:AskConnect\\|Entry\\|M\\(?:enu\\|sg\\)\\|Text\\)\\)\\|E\\(?:T_\\(?:Event\\|Hook\\|\\(?:Ignor\\|Singl\\)e\\)\\|ngine_\\(?:AlienSwarm\\|Bl\\(?:\\(?:ad\\|oodyGoodTim\\)e\\)\\|C\\(?:S\\(?:GO\\|S\\)\\|ontagion\\)\\|D\\(?:O\\(?:DS\\|TA\\)\\|arkMessiah\\)\\|EYE\\|HL2DM\\|Insurgency\\|Left4Dead2?\\|NuclearDawn\\|Original\\|Portal2\\|S\\(?:DK2013\\|ourceSDK200[67]\\)\\|TF2\\|Unknown\\)\\|ventHookMode_P\\(?:ost\\(?:NoCopy\\)?\\|re\\)\\)\\|F\\(?:BEAM_\\(?:END\\(?:ENTITY\\|VISIBLE\\)\\|F\\(?:ADE\\(?:IN\\|OUT\\)\\|OREVER\\)\\|HALOBEAM\\|ISACTIVE\\|NOTILE\\|ONLYNOISEONCE\\|S\\(?:HADE\\(?:IN\\|OUT\\)\\|INENOISE\\|OLID\\|TART\\(?:ENTITY\\|VISIBLE\\)\\)\\|USE_HITBOXES\\)\\|CVAR_\\(?:ARCHIVE\\(?:_XBOX\\)?\\|C\\(?:HEAT\\|LIENTDLL\\)\\|D\\(?:ATACACHE\\|EMO\\|ONTRECORD\\)\\|FILESYSTEM\\|GAMEDLL\\|INPUTSYSTEM\\|LAUNCHER\\|MATERIAL_SYSTEM\\|N\\(?:E\\(?:TWORKSYSTEM\\|VER_AS_STRING\\)\\|O\\(?:NE\\|T\\(?:IFY\\|_CONNECTED\\)\\)\\)\\|P\\(?:LUGIN\\|R\\(?:INTABLEONLY\\|OTECTED\\)\\)\\|REPLICATED\\|S\\(?:OUNDSYSTEM\\|PONLY\\|TUDIORENDER\\)\\|TOOLSYSTEM\\|U\\(?:N\\(?:\\(?:LOGG\\|REGISTER\\)ED\\)\\|SERINFO\\)\\|VPHYSICS\\)\\|EATURECAP_\\(?:COMMANDLISTENER\\|PLAYERRUNCMD_11PARAMS\\)\\|L\\(?:OAT_PI\\|_\\(?:A\\(?:IMTARGET\\|TCONTROLS\\)\\|BASEVELOCITY\\|C\\(?:LIENT\\|ONVEYOR\\)\\|D\\(?:ISSOLVING\\|ONTTOUCH\\|UCKING\\)\\|E\\(?:DICT_\\(?:ALWAYS\\|CHANGED\\|D\\(?:IRTY_PVS_INFORMATION\\|ONTSEND\\)\\|F\\(?:REE\\|ULL\\(?:CHECK\\)?\\)\\|P\\(?:\\(?:ENDING_DORMANT_\\|VS\\)CHECK\\)\\)\\|P2V_UNKNOWN1\\)\\|F\\(?:AKECLIENT\\|LY\\|R\\(?:EEZING\\|OZEN\\)\\|ULL_EDICT_CHANGED\\)\\|G\\(?:ODMODE\\|R\\(?:APHED\\|ENADE\\)\\)\\|IN\\(?:RAIN\\|WATER\\)\\|KILLME\\|N\\(?:OTARGET\\|PC\\)\\|O\\(?:BJECT\\|N\\(?:FIRE\\|GROUND\\|TRAIN\\)\\)\\|PARTIALGROUND\\|S\\(?:T\\(?:ATICPROP\\|EPMOVEMENT\\)\\|WIM\\)\\|TRANSRAGDOLL\\|UNBLOCKABLE_BY_PLAYER\\|W\\(?:ATERJUMP\\|ORLDBRUSH\\)\\)\\)\\|PERM_\\(?:G_\\(?:EXEC\\|READ\\|WRITE\\)\\|O_\\(?:EXEC\\|READ\\|WRITE\\)\\|U_\\(?:EXEC\\|READ\\|WRITE\\)\\)\\|eature\\(?:Status_\\(?:Available\\|Un\\(?:available\\|known\\)\\)\\|Type_\\(?:Capability\\|Native\\)\\)\\|indMap_\\(?:F\\(?:ound\\|uzzyMatch\\)\\|No\\(?:nCanonical\\|tFound\\)\\|PossiblyAvailable\\)\\)\\|I\\(?:N\\(?:VALID_\\(?:ADMIN_ID\\|ENT_REFERENCE\\|FCVAR_FLAGS\\|GROUP_ID\\|MESSAGE_ID\\|STRING_\\(?:INDEX\\|TABLE\\)\\)\\|_\\(?:A\\(?:LT[12]\\|TTACK[23]?\\)\\|B\\(?:ACK\\|ULLRUSH\\)\\|CANCEL\\|DUCK\\|FORWARD\\|GRENADE[12]\\|JUMP\\|LEFT\\|MOVE\\(?:\\(?:LEF\\|RIGH\\)T\\)\\|R\\(?:ELOAD\\|IGHT\\|UN\\)\\|S\\(?:CORE\\|PEED\\)\\|USE\\|W\\(?:ALK\\|EAPON[12]\\)\\|ZOOM\\)\\)\\|TEMDRAW_\\(?:CONTROL\\|D\\(?:EFAULT\\|ISABLED\\)\\|IGNORE\\|NOTEXT\\|RAWLINE\\|SPACER\\)\\|dentity_\\(?:Core\\|\\(?:Extensio\\|Plugi\\)n\\)\\|mmunity_\\(?:Default\\|Global\\)\\)\\|KvData_\\(?:Color\\|Float\\|Int\\|N\\(?:UMTYPES\\|one\\)\\|Ptr\\|String\\|UInt64\\|WString\\)\\|L\\(?:A\\(?:NG_SERVER\\|ST_VISIBLE_CONTENTS\\)\\|isten_\\(?:Default\\|No\\|Yes\\)\\)\\|M\\(?:A\\(?:PLIST_FLAG_\\(?:CLEARARRAY\\|MAPSFOLDER\\|NO_DEFAULT\\)\\|SK_\\(?:ALL\\|NPC\\(?:SOLID\\(?:_BRUSHONLY\\)?\\|WORLDSTATIC\\)\\|OPAQUE\\(?:_AND_NPCS\\)?\\|PLAYERSOLID\\(?:_BRUSHONLY\\)?\\|S\\(?:HOT\\(?:_\\(?:\\(?:HUL\\|PORTA\\)L\\)\\)?\\|OLID\\(?:_BRUSHONLY\\)?\\|PLITAREAPORTAL\\)\\|VISIBLE\\(?:_AND_NPCS\\)?\\|WATER\\)\\|X\\(?:PLAYERS\\|_\\(?:LIGHTSTYLES\\|\\(?:NAME\\|TARGET\\)_LENGTH\\)\\)\\)\\|ENU\\(?:FLAG_\\(?:BUTTON_\\(?:EXIT\\(?:BACK\\)?\\|NOVOTE\\)\\|NO_SOUND\\)\\|_\\(?:ACTIONS_\\(?:ALL\\|DEFAULT\\)\\|NO_PAGINATION\\|TIME_FOREVER\\)\\)\\|O\\(?:TDPANEL_TYPE_\\(?:FILE\\|INDEX\\|TEXT\\|URL\\)\\|VETYPE_\\(?:CUSTOM\\|FLY\\(?:GRAVITY\\)?\\|ISOMETRIC\\|LADDER\\|NO\\(?:CLIP\\|NE\\)\\|OBSERVER\\|PUSH\\|STEP\\|VPHYSICS\\|WALK\\)\\)\\|apChange_\\(?:Instant\\|\\(?:Map\\|Round\\)End\\)\\|enu\\(?:Action_\\(?:Cancel\\|D\\(?:isplay\\(?:Item\\)?\\|rawItem\\)\\|End\\|S\\(?:\\(?:elec\\|tar\\)t\\)\\|Vote\\(?:Cancel\\|End\\|Start\\)\\)\\|S\\(?:ource_\\(?:External\\|No\\(?:ne\\|rmal\\)\\|RawPanel\\)\\|tyle_\\(?:Default\\|Radio\\|Valve\\)\\)\\)\\)\\|N\\(?:ominate_\\(?:A\\(?:dded\\|lreadyInVote\\)\\|InvalidMap\\|Replaced\\|VoteFull\\)\\|umberType_Int\\(?:16\\|32\\|8\\)\\)\\|Override_Command\\(?:Group\\)?\\|P\\(?:B_FIELD_NOT_REPEATED\\|CRE_\\(?:ANCHORED\\|CASELESS\\|DO\\(?:LLAR_ENDONLY\\|TALL\\)\\|EXTENDED\\|MULTILINE\\|NO\\(?:TEMPTY\\|_UTF8_CHECK\\)\\|U\\(?:CP\\|NGREEDY\\|TF8\\)\\)\\|LA\\(?:TFORM_MAX_PATH\\|YER_FLAG_BITS\\)\\|aram_\\(?:A\\(?:\\(?:n\\|rra\\)y\\)\\|Cell\\(?:ByRef\\)?\\|Float\\(?:ByRef\\)?\\|String\\|VarArgs\\)\\|l\\(?:Info_\\(?:Author\\|Description\\|Name\\|URL\\|Version\\)\\|ugin_\\(?:BadLoad\\|C\\(?:hanged\\|ontinue\\|reated\\)\\|Error\\|Failed\\|Handled\\|Loaded\\|Paused\\|Running\\|Stop\\|Uncompiled\\)\\)\\|rop\\(?:Field_\\(?:Entity\\|Float\\|Integer\\|String\\(?:_T\\)?\\|Unsupported\\|Vector\\)\\|_\\(?:Data\\|Send\\)\\)\\)\\|QUERYCOOKIE_FAILED\\|R\\(?:E\\(?:GEX_ERROR_\\(?:BAD\\(?:COUNT\\|ENDIANNESS\\|LENGTH\\|M\\(?:AGIC\\|ODE\\)\\|NEWLINE\\|O\\(?:FFSET\\|PTION\\)\\|PARTIAL\\|UTF8\\(?:_OFFSET\\)?\\)\\|CALLOUT\\|DFA_\\(?:BADRESTART\\|RECURSE\\|U\\(?:COND\\|ITEM\\|MLIMIT\\)\\|WSSIZE\\)\\|INTERNAL\\|JIT_\\(?:BADOPTION\\|STACKLIMIT\\)\\|MATCHLIMIT\\|N\\(?:O\\(?:M\\(?:ATCH\\|EMORY\\)\\|NE\\|SUBSTRING\\)\\|ULL\\(?:WSLIMIT\\)?\\)\\|PARTIAL\\|RECURS\\(?:ELOOP\\|IONLIMIT\\)\\|SHORTUTF8\\|UNKNOWN_OPCODE\\)\\|NDER\\(?:FX_\\(?:CLAMP_MIN_SCALE\\|DISTORT\\|E\\(?:NV_\\(?:RAIN\\|SNOW\\)\\|XPLODE\\)\\|F\\(?:ADE_\\(?:FAST\\|SLOW\\)\\|LICKER_\\(?:FAST\\|SLOW\\)\\)\\|GLOWSHELL\\|HOLOGRAM\\|MAX\\|NO\\(?:NE\\|_DISSIPATION\\)\\|PULSE_\\(?:FAST\\(?:_WIDER?\\)?\\|SLOW\\(?:_WIDE\\)?\\)\\|RAGDOLL\\|S\\(?:OLID_\\(?:FAST\\|SLOW\\)\\|POTLIGHT\\|TROBE_\\(?:FAST\\(?:ER\\)?\\|SLOW\\)\\)\\)\\|_\\(?:ENVIRONMENTAL\\|GLOW\\|NO\\(?:NE\\|RMAL\\)\\|TRANS\\(?:A\\(?:DD\\(?:FRAMEBLEND\\)?\\|LPHA\\(?:ADD\\)?\\)\\|COLOR\\|TEXTURE\\)\\|WORLDGLOW\\)\\)\\|QUIRE_\\(?:EXTENSIONS\\|PLUGIN\\)\\)\\|a\\(?:ngeType_\\(?:\\(?:Aud\\|Vis\\)ibility\\)\\|yType_\\(?:EndPoint\\|Infinite\\)\\)\\|oundState_\\(?:B\\(?:\\(?:etweenRound\\|onu\\)s\\)\\|GameOver\\|Init\\|Pre\\(?:game\\|round\\)\\|R\\(?:estart\\|oundRunning\\)\\|Sta\\(?:\\(?:lemat\\|rtGam\\)e\\)\\|TeamWin\\)\\)\\|S\\(?:DK\\(?:C\\(?:all_\\(?:Entity\\(?:List\\)?\\|GameRules\\|Player\\|Raw\\|Static\\)\\|onf_\\(?:Address\\|Signature\\|Virtual\\)\\)\\|Hook_\\(?:Blocked\\(?:Post\\)?\\|EndTouch\\(?:Post\\)?\\|FireBulletsPost\\|G\\(?:etMaxHealth\\|roundEntChangedPost\\)\\|OnTakeDamage\\(?:Alive\\(?:Post\\)?\\|Post\\)?\\|P\\(?:ostThink\\(?:Post\\)?\\|reThink\\(?:Post\\)?\\)\\|Reload\\(?:Post\\)?\\|S\\(?:etTransmit\\|houldCollide\\|pawn\\(?:Post\\)?\\|tartTouch\\(?:Post\\)?\\)\\|T\\(?:hink\\(?:Post\\)?\\|ouch\\(?:Post\\)?\\|raceAttack\\(?:Post\\)?\\)\\|Use\\(?:Post\\)?\\|VPhysicsUpdate\\(?:Post\\)?\\|Weapon\\(?:Can\\(?:SwitchTo\\(?:Post\\)?\\|Use\\(?:Post\\)?\\)\\|Drop\\(?:Post\\)?\\|Equip\\(?:Post\\)?\\|Switch\\(?:Post\\)?\\)\\)\\|Library_\\(?:Engine\\|Server\\)\\|Pass_\\(?:By\\(?:Ref\\|Value\\)\\|P\\(?:lain\\|ointer\\)\\)\\|Type_\\(?:Bool\\|CBase\\(?:Entity\\|Player\\)\\|Edict\\|Float\\|PlainOldData\\|QAngle\\|String\\|Vector\\)\\)\\|EEK_\\(?:CUR\\|END\\|SET\\)\\|M_\\(?:PARAM_\\(?:COPYBACK\\|STRING_\\(?:BINARY\\|COPY\\|UTF8\\)\\)\\|REPLY_TO_C\\(?:HAT\\|ONSOLE\\)\\)\\|ND\\(?:ATTN_\\(?:IDLE\\|NO\\(?:NE\\|RMAL\\)\\|RICOCHET\\|STATIC\\)\\|PITCH_\\(?:HIGH\\|LOW\\|NORMAL\\)\\|VOL_NORMAL\\)\\|OU\\(?:ND_FROM_\\(?:LOCAL_PLAYER\\|PLAYER\\|WORLD\\)\\|RCE\\(?:MOD_\\(?:PLUGINAPI_VERSION\\|V\\(?:ERSION\\|_\\(?:CSET\\|M\\(?:\\(?:AJ\\|IN\\)OR\\)\\|RE\\(?:LEASE\\|V\\)\\|TAG\\)\\)\\)\\|_SDK_\\(?:ALIENSWARM\\|BLOODYGOODTIME\\|CS\\(?:GO\\|S\\)\\|D\\(?:ARKMESSIAH\\|OTA\\)\\|E\\(?:PISODE\\(?:2VALVE\\|[12]\\)\\|YE\\)\\|LEFT4DEAD2?\\|ORIGINAL\\|UNKNOWN\\)\\)\\)\\|P_\\(?:ERROR_\\(?:A\\(?:BORTED\\|RRAY_\\(?:BOUNDS\\|TOO_BIG\\)\\)\\|D\\(?:ECOMPRESSOR\\|IVIDE_BY_ZERO\\)\\|FILE_FORMAT\\|HEAP\\(?:L\\(?:EAK\\|OW\\)\\|MIN\\)\\|IN\\(?:DEX\\|STRUCTION_PARAM\\|VALID_\\(?:ADDRESS\\|INSTRUCTION\\|NATIVE\\)\\)\\|MEMACCESS\\|N\\(?:ATIVE\\|O\\(?:NE\\|T\\(?:DEBUGGING\\|_\\(?:FOUND\\|RUNNABLE\\)\\)\\)\\)\\|PARAM\\(?:S_MAX\\)?\\|STACK\\(?:L\\(?:EAK\\|OW\\)\\|MIN\\)\\|TRACKER_BOUNDS\\)\\|PARAMFLAG_BYREF\\)\\|ort_\\(?:Ascending\\|Descending\\|Float\\|Integer\\|Random\\|String\\)\\)\\|T\\(?:E\\(?:MP_REQUIRE_EXTENSIONS\\|_EXPLFLAG_\\(?:DRAWALPHA\\|NO\\(?:ADDITIVE\\|DLIGHTS\\|FIREBALL\\(?:SMOKE\\)?\\|NE\\|PARTICLES\\|SOUND\\)\\|ROTATE\\)\\)\\|F\\(?:C\\(?:lass_\\(?:DemoMan\\|Engineer\\|Heavy\\|Medic\\|Pyro\\|S\\(?:cout\\|niper\\|oldier\\|py\\)\\|Unknown\\)\\|ond_\\(?:AfterburnImmune\\|B\\(?:alloonHead\\|l\\(?:ast\\(?:Immune\\|Jumping\\)\\|eeding\\)\\|onked\\|u\\(?:ffed\\|lletImmune\\)\\)\\|C\\(?:harging\\|loak\\(?:Flicker\\|ed\\)\\|rit\\(?:C\\(?:anteen\\|ola\\)\\|DemoCharge\\|Hype\\|Mmmph\\|On\\(?:Damage\\|F\\(?:irstBlood\\|lagCapture\\)\\|Kill\\|Win\\)\\|RuneTemp\\)\\)\\|D\\(?:azed\\|e\\(?:adRingered\\|fenseBuff\\(?:Mmmph\\|NoCritBlock\\|ed\\)\\|moBuff\\)\\|isguis\\(?:e\\(?:Removed\\|d\\(?:AsDispenser\\)?\\)\\|ing\\)\\|odgeChance\\)\\|F\\(?:ireImmune\\|ocusBuff\\|reezeInput\\)\\|GrapplingHook\\(?:Bleeding\\|Latched\\|SafeFall\\)?\\|H\\(?:a\\(?:lloween\\(?:BombHead\\|CritCandy\\|G\\(?:hostMode\\|iant\\)\\|InHell\\|Kart\\(?:Cage\\|Dash\\|NoTurn\\)?\\|QuickHeal\\|SpeedBoost\\|T\\(?:hriller\\|iny\\)\\)\\|sRune\\)\\|ealing\\)\\|InHealRadius\\|Jarated\\|Kritzkrieged\\|M\\(?:VMBotRadiowave\\|arkedForDeath\\(?:Silent\\)?\\|e\\(?:digunDebuff\\|gaHeal\\|leeOnly\\)\\|i\\(?:lked\\|niCritOnKill\\)\\)\\|NoHealingDamageBuff\\|O\\(?:bscuredSmoke\\|nFire\\|verhealed\\)\\|P\\(?:a\\(?:rachute\\|sstimeInterception\\)\\|reventDeath\\)\\|R\\(?:adiusHealOnDamage\\|e\\(?:genBuffed\\|programmed\\|strictToMelee\\)\\|une\\(?:Agility\\|Haste\\|Imbalance\\|Knockout\\|Precision\\|Re\\(?:gen\\|sist\\)\\|Strength\\|Vampire\\|Warlock\\)\\)\\|S\\(?:apped\\|lowed\\|mall\\(?:\\(?:B\\(?:\\(?:las\\|ulle\\)t\\)\\|Fire\\)Resist\\)\\|peedBuffAlly\\|tealthed\\(?:UserBuffFade\\)?\\|wimming\\(?:Curse\\|NoEffects\\)\\)\\|T\\(?:aunting\\|eleport\\(?:edGlow\\|ing\\)\\|mpDamageBonus\\)\\|U\\(?:ber\\(?:B\\(?:\\(?:las\\|ulle\\)tResist\\)\\|FireResist\\|charge\\(?:Fading\\|d\\(?:Canteen\\|Hidden\\|OnTakeDamage\\)?\\)\\)\\|nknown[12]\\)\\|Zoomed\\)\\)\\|Holiday_Invalid\\|Object\\(?:Mode_\\(?:E\\(?:ntrance\\|xit\\)\\|None\\)\\|_\\(?:CartDispenser\\|Dispenser\\|S\\(?:apper\\|entry\\)\\|Teleporter\\)\\)\\|Resource_\\(?:B\\(?:ackstabs\\|uildingsDestroyed\\)\\|Captures\\|D\\(?:\\(?:e\\(?:ath\\|fense\\)\\|omination\\)s\\)\\|Hea\\(?:\\(?:dsho\\|lPoin\\)ts\\)\\|Invulns\\|KillAssists\\|MaxHealth\\|P\\(?:ing\\|layerClass\\)\\|Re\\(?:supplyPoints\\|venge\\)\\|Score\\|T\\(?:eleports\\|otalScore\\)\\)\\|Team_\\(?:Blue\\|Red\\|Spectator\\|Unassigned\\)\\|_\\(?:CONDFLAG_\\(?:B\\(?:LEEDING\\|\\(?:ONK\\|UFF\\)ED\\)\\|C\\(?:HARGING\\|LOAK\\(?:ED\\|FLICKER\\)\\|RITCOLA\\)\\|D\\(?:AZED\\|E\\(?:ADRINGERED\\|FENSEBUFFED\\|MOBUFF\\)\\|ISGUIS\\(?:ED\\|ING\\)\\)\\|HEALING\\|INHEALRADIUS\\|JARATED\\|KRITZKRIEGED\\|M\\(?:ARKEDFORDEATH\\|EGAHEAL\\|ILKED\\)\\|NONE\\|O\\(?:NFIRE\\|VERHEALED\\)\\|REGENBUFFED\\|SLOWED\\|T\\(?:AUNTING\\|ELEPORT\\(?:GLOW\\|ING\\)\\)\\|UBERCHARGE\\(?:D\\|FADE\\)\\|ZOOMED\\)\\|DEATHFLAG_\\(?:ASSISTER\\(?:DOMINATION\\|REVENGE\\)\\|DEADRINGER\\|FIRSTBLOOD\\|GIBBED\\|INTERRUPTED\\|KILLER\\(?:DOMINATION\\|REVENGE\\)\\|PURGATORY\\)\\|STUNFLAG\\(?:S_\\(?:BIGBONK\\|GHOSTSCARE\\|LOSERSTATE\\|\\(?:NORMA\\|SMAL\\)LBONK\\)\\|_\\(?:BONKSTUCK\\|CHEERSOUND\\|GHOSTEFFECT\\|LIMITMOVEMENT\\|NOSOUNDOREFFECT\\|\\(?:SLOWDOW\\|THIRDPERSO\\)N\\)\\)\\)\\)\\|IMER_\\(?:DATA_HNDL_CLOSE\\|FLAG_NO_MAPCHANGE\\|HNDL_CLOSE\\|REPEAT\\)\\)\\|U\\(?:M_\\(?:\\(?:BitB\\|Protob\\)uf\\)\\|SERMSG_\\(?:BLOCKHOOKS\\|INITMSG\\|RELIABLE\\)\\|se_\\(?:O\\(?:ff\\|n\\)\\|Set\\|Toggle\\)\\)\\|V\\(?:DECODE_FLAG_\\(?:ALLOW\\(?:N\\(?:OTINGAME\\|ULL\\)\\|WORLD\\)\\|BYREF\\)\\|ENCODE_FLAG_COPYBACK\\|O\\(?:ICE_\\(?:LISTEN\\(?:ALL\\|TEAM\\)\\|MUTED\\|NORMAL\\|SPEAKALL\\|TEAM\\)\\|TE\\(?:FLAG_NO_REVOTES\\|INFO_\\(?:CLIENT_I\\(?:NDEX\\|TEM\\)\\|ITEM_\\(?:INDEX\\|VOTES\\)\\)\\)\\)\\)\\)\\>"
  "An optimized regexp of SourcePawn generated-constants.")

(defvar sourcepawn-mode-font-lock-regexp-generated-types
  "\\<\\(A\\(?:PLRes\\|ction\\|d\\(?:dress\\|m\\(?:AccessMode\\|in\\(?:CachePart\\|Flag\\|Id\\)\\)\\)\\|uthIdType\\)\\|C\\(?:S\\(?:RoundEndReason\\|WeaponID\\)\\|lientRangeType\\|o\\(?:nVar\\(?:Bounds\\|QueryResult\\)\\|okie\\(?:Access\\|Menu\\(?:Action\\)?\\)\\)\\)\\|D\\(?:B\\(?:BindType\\|Priority\\|Result\\)\\|ialogType\\)\\|E\\(?:ngineVersion\\|ventHookMode\\|x\\(?:ecType\\|tension\\)\\)\\|F\\(?:eature\\(?:Status\\|Type\\)\\|i\\(?:leT\\(?:\\(?:imeMod\\|yp\\)e\\)\\|ndMapResult\\)\\)\\|GroupId\\|Handle\\|I\\(?:dentity\\|mmunityType\\)\\|KvDataTypes\\|ListenOverride\\|M\\(?:apChange\\|enu\\(?:Action\\|S\\(?:\\(?:ourc\\|tyl\\)e\\)\\)\\|oveType\\)\\|N\\(?:etFlow\\|ominateResult\\|umberType\\)\\|Override\\(?:\\(?:Rul\\|Typ\\)e\\)\\|P\\(?:a\\(?:\\(?:ram\\|th\\)Type\\)\\|l\\(?:Vers\\|ugin\\(?:Info\\|Status\\)?\\)\\|rop\\(?:\\(?:Field\\)?Type\\)\\)\\|QueryCookie\\|R\\(?:ayType\\|e\\(?:gexError\\|nder\\(?:Fx\\|Mode\\)\\|plySource\\)\\|oundState\\)\\|S\\(?:DK\\(?:CallType\\|FuncConfSource\\|HookType\\|Library\\|PassMethod\\|Type\\)\\|MC\\(?:Error\\|Result\\)\\|haredPlugin\\|ort\\(?:Order\\|Type\\)\\)\\|T\\(?:F\\(?:C\\(?:lassType\\|ond\\)\\|Holiday\\|Object\\(?:\\(?:Mod\\|Typ\\)e\\)\\|ResourceType\\|Team\\)\\|opMenu\\(?:Action\\|Object\\(?:Type\\)?\\|Position\\)\\)\\|Use\\(?:Type\\|rM\\(?:essageType\\|sg\\)\\)\\)\\>"
  "An optimized regexp of SourcePawn generated-types.")

(defvar sourcepawn-mode-font-lock-regexp-generated-forwards
  "\\<\\(AskPluginLoad2?\\|CS_On\\(?:BuyCommand\\|CSWeaponDrop\\|GetWeaponPrice\\|TerminateRound\\)\\|On\\(?:Ban\\(?:Client\\|Identity\\)\\|Client\\(?:Co\\(?:mmand\\|nnect\\)\\|FloodCheck\\|PreAdminCheck\\|SayCommand\\)\\|LogAction\\|PlayerRunCmd\\|Re\\(?:buildAdminCache\\|moveBan\\)\\)\\|TF2_\\(?:CalcIsAttackCritical\\|On\\(?:GetHoliday\\|IsHolidayActive\\|PlayerTeleport\\)\\)\\|operator%\\)\\>"
  "An optimized regexp of SourcePawn generated-forwards.")

(defvar sourcepawn-mode-font-lock-regexp-generated-natives-stocks
  "\\<\\(A\\(?:TTN_TO_SNDLEVEL\\|c\\(?:ceptEntityInput\\|tivateEntity\\)\\|d\\(?:d\\(?:A\\(?:dmGroupCmdOverride\\|mbientSoundHook\\)\\|Command\\(?:Listener\\|Override\\)\\|FileToDownloadsTable\\|GameLogHook\\|MultiTargetFilter\\|NormalSoundHook\\|ServerTag\\|T\\(?:argetsToMenu2?\\|empEntHook\\|o\\(?:Forward\\|StringTable\\)\\)\\|\\(?:UserFlag\\|Vector\\)s\\)\\|minInheritGroup\\)\\|r\\(?:c\\(?:Cosine\\|Sine\\|Tangent2?\\)\\|eClientCookiesCached\\|ray\\(?:List\\|Stack\\)\\)\\|utoExecConfig\\)\\|B\\(?:a\\(?:n\\(?:Client\\|Identity\\)\\|seComm_\\(?:IsClient\\(?:\\(?:Gagg\\|Mut\\)ed\\)\\|SetClient\\(?:Gag\\|Mute\\)\\)\\)\\|i\\(?:ndAdminIdentity\\|tToFlag\\)\\|reakString\\|yteCountToCells\\)\\|C\\(?:S_\\(?:AliasToWeaponID\\|DropWeapon\\|Get\\(?:Client\\(?:Assists\\|C\\(?:lanTag\\|ontributionScore\\)\\)\\|MVPCount\\|T\\(?:eamScore\\|ranslatedWeaponAlias\\)\\|WeaponPrice\\)\\|IsValidWeaponID\\|RespawnPlayer\\|S\\(?:et\\(?:Client\\(?:Assists\\|C\\(?:lanTag\\|ontributionScore\\)\\)\\|MVPCount\\|TeamScore\\)\\|witchTeam\\)\\|TerminateRound\\|UpdateClientModel\\|WeaponIDToAlias\\)\\|a\\(?:ll_\\(?:Cancel\\|Finish\\|Push\\(?:Array\\(?:Ex\\)?\\|Cell\\(?:Ref\\)?\\|Float\\(?:Ref\\)?\\|String\\(?:Ex\\)?\\)\\|StartF\\(?:orward\\|unction\\)\\)\\|n\\(?:AdminTarget\\|MapChooserStartVote\\|TestFeatures\\|UserTarget\\)\\)\\|h\\(?:a\\(?:nge\\(?:ClientTeam\\|EdictState\\)\\|rTo\\(?:\\(?:Low\\|Upp\\)er\\)\\)\\|eck\\(?:\\(?:Command\\)?Access\\)\\)\\|l\\(?:ear\\(?:SyncHud\\|Trie\\)\\|ientCommand\\|o\\(?:[ns]eHandle\\)\\)\\|o\\(?:mmandExists\\|sine\\)\\|reate\\(?:A\\(?:dm\\(?:Group\\|in\\)\\|uthMethod\\)\\|D\\(?:ataTimer\\|ialog\\)\\|E\\(?:dict\\|ntityByName\\)\\|F\\(?:akeClient\\|orward\\)\\|GlobalForward\\|HudSynchronizer\\|Native\\|Profiler\\|T\\(?:imer\\|rie\\)\\)\\)\\|D\\(?:egToRad\\|isp\\(?:atch\\(?:KeyValue\\(?:Float\\|Vector\\)?\\|Spawn\\)\\|layAskConnectBox\\)\\|umpAdminCache\\)\\|E\\(?:mit\\(?:Ambient\\(?:\\(?:Game\\)?Sound\\)\\|GameSound\\(?:To\\(?:All\\|Client\\)\\)?\\|S\\(?:entence\\|ound\\(?:To\\(?:All\\|Client\\)\\)?\\)\\)\\|n\\(?:d\\(?:Message\\|OfMapVoteEnabled\\|PrepSDKCall\\)\\|t\\(?:IndexToEntRef\\|RefToEntIndex\\|erProfilingEvent\\)\\)\\|quipPlayerWeapon\\|x\\(?:p\\(?:lodeString\\|onential\\)\\|t\\(?:endMapTimeLimit\\|inguishEntity\\)\\)\\)\\|F\\(?:a\\(?:deClientVolume\\|keClientCommand\\(?:Ex\\)?\\)\\|ind\\(?:Adm\\(?:Group\\|inByIdentity\\)\\|C\\(?:lientCookie\\|ommandLineParam\\)\\|DataMap\\(?:Info\\|Offs\\)\\|EntityByClassname\\|F\\(?:irstConCommand\\|lag\\(?:By\\(?:Char\\|Name\\)\\|Char\\)\\)\\|NextConCommand\\|PluginBy\\(?:File\\|Number\\)\\|S\\(?:endProp\\(?:Info\\|Offs\\)\\|tring\\(?:Index\\|Table\\)\\)\\|T\\(?:arget\\|eamByName\\)\\)\\|l\\(?:ag\\(?:ArrayToBits\\|Bit\\(?:ArrayToBits\\|sTo\\(?:\\(?:Bit\\)?Array\\)\\)\\|ToBit\\)\\|oat\\(?:A\\(?:bs\\|dd\\)\\|Compare\\|Div\\|Fraction\\|Mul\\|Sub\\|ToString\\)\\)\\|or\\(?:ce\\(?:ChangeLevel\\|PlayerSuicide\\)\\|mat\\(?:ActivitySource\\|Ex\\|NativeString\\|Time\\|UserLogText\\)?\\)\\)\\|G\\(?:ame\\(?:ConfGet\\(?:Address\\|KeyValue\\|Offset\\)\\|Rules_\\(?:Get\\(?:Prop\\(?:Ent\\|Float\\|String\\|Vector\\)?\\|RoundState\\)\\|SetProp\\(?:Ent\\|Float\\|String\\|Vector\\)?\\)\\)\\|e\\(?:oipCo\\(?:de[23]\\|untry\\)\\|t\\(?:A\\(?:dm\\(?:Group\\(?:AddFlags?\\|CmdOverride\\|Immun\\(?:e\\(?:Count\\|From\\)\\|ity\\(?:Level\\)?\\)\\)\\|in\\(?:Flags?\\|Group\\(?:Count\\)?\\|ImmunityLevel\\|Password\\|Username\\)\\)\\|ngleVectors\\)\\|C\\(?:harBytes\\|lient\\(?:A\\(?:bs\\(?:Angles\\|Origin\\)\\|imTarget\\|rmor\\|uth\\(?:Id\\|String\\)\\|vg\\(?:Choke\\|Data\\|L\\(?:atency\\|oss\\)\\|Packets\\)\\)\\|Buttons\\|Co\\(?:okie\\(?:Time\\)?\\|unt\\)\\|D\\(?:ataRate\\|eaths\\)\\|Eye\\(?:Angles\\|Position\\)\\|Fr\\(?:ags\\|omSerial\\)\\|Health\\|I\\(?:P\\|nfo\\)\\|L\\(?:a\\(?:nguage\\|tency\\)\\|istening\\(?:Flags\\)?\\)\\|M\\(?:axs\\|ins\\|odel\\)\\|Name\\|OfUserId\\|Serial\\|T\\(?:eam\\|ime\\)\\|UserId\\|Weapon\\)\\|md\\(?:Arg\\(?:String\\|s\\)?\\|ReplySource\\)\\|o\\(?:mmand\\(?:Flags\\|Iterator\\|Line\\(?:Param\\(?:\\(?:Floa\\|In\\)t\\)?\\)?\\|Override\\)\\|okie\\(?:Access\\|Iterator\\)\\)\\|urrentMap\\)\\|DistGainFromSoundLevel\\|E\\(?:dict\\(?:Classname\\|Flags\\)\\|n\\(?:gine\\(?:Time\\|Version\\)\\|t\\(?:Data\\(?:Ent2?\\|Float\\|String\\|Vector\\)?\\|Prop\\(?:ArraySize\\|Ent\\|Float\\|String\\|Vector\\)?\\|SendPropOffs\\|ity\\(?:Address\\|C\\(?:lassname\\|ount\\)\\|Flags\\|Gravity\\|MoveType\\|NetClass\\|Render\\(?:Fx\\|Mode\\)\\)\\)\\)\\|x\\(?:cludeMapList\\|tensionFileStatus\\)\\)\\|F\\(?:eatureStatus\\|ileTime\\|orwardFunctionCount\\|unctionByName\\)\\|Game\\(?:Description\\|FolderName\\|SoundParams\\|Ti\\(?:ckCount\\|me\\)\\)\\|L\\(?:anguage\\(?:By\\(?:\\(?:Cod\\|Nam\\)e\\)\\|Count\\|Info\\)\\|istenOverride\\)\\|M\\(?:a\\(?:p\\(?:History\\(?:Size\\)?\\|TimeL\\(?:\\(?:ef\\|imi\\)t\\)\\)\\|x\\(?:\\(?:Client\\|Entitie\\|HumanPlayer\\)s\\)\\)\\|yHandle\\)\\|N\\(?:ative\\(?:Array\\|Cell\\(?:Ref\\)?\\|String\\(?:Length\\)?\\)\\|extMap\\|ominatedMapList\\|umStringTables\\)\\|P\\(?:l\\(?:ayer\\(?:DecalFile\\|JingleFile\\|ResourceEntity\\|WeaponSlot\\)\\|ugin\\(?:Filename\\|I\\(?:nfo\\|terator\\)\\|Status\\)\\)\\|rofilerTime\\)\\|Random\\(?:\\(?:Floa\\|In\\)t\\)\\|S\\(?:erver\\(?:Language\\|NetStats\\)\\|oundDuration\\|t\\(?:eamAccountID\\|ringTable\\(?:Data\\(?:Length\\)?\\|MaxStrings\\|N\\(?:ame\\|umStrings\\)\\)\\)\\|ysTickCount\\)\\|T\\(?:eam\\(?:C\\(?:\\(?:lientC\\)?ount\\)\\|\\(?:Nam\\|Scor\\)e\\)\\|i\\(?:ck\\(?:Interval\\|edTime\\)\\|me\\)\\|rieS\\(?:ize\\|napshotKey\\)\\)\\|U\\(?:Random\\(?:\\(?:Floa\\|In\\)t\\)\\|ser\\(?:Admin\\|FlagBits\\|Message\\(?:Id\\|\\(?:Nam\\|Typ\\)e\\)\\)\\)\\|Vector\\(?:Angles\\|CrossProduct\\|D\\(?:istance\\|otProduct\\)\\|Length\\|Vectors\\)\\)\\)\\|ivePlayer\\(?:Ammo\\|Item\\)\\|uessSDKVersion\\)\\|H\\(?:asEndOfMapVoteFinished\\|ook\\(?:EntityOutput\\|SingleEntityOutput\\|UserMessage\\)\\)\\|I\\(?:gniteEntity\\|mplodeStrings\\|n\\(?:activateClient\\|itiateMapChooserVote\\|sertServerCommand\\|tToString\\)\\|s\\(?:C\\(?:ha\\(?:r\\(?:Alpha\\|Lower\\|MB\\|Numeric\\|Space\\|Upper\\)\\|tTrigger\\)\\|lient\\(?:Authorized\\|Connected\\|In\\(?:\\(?:Gam\\|KickQueu\\)e\\)\\|Muted\\|Observer\\|Replay\\|SourceTV\\|TimingOut\\)\\)\\|De\\(?:calPrecached\\|dicatedServer\\)\\|EntNetworkable\\|FakeClient\\|GenericPrecached\\|M\\(?:\\(?:apVali\\|odelPrecache\\)d\\)\\|P\\(?:l\\(?:ayer\\(?:\\(?:Aliv\\|InGam\\)e\\)\\|uginDebugging\\)\\|rofilingActive\\)\\|S\\(?:erverProcessing\\|oundPrecached\\)\\|Valid\\(?:E\\(?:dict\\|ntity\\)\\|Handle\\)\\)\\)\\|K\\(?:eyValues\\|i\\(?:ckClient\\(?:Ex\\)?\\|llTimer\\)\\)\\|L\\(?:eaveProfilingEvent\\|ibraryExists\\|o\\(?:ad\\(?:FromAddress\\|GameConfigFile\\|\\(?:Map\\|Translation\\)s\\)\\|ckStringTables\\|g\\(?:Action\\|Error\\|Message\\|To\\(?:File\\(?:Ex\\)?\\|Game\\)\\|arithm\\)\\)\\)\\|M\\(?:a\\(?:ke\\(?:CompatEntRef\\|VectorFromPoints\\)\\|rkNativeAsOptional\\)\\|enu\\|orePlugins\\)\\|N\\(?:egateVector\\|o\\(?:minateMap\\|rmalizeVector\\|tifyPostAdminCheck\\)\\)\\|P\\(?:anel\\|ow\\|r\\(?:e\\(?:cache\\(?:Decal\\|Generic\\|Model\\|S\\(?:criptSound\\|entenceFile\\|ound\\)\\)\\|fetchSound\\|pSDKCall_\\(?:AddParameter\\|Set\\(?:Address\\|FromConf\\|ReturnInfo\\|Signature\\|Virtual\\)\\)\\)\\|int\\(?:CenterText\\(?:All\\)?\\|HintText\\(?:ToAll\\)?\\|To\\(?:C\\(?:hat\\(?:All\\)?\\|onsole\\)\\|Server\\)\\)\\|ocessTargetString\\)\\)\\|R\\(?:adToDeg\\|e\\(?:ad\\(?:Co\\(?:\\(?:mmand\\|okie\\)Iterator\\)\\|FlagString\\|MapList\\|Plugin\\|StringTable\\)\\|connectClient\\|displayAdminMenu\\|g\\(?:AdminCmd\\|C\\(?:lientCookie\\|onsoleCmd\\)\\|PluginLibrary\\|ServerCmd\\|isterAuthIdentType\\)\\|move\\(?:A\\(?:dmin\\|llFromForward\\|mbientSoundHook\\)\\|Ban\\|CommandListener\\|Edict\\|From\\(?:Forward\\|Trie\\)\\|GameLogHook\\|MultiTargetFilter\\|No\\(?:minationBy\\(?:Map\\|Owner\\)\\|rmalSoundHook\\)\\|PlayerItem\\|ServerTag\\|TempEntHook\\|UserFlags\\)\\|pl\\(?:aceString\\(?:Ex\\)?\\|yTo\\(?:Command\\|TargetError\\)\\)\\|qu\\(?:\\(?:estFram\\|ireFeatur\\)e\\)\\)\\|ound\\(?:Float\\|To\\(?:Ceil\\|Floor\\|Nearest\\|Zero\\)\\)\\|unAdminCacheChecks\\)\\|S\\(?:DKCall\\|MC\\(?:Parser\\|_SetParse\\(?:End\\|Start\\)\\)\\|QL_ExecuteTransaction\\|caleVector\\|e\\(?:rver\\(?:Command\\(?:Ex\\)?\\|Execute\\)\\|t\\(?:A\\(?:dm\\(?:Group\\(?:AddFlag\\|Immun\\(?:eFrom\\|ity\\(?:Level\\)?\\)\\)\\|in\\(?:Flag\\|ImmunityLevel\\|Password\\)\\)\\|uthIdCookie\\)\\|C\\(?:lient\\(?:Cookie\\|Info\\|L\\(?:anguage\\|istening\\(?:Flags\\)?\\)\\|ViewEntity\\)\\|mdReplySource\\|o\\(?:mmandFlags\\|okie\\(?:MenuItem\\|PrefabMenu\\)\\)\\)\\|E\\(?:dictFlags\\|nt\\(?:Data\\(?:Array\\|Ent2?\\|Float\\|String\\|Vector\\)?\\|Prop\\(?:Ent\\|Float\\|String\\|Vector\\)?\\|ity\\(?:Flags\\|Gravity\\|Health\\|Mo\\(?:del\\|veType\\)\\|Render\\(?:Color\\|Fx\\|Mode\\)\\)\\)\\)\\|Fa\\(?:ilState\\|keClientConVar\\)\\|GlobalTransTarget\\|HudTextParams\\(?:Ex\\)?\\|Li\\(?:\\(?:ghtStyl\\|stenOverrid\\)e\\)\\|MapListCompatBind\\|N\\(?:ative\\(?:Array\\|CellRef\\|String\\)\\|extMap\\)\\|RandomSeed\\|StringTableData\\|TeamScore\\|U\\(?:RandomSeed\\(?:Simple\\)?\\|ser\\(?:Admin\\|FlagBits\\)\\)\\|Variant\\(?:Bool\\|Color\\|Entity\\|Float\\|Int\\|PosVector3D\\|String\\|Vector3D\\)\\)\\)\\|how\\(?:Activity\\(?:2\\|Ex\\)?\\|CookieMenu\\|HudText\\|MOTDPanel\\|SyncHudText\\|VGUIPanel\\)\\|ine\\|lapPlayer\\|ort\\(?:ADTArray\\(?:Custom\\)?\\|Custom\\(?:[12]D\\)\\|\\(?:Float\\|Integer\\|String\\)s\\)\\|plitString\\|quareRoot\\|t\\(?:art\\(?:Message\\(?:All\\|Ex\\|One\\)?\\|Pr\\(?:epSDKCall\\|ofiling\\)\\)\\|o\\(?:p\\(?:Profiling\\|Sound\\)\\|reToAddress\\)\\|r\\(?:Break\\|C\\(?:at\\|o\\(?:mpare\\|ntains\\|py\\)\\)\\|Equal\\|i\\(?:ng\\(?:Map\\|To\\(?:Float\\(?:Ex\\)?\\|Int\\(?:Ex\\)?\\)\\)\\|pQuotes\\)\\)\\)\\|ubtractVectors\\)\\|T\\(?:E_\\(?:IsValidProp\\|Read\\(?:Float\\|Num\\|Vector\\)\\|S\\(?:e\\(?:nd\\(?:To\\(?:All\\|Client\\)\\)?\\|tup\\(?:ArmorRicochet\\|B\\(?:eam\\(?:Follow\\|Laser\\|Points\\|Ring\\(?:Point\\)?\\)\\|loodSprite\\)\\|Dust\\|E\\(?:nergySplash\\|xplosion\\)\\|GlowSprite\\|M\\(?:etalSparks\\|uzzleFlash\\)\\|S\\(?:moke\\|parks\\)\\)\\)\\|tart\\)\\|Write\\(?:Angles\\|EncodedEnt\\|Float\\(?:Array\\)?\\|Num\\|Vector\\)\\)\\|F2_\\(?:AddCondition\\|ChangeClientTeam\\|DisguisePlayer\\|Get\\(?:Cl\\(?:ass\\|ientTeam\\)\\|Object\\(?:\\(?:Mod\\|Typ\\)e\\)\\|Player\\(?:C\\(?:\\(?:las\\|onditionFlag\\)s\\)\\|ResourceData\\)\\|ResourceEntity\\)\\|I\\(?:gnitePlayer\\|s\\(?:HolidayActive\\|PlayerIn\\(?:Condition\\|Duel\\)\\)\\)\\|MakeBleed\\|Re\\(?:generatePlayer\\|move\\(?:AllWeapons\\|Condition\\|PlayerDisguise\\|Wea\\(?:ponSlot\\|rable\\)\\)\\|spawnPlayer\\)\\|S\\(?:etPlayer\\(?:Class\\|PowerPlay\\|ResourceData\\)\\|tunPlayer\\)\\)\\|R_\\(?:DidHit\\|Get\\(?:En\\(?:dPosition\\|tityIndex\\)\\|Fraction\\|HitGroup\\|P\\(?:laneNormal\\|ointContents\\(?:Ent\\)?\\)\\)\\|PointOutsideWorld\\|Trace\\(?:Hull\\(?:Ex\\|Filter\\(?:Ex\\)?\\)?\\|Ray\\(?:Ex\\|Filter\\(?:Ex\\)?\\)?\\)\\)\\|angent\\|eleportEntity\\|hrow\\(?:\\(?:Native\\)?Error\\)\\|opMenu\\|r\\(?:ansaction\\|i\\(?:eSnapshot\\(?:KeyBufferSize\\|Length\\)\\|ggerTimer\\|mString\\)\\)\\)\\|Un\\(?:hook\\(?:EntityOutput\\|SingleEntityOutput\\|UserMessage\\)\\|setCommandOverride\\)\\|V\\(?:Format\\|erifyCoreVersion\\)\\|__FLOAT_\\(?:\\(?:EQ\\|G[ET]\\|L[ET]\\|N\\(?:E\\|OT\\)\\)__\\)\\|float\\|get\\|s\\(?:et\\|tr\\(?:c\\(?:mp\\|opy\\)\\|len\\|ncmp\\)\\)\\)\\>"
  "An optimized regexp of SourcePawn generated-natives-stocks.")

;; -!- end generated keywords
;; OKAY, now you can edit again!

;; define a regexp for full, hash-prefixed preprocessor expressions
(defvar sourcepawn-mode-font-lock-regexp-preprocessor-full
  (concat "#" sourcepawn-mode-font-lock-regexp-preprocessor "\\(?:[ \t]+" sourcepawn-mode-font-lock-regexp-preprocessor "\\)*")
  "A preprocessor regexp that includes the first \"#\".")

;; set up the syntax highlighting defaults
(defvar sourcepawn-mode-font-lock-defaults
	  `(
		;; preprocessor statements
		(,sourcepawn-mode-font-lock-regexp-preprocessor-full 0 font-lock-preprocessor-face keep)
		;; string color for braced include statements
		("#[iI][nN][cC][lL][uU][dD][eE][ \t]+\\(<.*>\\)" 1 font-lock-string-face t)
		;; variable def color for define statements
		("#[dD][eE][fF][iI][nN][eE][ \t]+\\<\\(\\(?:\\sw\\)+\\)\\>" 1 font-lock-variable-name-face)
		
		(,sourcepawn-mode-font-lock-regexp-keywords 1 font-lock-keyword-face)

		(,sourcepawn-mode-font-lock-regexp-types 1 font-lock-type-face)
		(,sourcepawn-mode-font-lock-regexp-generated-types 1 font-lock-type-face)
		
		(,sourcepawn-mode-font-lock-regexp-constants 1 font-lock-constant-face)
		(,sourcepawn-mode-font-lock-regexp-generated-constants 1 font-lock-constant-face)
		
		(,sourcepawn-mode-font-lock-regexp-generated-natives-stocks 1 font-lock-builtin-face)
		(,sourcepawn-mode-font-lock-regexp-generated-forwards 1 font-lock-function-name-face)
		
		;; variable declarations
		(sourcepawn-mode-font-lock-matcher-variable-names 1 font-lock-variable-name-face)
	   )
	  "The default syntax highlighting rules for sourcepawn-mode.")

;; define our mode
(define-derived-mode sourcepawn-mode fundamental-mode
  "SourcePawn"
  "Major mode to edit SourcePawn source files."
  :syntax-table sourcepawn-mode-syntax-table
  (set (make-local-variable 'font-lock-defaults)
	   '(sourcepawn-mode-font-lock-defaults nil))

  ;; indentation
  (set (make-local-variable 'indent-line-function) 'sourcepawn-mode-indent-line)
  
  ;; comments
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-start-skip) "/\\*+ *\\|//+ *")
  (set (make-local-variable 'comment-end) ""))

;; register it to auto-load on *.sp files
;; Or, leave it up to the user (as I have)
;(add-to-list 'auto-mode-alist '(".sp\\'" . sourcepawn-mode))

;; tell emacs we provide sourcepawn-mode
(provide 'sourcepawn-mode)
