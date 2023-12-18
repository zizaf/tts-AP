;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;-*-mode:scheme-*-
;;;                                                                       ;;
;;;                    Alan W Black and Kevin Lenzo                       ;;
;;;                         Copyright (c) 1998                            ;;
;;;                        All Rights Reserved.                           ;;
;;;                                                                       ;;
;;;  Permission is hereby granted, free of charge, to use and distribute  ;;
;;;  this software and its documentation without restriction, including   ;;
;;;  without limitation the rights to use, copy, modify, merge, publish,  ;;
;;;  distribute, sublicense, and/or sell copies of this work, and to      ;;
;;;  permit persons to whom this work is furnished to do so, subject to   ;;
;;;  the following conditions:                                            ;;
;;;   1. The code must retain the above copyright notice, this list of    ;;
;;;      conditions and the following disclaimer.                         ;;
;;;   2. Any modifications must be clearly marked as such.                ;;
;;;   3. Original authors' names are not deleted.                         ;;
;;;   4. The authors' names are not used to endorse or promote products   ;;
;;;      derived from this software without specific prior written        ;;
;;;      permission.                                                      ;;
;;;                                                                       ;;
;;;  THE AUTHORS OF THIS WORK DISCLAIM ALL WARRANTIES WITH REGARD TO      ;;
;;;  THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY   ;;
;;;  AND FITNESS, IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY         ;;
;;;  SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES            ;;
;;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   ;;
;;;  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          ;;
;;;  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       ;;
;;;  THIS SOFTWARE.                                                       ;;
;;;                                                                       ;;
;;;  This file is part "Building Voices in the Festival Speech            ;;
;;;  Synthesis System" by Alan W Black and Kevin Lenzo written at         ;;
;;;  Robotics Institute, Carnegie Mellon University, fall 98              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  A mixture of the US English diphone voice and Spanish feautures
;;;
;;;  Much of the front end is based on KED
;;;

;; changing the directory path
(defvar kal_diphone_dir (cdr (assoc 'kal_diphone_spanglish voice-locations))
  "kal_diphone_spanglish_dir
  The default directory for the kal diphone spanglish database.")
(set! load-path (cons (path-append kal_diphone_spanglish_dir "festvox/") load-path))

(require 'radio_phones) ;; wanting to use the same phone database as kal_diphone
(require_module 'UniSyn)

;; set this to lpc or psola
(defvar kal_sigpr 'lpc)
;; Reset this to ungroup for ungrouped version
(defvar kal_groupungroup 'group)

(if (probe_file (path-append kal_diphone_spanglish_dir "group/kallpc16k.group"))
    (defvar kal_index_file 
      (path-append kal_diphone_dir "group/kallpc16k.group"))
    (defvar kal_index_file 
      (path-append kal_diphone_dir "group/kallpc8k.group")))

(set! kal_psola_sep 
      (list
       '(name "kal_psola_sep")
       (list 'index_file (path-append kal_diphone_dir "dic/diphdic.est"))
       '(grouped "false")
       (list 'coef_dir (path-append kal_diphone_dir "pm"))
       (list 'sig_dir (path-append kal_diphone_dir "wav"))
       '(coef_ext ".pm")
       '(sig_ext ".wav")
;       '(alternates_left ((er ax)))
;       '(alternates_right (($p p) ($k k) ($g g) ($d d) ($b b) ($t t)
;				  (y ih) (ax ah) (ll l)))
       '(alternates_right ((er ax)))
       '(default_diphone "ax-ax")))

(set! kal_lpc_sep 
      (list
       '(name "kal_lpc_sep")
       (list 'index_file (path-append kal_diphone_dir "dic/diphdic.est"))
       '(grouped "false")
       (list 'coef_dir (path-append kal_diphone_dir "lpc"))
       (list 'sig_dir  (path-append kal_diphone_dir "lpc"))
       '(coef_ext ".lpc")
       '(sig_ext ".res")
       '(alternates_right ((er ax)))
       '(default_diphone "ax-ax")))

(set! kal_lpc_group 
      (list
       '(name "kal_lpc_group")
       (list 'index_file kal_index_file)
       '(grouped "true")
       '(alternates_right ((er ax)))
       '(default_diphone "ax-ax")))

(defvar kal_di_16k nil)
(defvar kal_di_8k nil)

(cond
 (kal_di_16k
  (require 'kal_di)
  (setup_kal_diphone_lpc_16k_grouped))
 (kal_di_8k
  (require 'kal_di)
  (setup_kal_diphone_lpc_8k_grouped))
 ((and (eq kal_sigpr 'psola)
       (eq kal_groupungroup 'group))
  (set! kal_db_name (us_diphone_init kal_psola_group)))
 ((and (eq kal_sigpr 'psola)
       (eq kal_groupungroup 'ungroup))
  (set! kal_db_name (us_diphone_init kal_psola_sep)))
 ((and (eq kal_sigpr 'lpc)
       (eq kal_groupungroup 'group))
  (set! kal_db_name (us_diphone_init kal_lpc_group)))
 ((and (eq kal_sigpr 'lpc)
       (eq kal_groupungroup 'ungroup))
  (set! kal_db_name (us_diphone_init kal_lpc_sep))))

;;;;  This part has been deleted as the identification of consonant clusters is irrelevant for Spansih
;;;;  Our general diphone scheme allows identification of consonant
;;;   clusters etc the follow rules should work for American English
;;;;

	  

;;;  Set up the CMU lexicon
;;;  The CMU lexicon is not needed for Spanish since the pronunciation can almost completely be predicted by its orthography.
;;;  (setup_cmu_lex)
;;;  Instead the code for the phone set from the documentation will be used.
 
(defPhoneSet
  spanish
  ;;;  Phone Features
  (;; vowel or consonant
   (vc + -)  
   ;; vowel length: short long diphthong schwa
   (vlng s l d a 0)
   ;; vowel height: high mid low
   (vheight 1 2 3 -)
   ;; vowel frontness: front mid back
   (vfront 1 2 3 -)
   ;; lip rounding
   (vrnd + -)
   ;; consonant type: stop fricative affricative nasal liquid
   (ctype s f a n l 0)
   ;; place of articulation: labial alveolar palatal labio-dental
   ;;                         dental velar
   (cplace l a p b d v 0)
   ;; consonant voicing
   (cvox + -)
   )
  ;; Phone set members (features are not! set properly)
  (
   (#  - 0 - - - 0 0 -)
   (a  + l 3 1 - 0 0 -)
   (e  + l 2 1 - 0 0 -)
   (i  + l 1 1 - 0 0 -)
   (o  + l 3 3 - 0 0 -)
   (u  + l 1 3 + 0 0 -)
   (b  - 0 - - + s l +)
   (ch - 0 - - + a a -)
   (d  - 0 - - + s a +)
   (f  - 0 - - + f b -)
   (g  - 0 - - + s p +)
   (j  - 0 - - + l a +)
   (k  - 0 - - + s p -)
   (l  - 0 - - + l d +)
   (ll - 0 - - + l d +)
   (m  - 0 - - + n l +)
   (n  - 0 - - + n d +)
   (ny - 0 - - + n v +)
   (p  - 0 - - + s l -)
   (r  - 0 - - + l p +)
   (rr - 0 - - + l p +)
   (s  - 0 - - + f a +)
   (t  - 0 - - + s a +)
   (th - 0 - - + f d +)
   (x  - 0 - - + a a -)
  )
)
(PhoneSet.silences '(#))

;;; Lexicon and LTS (letter to sound)
 (lex.create "spanish")
 (lex.set.phoneset "spanish")

(lts.ruleset
;  Name of rule set
 spanish
;  Sets used in the rules
(
  (LNS l n s )
  (AEOU a e o u )
  (AEO a e o )
  (EI e i )
  (BDGLMN b d g l m n )
)
;  Rules
(
 ( [ a ] = a )
 ( [ e ] = e )
 ( [ i ] = i )
 ( [ o ] = o )
 ( [ u ] = u )
 ( [ "'" a ] = a1 )   ;; stressed vowels
 ( [ "'" e ] = e1 )
 ( [ "'" i ] = i1 )
 ( [ "'" o ] = o1 )
 ( [ "'" u ] = u1 )
 ( [ b ] = b )
 ( [ v ] = b )
 ( [ c ] "'" EI = th )
 ( [ c ] EI = th )
 ( [ c h ] = ch )
 ( [ c ] = k )
 ( [ d ] = d )
 ( [ f ] = f )
 ( [ g ] "'" EI = x )
 ( [ g ] EI = x )
 ( [ g u ] "'" EI = g )
 ( [ g u ] EI = g )
 ( [ g ] = g )
 ( [ h u e ] = u e )
 ( [ h i e ] = i e )
 ( [ h ] =  )
 ( [ j ] = x )
 ( [ k ] = k )
 ( [ l l ] # = l )
 ( [ l l ] = ll )
 ( [ l ] = l )
 ( [ m ] = m )
 ( [ ~ n ] = ny )
 ( [ n ] = n )
 ( [ p ] = p )
 ( [ q u ] = k )
 ( [ r r ] = rr )
 ( # [ r ] = rr )
 ( LNS [ r ] = rr )
 ( [ r ] = r )
 ( [ s ] BDGLMN = th )
 ( [ s ] = s )
 ( # [ s ] C = e s )
 ( [ t ] = t )
 ( [ w ] = u )
 ( [ x ] = k s )
 ( AEO [ y ] = i )
 ( # [ y ] # = i )
 ( [ y ] = ll )
 ( [ z ] = th )
))

(lex.set.lts.ruleset 'spanish)

(define (spanish_lts word features)
  "(spanish_lts WORD FEATURES)
 Using letter to sound rules build a spanish pronunciations of WORD."
  (list word
        nil
        (lex.syllabify.phstress (lts.apply (downcase word) 'spanish))))
 (lex.set.lts.method spanish_lts)
 
 ;;;Phrasing
(set! spanish_phrase_cart_tree
'
((lisp_token_end_punc in ("?" "." ":"))
 ((BB))
 ((lisp_token_end_punc in ("'" "/" " " ";"))
  ((BB))
  ((n.name is 0) ;; end of utterance
  ((BB))
  ((NB))))))

;;;Duration
(set! spanish_dur_tree
 '
   ((R:SylStructure.parent.R:Syllable.p.syl_break > 1 ) ;; clause initial
    ((R:SylStructure.parent.stress is 1)
     ((1.5))
     ((1.2)))
    ((R:SylStructure.parent.syl_break > 1)   ;; clause final
     ((R:SylStructure.parent.stress is 1)
      ((2.0))
      ((1.5)))
     ((R:SylStructure.parent.stress is 1)
      ((1.2))
      ((1.0))))))
      
 (set! spanish_el_phone_data
'(
   (# 0.0 0.250)
   (a 0.0 0.090)
   (e 0.0 0.090)
   (i 0.0 0.080)
   (o 0.0 0.090)
   (u 0.0 0.080)
   (b 0.0 0.065)
   (ch 0.0 0.135)
   (d 0.0 0.060)
   (f 0.0 0.100)
   (g 0.0 0.080)
   (j 0.0 0.100)
   (k 0.0 0.100)
   (l 0.0 0.080)
   (ll 0.0 0.105)
   (m 0.0 0.070)
   (n 0.0 0.080)
   (ny 0.0 0.110)
   (p 0.0 0.100)
   (r 0.0 0.030)
   (rr 0.0 0.080)
   (s 0.0 0.110)
   (t 0.0 0.085)
   (th 0.0 0.100)
   (x 0.0 0.130)
))



(define (voice_kal_diphone_spanglish)
"(voice_kal_diphone_spanglish)
 Set up synthesis for Male Spanish speaker: Eduardo Lopez"
  ;; Phone set
  (voice_reset)
  (Parameter.set 'Language 'spanglish)
  (Parameter.set 'PhoneSet 'spanish)
  (PhoneSet.select 'spanish)
  ;; Tokenization rules I assume not required as they are specifically established for English
  ;;(set! token_to_words english_token_to_words)
  ;; POS tagger not required as it is established for English
  ;;(require 'pos)
  ;;(set! pos_lex_name "english_poslex")
  ;;(set! pos_ngram_name 'english_pos_ngram)
  ;;(set! pos_supported t)
  ;;(set! guess_pos english_guess_pos)   ;; need this for accents
  ;; Lexicon selection
  (lex.select "spanish")
  ;;(set! postlex_rules_hooks (list postlex_apos_s_check)) I assume not required.
  ;; Phrase prediction
  ;;(require 'phrase) We want to use the Cart Tree given in the documentation.
  (set! phrase_cart_tree spanish_phrase_cart_tree)
  (Parameter.set 'Phrase_Method 'cart_tree)
  ;;(set! phr_break_params english_phr_break_params) again only needed for English
  ;; Accent and tone prediction, will not be taken into consideration.
  ;;(require 'tobi)
  ;;(set! int_tone_cart_tree f2b_int_tone_cart_tree)
  ;;(set! int_accent_cart_tree f2b_int_accent_cart_tree)

  ;;(set! postlex_vowel_reduce_cart_tree 
  ;;postlex_vowel_reduce_cart_data)
  ;; F0 prediction
  ;;(require 'f2bf0lr)
  ;;(set! f0_lr_start f2b_f0_lr_start)
  ;;(set! f0_lr_mid f2b_f0_lr_mid)
  ;;(set! f0_lr_end f2b_f0_lr_end)
  ;;(Parameter.set 'Int_Method Intonation_Tree)
  ;;(set! int_lr_params
	;;'((target_f0_mean 105) (target_f0_std 14)
	  ;;(model_f0_mean 170) (model_f0_std 34)))
  ;;(Parameter.set 'Int_Target_Method Int_Targets_LR)
  ;; Duration prediction
  (set! duration_cart_tree spanish_dur_tree)
  (set! duration_ph_info spanish_el_phone_data)
  (Parameter.set 'Duration_Method 'Tree_ZScores)
  (Parameter.set 'Duration_Stretch 1.1)
  ;; Waveform synthesizer: kal diphones. The main objective was to use kal_diphone for the waveform synthesis
  ;; This assigned the diphone names from their context (_ $ etc)
  (cond
   ((or kal_di_16k kal_di_8k)
    ;; Waveform synthesizer: kal diphones
    (Parameter.set 'Synth_Method Diphone_Synthesize)
    ;; This assigned the diphone names from their context (_ $ etc)
    ;;(set! diphone_module_hooks (list kal_di_const_clusters ))
    (Diphone.select 'kal))
   (t
    (set! UniSyn_module_hooks (list kal_diphone_const_clusters ))
    (set! us_abs_offset 0.0)
    (set! window_factor 1.0)
    (set! us_rel_offset 0.0)
    (set! us_gain 0.9)

    (Parameter.set 'Synth_Method 'UniSyn)
    (Parameter.set 'us_sigpr kal_sigpr)
    (us_db_select kal_db_name)))

  (set! after_synth_hooks 
	(lambda (utt) 
	  (utt.wave.rescale utt 2.6)))

  (set! current-voice 'kal_diphone_spanglish)
)

(proclaim_voice
 'kal_diphone_spanglish
 '((language spanglish)
   (gender male)
   (dialect castille)
   (description
    "This voice is an attempt to a Castille Spanish male voice using a
     residual excited LPC diphone synthesis method.  It uses 
     the CMU Lexicon pronunciations.  Prosodic phrasing is provided 
     by a statistically trained model using part of speech and local 
     distribution of breaks.  Intonation is provided by a CART tree 
     predicting ToBI accents and an F0 contour generated from a model 
     trained from natural speech.  The duration model is also trained 
     from data using a CART tree.")))

(provide 'kal_diphone_spanglish)

