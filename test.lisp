(use-package :boxen)
(boxen:arm)

(defvar *foo* ┌─
              │gralt
              └─)


(setf *foo*
  ┌───────────────────────────────────────────────────────────────────
  │┌──────────────────────────────────────────────────────────────────
  ││Above all the wonders of Lisp's pantheon stand its metalinguistic
  ││tools; by their grace have Lisp's acolytes been liberated from the
  ││rigid asceticism of lesser faiths.  Thanks to Macro and kin, the
  ││jolly, complacent Lisp hacker can gaze through a fragrant cloud of
  ││setfs and defstructs at the emaciated unfortunates below, scraping
  ││out their meager code in inflexible notation, and sneer
  ││superciliously.  It's a good feeling.
  ││                                             -- Jonathan Amsterdam
  │└──────────────────────────────────────────────────────────────────
  └───────────────────────────────────────────────────────────────────)

(format t *foo*)

(setf *foo*
      ┌─────   
      │a
      │  hoo-ha
      │         of
      │            foos
      └────)

(format t *foo*)

(setf *foo* ┌─ ─ ─ ──   
      │a
            │  hoo-ha
      └────)

(format t *foo*)

(setf *foo* ┌
            │this  ⌖  should generate a warning
	    └)

(format t *foo*)

;; ; with shellshock:
;; (defvar *hunoz*
;;   ╔═ string ═══════
;;   ║This is the sound at the end of the world.
;;   #! play `locate end_of_world.wav`
;;   ║Which coincidentally, occurs at
;;   #! date
;;   #! sudo shutdown -h now "If it keeps on rainin', the levee's gonna break."
;;   ╚════════════════)

