(ns clj-markov-melody.core
  (:require [cemerick.pomegranate :as pom]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Some helper functions to interface jMusic

(pom/add-classpath "jMusic1.6.3.jar")
(import jm.JMC)
(import (jm.music.data Note Score Part Phrase))
(import (jm.util Read))
(import (jm.util View))

(defn load-midi
  "Returns jMusic Note objects for the 0th track of MIDI file filename"
  [filename]
  (let [score (Score. filename)]
    (Read/midi score filename)
    (.getNoteList (.getPhrase (.getPart score 0) 0))))

(defn make-score-from-notes
  "Produce a jMusic Score from a seq of jMusic notes"
  [notes tempo inst]
  (let [score  (Score. )
        phrase (Phrase. )]
    (do
      (doseq [n notes] (.addNote phrase n))
      (.addPart score (Part. phrase "melody" inst))
      (.setTempo score tempo))
    score))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Markov chain routines

(defn transitions
  "Produces a transition table for the production of markov chains"
  [values]
  (reduce (fn [accum [k v]] (assoc accum k (conj (get accum k) v))) {}
          (partition 2 1 values)))

(defn markov-chain
  "Return a random chain of length len using transition table trans"
  [trans len]
  (loop [value (rand-nth (keys trans)) part []]
    (if (>= (count part) len)
      part
      (recur (if (not (empty? (trans value)))
               (rand-nth (trans value))
               (rand-nth (keys trans)))
             (conj part value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; example with music

;; Ah! vous dirai-je, Maman
(def twinkle (load-midi "midi/twinkle_twinkle.mid"))

;; the original melody
(View/notate
 (make-score-from-notes twinkle 120 jm.JMC/FLUTE))

;; produce a very similar random melody based on transitions between
;; pitch/duration tuples (i.e. "notes")
(View/notate
 (make-score-from-notes
  (markov-chain (transitions twinkle) 12) 120 jm.JMC/FLUTE))

;; produce a less similar permutation by considering pitch and duration
;; transitions separately, then recombining them
(defn markov-melody
  "Return a random melody of length 'len' based on 'notes'"
  [notes len]
  (map #(Note. %1 %2)
       (markov-chain (transitions (map #(.getPitch %) notes)) len)
       (markov-chain (transitions (map #(.getDuration %) notes)) len)))

(View/notate
 (make-score-from-notes
  (markov-melody twinkle 16) 120 jm.JMC/FLUTE))

;; Alouette, gentille Alouette
(def alouette (load-midi "midi/alouette.mid"))

(View/notate
 (make-score-from-notes
  (markov-melody alouette 16) 120 jm.JMC/FLUTE))

;; a random melody from the combination of twinkle and alouette
;; (N.B. works well because I've transposed them to the same key)
(View/notate
 (make-score-from-notes
  (markov-melody (concat twinkle alouette) 16) 120 jm.JMC/FLUTE))

(def plainsong (load-midi "midi/plainsong.mid"))

(View/notate
 (make-score-from-notes
  (markov-melody (concat twinkle alouette plainsong) 32) 120 jm.JMC/FLUTE))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; and, as a bonus, some poetry

(def wandering-aengus "
I went out to the hazel wood,
Because a fire was in my head,
And cut and peeled a hazel wand,
And hooked a berry to a thread;
And when white moths were on the wing,
And moth-like stars were flickering out,
I dropped the berry in a stream
And caught a little silver trout.

When I had laid it on the floor
I went to blow the fire a-flame,
But something rustled on the floor,
And someone called me by my name:
It had become a glimmering girl
With apple blossom in her hair
Who called me by my name and ran
And faded through the brightening air.

Though I am old with wandering
Through hollow lands and hilly lands,
I will find out where she has gone,
And kiss her lips and take her hands;
And walk among long dappled grass,
And pluck till time and times are done,
The silver apples of the moon,
The golden apples of the sun.") ;; -- Yeats

(markov-chain
 (transitions
  (filter #(and (not (nil? %)) (> (.length %) 0))
          (.split (.toLowerCase wandering-aengus) "[^\\w]"))) 10)
;; => ["wandering" "through" "hollow" "lands" "i" "had" "become" "a"
;; "hazel" "wand"]
;; => ["girl" "with" "apple" "blossom" "in" "her" "hands" "and"
;; "hilly" "lands"]
