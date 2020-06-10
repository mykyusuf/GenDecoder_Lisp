(ns .main)

(use 'clojure.java.io)
(require '[clojure.string :as str] )

(defn oku [map2]

  (slurp "document1.txt")
  (slurp "dictionary1.txt")

  (def a (seq '()))
  (def b [])
  (def testy [])
  (def control true)

  (with-open [rdr (reader "document1.txt")]
    (doseq [line (line-seq rdr)]

      (doseq [x (str/split line #"\s+")]

        (def size (count x))
        (def s1 (range size))
        (def tempmap (zipmap s1 (partition 1 1 x)))

        (loop [i 0]
          (when (< i size)

            (def son (seq ((symbol (apply str (get tempmap i))) map2)))
            (def b (concat b son))

            (recur (+ i 1)))
          )


        (with-open [rdr2 (reader "dictionary1.txt")]

          (def tru 0)
          (def yok 0)

          (doseq [line2 (line-seq rdr2)]

            (doseq [y (str/split line2 #"\s+")]


              (def testy (conj testy y))
              (def size2 (count y))

              (def t(compare x y))

              (if (= t 0)

                (do (def tru 1))

                )
              )
            )


          (def b_size (count b))
          (loop [i 0]
            (when (< i b_size )

              (def ccc (some (partial = (apply str b))  (seq testy) ))
              (if  (= ccc nil)
                (def control false)
                ())
              ;(println ccc)
              ;(println (apply str b))
              ;(println testy)
              ))



          )

        (def b [])

        )

      ))

      control
  )

(defn alphabet [new old input i]

  (def aa false)
  (cond
    (empty? input) ( do  (def m (zipmap ['a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p,'q,'r,'s,'t,'u,'v,'w,'x,'y,'z] old)) (def aa (oku m)) (println "taraniyor ...")
                         (if aa (do (println "buldu!!!") (print "kullanilan map") (println m) (System/exit 0) ) () ))
    (< i 0) ( )
    :else

          (alphabet
            (alphabet
              new
              (conj old (nth input i))
              (remove (fn [x] (= x (nth input i))) input)
              (dec
                (count
                  (remove
                    (fn [x] (= x (nth input i)))
                    input))))
            old
            input
            (dec i))

    )

  )

(defn permutations [a-set]
  (alphabet (seq '()) (seq '()) (seq a-set) (dec (count a-set))))


(def p(permutations ["a","b","c","d","e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"]))
;(def m(zipmap ['a,'b,'c,'d,'e,'f,'g,'h,'i,'j,'k,'l,'m,'n,'o,'p,'q,'r,'s,'t,'u,'v,'w,'x,'y,'z] ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]))
;(println m)
;(oku m)

