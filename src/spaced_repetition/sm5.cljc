(ns spaced-repetition.sm5)

;; controls the rate at which EF is increased or decreased; must be a
;; number between 0 and 1 (the greater it is the faster the changes of
;; the OF matrix)."
(def learn-fraction 0.5)
(def min-ef 1.3)
(def default-ef 2.5)

(defn interpolate-with-ef [ef low high]
  (let [t (/ (- ef min-ef) (- default-ef min-ef))]
    (+ (* high t) (* low (- 1 t)))))

(defn initial-optimal-factor [n ef]
  (cond
    (= 1 n) (interpolate-with-ef ef 5 8)
    (= 2 n) (interpolate-with-ef ef 13 21)
    :else (- ef 0.1)))

(defn propagate [{[src-ef src-of] :last-set}
                 dest-n dest-ef]
  (when (and (> dest-n 2) src-ef src-of)
    (* src-of (/ dest-ef src-ef))))

(defn ef-bucket [ef]
  (double (/ (Math/round (double (* ef 10)))
             10)))

(defn ofm-ref [ofm n ef]
  (let [ef (ef-bucket ef)]
    (or (get-in ofm [:of [n ef]])
        (propagate ofm n ef)
        (initial-optimal-factor n ef))))

(defn ofm-set [ofm n ef of]
  (let [ef (ef-bucket ef)]
    (cond-> (assoc-in ofm [:of [n ef]] of)
      (> n 2) (assoc :last-seen-rest [ef of]))))

(defn inter-repetition-interval
  [n ef ofm last-interval]
  (let [of (ofm-ref ofm n ef)]
    (if (<= n 2)
      of
      (* of last-interval))))

(defn modify-e-factor [ef quality]
  (max min-ef
       (let [x5-q (partial * (- 5 quality))]
         (+ ef (- 0.1 (x5-q (+ 0.08 (x5-q 0.02))))))))

(defn calculate-new-optimal-factor
  "Takes the following arguments:

   - interval-used, the last interval used for the item in question

   - quality of the repetition response;

   - used-of, the optimal factor used in calculation of the last interval used
     for the item in question;

   - old-of, the previous value of the OF entry corresponding to the relevant
     repetition number and the E-Factor of the item;

   - fraction, a number belonging to the range (0,1) determining the rate of
     modifications (the greater it is the faster the changes of the OF matrix).

  Returns the newly calculated value of the considered entry of the OF matrix.

  Intermediate values involved:

  - mod5, value proposed for the modifier in case of q=5;

  - mod2, value proposed for the modifier in case of q=2;

  - modifier,number determining how many times the OF value will increase or
    decrease."
  [interval-used quality used-of old-of fraction]
  (let [mod5 (max 1.05 (/ (inc interval-used) interval-used))
        mod2 (min 0.75 (/ (dec interval-used) interval-used))
        modifier (max 0.05 (if (> quality 4)
                             (inc (* (dec mod5) (- quality 4)))
                             (- 1 (* (/ (- 1 mod2) 2) (- 4 quality)))))
        of (let [new-of (* modifier used-of)]
             (if (or (and (> quality 4) (< new-of old-of))
                     (and (< quality 4) (> new-of old-of)))
               old-of
               new-of))]
    (max 1.2 (+ (* of fraction) (* old-of (- 1 fraction))))))

(defn dispersal-m []
  (let [a 0.04652
        b 0.09210
        p (- (rand) 0.5)]
    (* (Math/signum p)
       (/ (Math/log (- 1 (* (/ b a) (Math/abs p))))
          (- b)))))

(defn near-optimal-interval [previously-used-interval optimal-interval]
  (Math/round
   (+ previously-used-interval
      (* (- optimal-interval previously-used-interval)
         (+ 1 (/ (dispersal-m) 100))))))

(defn next-state
  "Calculate the next time an item should be reviewed by a user.

  quality [user-state [item-state]] -> {:new-interval ...
                                        :new-user-state ...
                                        :new-item-state ...}

  [XXX: find link explaining quality of recall.]

  Users and items (questions to learn) are represented by opaque data
  structures. You pass these structures to this function and get new versions of
  them returned in a mapping. The first time a given item is reviewed by a given
  user you can omit the `item-state', and the first time a given user reviews
  any item at all you can omit the `user state'.

  In addition to rescheduling the question for the new-interval, any questions
  in this session with quality lower than 3 should be reviewed cyclically until
  quality becomes 4 or 5."
  ([quality] (next-state quality nil))
  ([quality of-matrix] (next-state quality of-matrix [0 default-ef nil nil]))
  ([quality of-matrix [last-n last-ef last-interval used-of]]
   (assert (>= last-n 0))
   (assert (<= 0 quality 5))
   (assert (or (not last-interval) (> last-interval 0)))
   (let [new-n (if (< quality 3) 1 (+ last-n 1))
         new-ef (modify-e-factor last-ef quality)
         old-of (ofm-ref of-matrix last-n last-ef)
         new-of (when last-interval
                  (calculate-new-optimal-factor
                   last-interval
                   quality
                   used-of
                   old-of
                   learn-fraction))
         new-ofm (if new-of
                   (ofm-set of-matrix last-n last-ef new-of)
                   of-matrix)
         new-interval (near-optimal-interval
                       (or last-interval 0)
                       (inter-repetition-interval new-n new-ef new-ofm last-interval))]
     {:new-interval new-interval
      :new-user-state new-ofm
      :new-item-state [new-n new-ef new-interval (ofm-ref new-ofm new-n new-ef)]})))
