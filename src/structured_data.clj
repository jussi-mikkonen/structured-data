(ns structured-data)

(defn do-a-thing [x]
  (let [xx (+ x x)]
    (Math/pow xx xx)))

(defn spiff [v]
  (let [first (get v 0)
        third (get v 2)]
    (+ first third)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (let [[f s t] v]
    (+ f t)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[xb yb] [xt yt]]]
  (Math/abs (- xt xb)))

(defn height [[[xb yb] [xt yt]]]
  (Math/abs (- yt yb)))

(defn square? [rectangle]
  (let [w (width rectangle)
        h (height rectangle)]
    (== h w)))

(defn area [rectangle]
  (let [w (width rectangle)
        h (height rectangle)]
    (* h w)))


(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp, yp] point
        upperbound (and (<= xp x2) (<= yp y2))
        lowerbound (and (>= xp x1) (>= yp y1))]
    (and upperbound lowerbound)))

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (< 1 (author-count book)))

(defn add-author [book new-author]
  (let [authors (:authors book)]
    (assoc book :authors (conj authors new-author))))

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (map second collection))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (cond
      (contains? a-set elem) (disj a-set elem)
      :else (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (let [len (count a-seq)
        setlen (count (set a-seq))]
    (not (= len setlen))))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
  (set (map :name (authors books))))

(defn author->string [author]
  (let [hasbyear (contains? author :birth-year)
        hasdyear (contains? author :death-year)
        byear (:birth-year author)
        dyear (:death-year author)]
    (cond
        hasdyear (str (:name author) " (" byear " - " dyear ")")
        hasbyear (str (:name author) " (" byear " - " ")")
        :else   (str (:name author)))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
  (let [bookcount (count books)]
    (cond
      (= 0 bookcount) "No books."
      (= 1 bookcount) (str "1 book. " (book->string (first books)) ".")
      :else (str bookcount " books. " (apply str (interpose ". " (map book->string books))) "."))))

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books))

(defn author-by-name [name authors]
  (let [auth (filter (fn [a] (= (:name a) name)) authors)]
    (first auth)))
    ;(cond
    ;    (= 0 (count auth)) nil
    ;    :else (first auth))))

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [livingAuthors (living-authors (:authors book))]
    (not (empty? livingAuthors))))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
