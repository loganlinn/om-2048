(ns om-2048.core
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop]])
  (:require
   [clojure.string :as str]
   [cljs.core.async :as async :refer [<!]]
   [dommy.core :as dommy]
   [om.core :as om]
   [om-tools.core :refer-macros [defcomponentk]]
   [om-tools.dom :as dom :include-macros true]))

(enable-console-print!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic functions

(defn pad
  "Returns a lazy seq of size n containing values of coll, padded as necessary with v"
  [n v coll]
  (take n (concat coll (repeat v))))

(defn transposev
  "Transposes 2d vector"
  [vs]
  (apply mapv vector vs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Game logic

(defprotocol IBoardTile
  (tile-value [this])
  (tile-id [this])
  (tile-parents [this]))

(defprotocol IMergeableTile
  (mergeable? [this other])
  (merge-tile [this other]))

(defn- gen-tile-id [] (str (gensym "tile")))

(deftype BoardTile [id value parents]
  IBoardTile
  (tile-value [_] value)
  (tile-id [_] id)
  (tile-parents [_] parents)
  IMergeableTile
  (mergeable? [_ other]
    (when (satisfies? IBoardTile other)
      (= value (tile-value other))))
  (merge-tile [this other]
    (BoardTile. (gen-tile-id) (+ value (tile-value other)) [this other])))

(extend-type nil
  IBoardTile
  (tile-value [_] nil)
  (tile-id [_] nil)
  (tile-parents [_] nil))

(defn create-tile
  ([value]
     (create-tile value nil))
  ([value parents]
     (BoardTile. (gen-tile-id) value parents)))

(defn walk-board
  "Returns a sequences of all non-nil results of (f row col cell-value) for all board cells"
  [f board]
  (->> board
       (map-indexed
        (fn [row cs]
          (keep-indexed
           (fn [col cell] (f row col cell))
           cs)))
       (apply concat)))

(defn empty-cells
  "Returns a sequence of [x y] vectors of empty cells"
  [board]
  (walk-board (fn [row col cell] (when-not cell [row col])) board))

(defn tiles-by-position
  [board]
  (walk-board (fn [row col cell] (when cell [[row col] cell])) board))

(defn rand-value [] (if (< (rand) 0.9) 2 4))

(defn add-rand-tile
  "Returns new board after adding new tile in random open cell, if any, otherwise board."
  [board]
  (if-let [coords (rand-nth (empty-cells board))]
    (assoc-in board coords (create-tile (rand-value)))
    board))

(defn empty-board
  "Returns an empty board of given dimensions"
  [width height]
  (vec (repeat height (vec (repeat width nil)))))

(defn create-board
  ([[w h]] (empty-board w h))
  ([size spec]
     (reduce
      (fn [board [coord value]]
        (assoc-in board coord (create-tile value)))
      (create-board size)
      spec)))

(defn rand-board
  ([size] (rand-board size 2))
  ([size num-vals]
     (reduce
      (fn [board _] (add-rand-tile board))
      (create-board size)
      (range num-vals))))

(defn board-size [board]
  [(count board) (count (first board))])

(defn merge-row
  "Returns new row after merging adjacent mergeable tiles
   Ex (merge-row [2 2 nil 4]) => [4 4 nil nil]"
  [row]
  (let [n (count row)
        in (filterv identity row)
        end (dec (count in))]
    (loop [i 0, out []]
      (cond
       (> i end) (pad n nil out)
       (= i end) (pad n nil (conj out (get in i)))
       :else (let [m (get in i)
                   n (get in (inc i))]
               (if (mergeable? m n)
                 (recur (+ i 2) (conj out (merge-tile m n)))
                 (recur (+ i 1) (conj out m))))))))

(defn merge-left [board]
  (mapv (comp vec merge-row) board))

(defn merge-right [board]
  (mapv (comp vec reverse merge-row reverse) board))

(defn merge-up [board]
  (-> board transposev merge-left transposev))

(defn merge-down [board]
  (-> board transposev merge-right transposev))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keyboard Task

(defn which-key
  "Returns keycode for keyboard event, e"
  [e] (.-which e))

(def +keymap+
  {37 :left, 38 :up, 39 :right, 40 :down})

(def +keybinds+
  {:left merge-left, :up merge-up, :right merge-right, :down merge-down})

(defn step-board [f board]
  (let [next-board (f board)]
    (if (= board next-board)
      board
      (add-rand-tile next-board))))

(defn keyboard-task
  "Starts a go-loop that takes keyboard events from key-chan
   and maps to merge-fn in keybind and applies it to board cursor"
  [owner board key-chan]
  (let [board-fns (async/map< #(-> % which-key +keymap+ +keybinds+ (or identity)) key-chan)]
    (go-loop []
      (when-let [merge-fn (<! board-fns)]
        (om/transact! board #(step-board merge-fn %))
        (recur)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Components & HTML

(defn grid-html [board]
  (let [[w h] (board-size board)]
    (dom/div
     {:class "grid-container"}
     (for [_ (range h)]
       (dom/div
        {:class "grid-row"}
        (for [_ (range w)]
          (dom/div
           {:class "grid-cell"})))))))

(defn tile-html [row col tile]
  (let [value (tile-value tile)
        parents? ^boolean (tile-parents tile)]
    (dom/div
     ;; TODO cleanup
     {:class (str "tile tile-" value " tile-position-" (inc col) "-" (inc row)
                  (if ^boolean (tile-parents tile) " tile-merged" " tile-new")
                  (when (> (tile-value tile) 2048) " tile-super"))
      :key (tile-id tile)}
     (dom/div {:class "tile-inner"} value))))

(defn tiles-html [board]
  (dom/div
   {:class "tile-container"}
   (->> board
        tiles-by-position
        (into (sorted-map))
        (map (fn [[[x y] tile]]
               (if-let [parents (tile-parents tile)]
                 [(tile-html x y (second parents))
                  (tile-html x y (first parents))
                  (tile-html x y tile)]
                 (tile-html x y tile)))))))

(defcomponentk game-board
  [[:data board]
   [:shared keydown-mult]
   owner]
  (will-mount [_]
    (let [keydown-chan (async/chan (async/sliding-buffer 1))]
      (async/tap keydown-mult keydown-chan)
      (om/set-state! owner :keydown-chan keydown-chan)
      (keyboard-task owner board keydown-chan)))
  (will-unmount [_]
    (let [keydown-chan (om/get-state owner :keydown-chan)]
      (async/untap keydown-mult keydown-chan)
      (async/close! keydown-chan)))
  (render [_]
    (dom/div
     {:class "game-container"}
     (grid-html board)
     (tiles-html board))))

(defcomponentk app
  [data owner]
  (init-state [_]
    {:show-board? true})
  (render-state [_ {:keys [show-board?]}]
    (dom/div
     (dom/button {:on-click #(om/set-state! owner :show-board? (not show-board?))}
                 (if show-board? "Unmount" "Remount"))
     (dom/button {:on-click #(do (.. % -target blur)
                                 (om/transact! data :board
                                               (fn [board]
                                                 (rand-board (board-size board)))))}
                 "New Game")
     (when show-board?
       (om/build game-board data)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def keydown-chan (async/chan))

(dommy/listen! js/window :keydown
               #(when (= "BODY" (.. % -target -tagName))
                  (async/put! keydown-chan %)))

(om/root app
         {:board (rand-board [4 4])}
         {:target (.getElementById js/document "app")
          :shared {:keydown-mult (async/mult keydown-chan)}})
