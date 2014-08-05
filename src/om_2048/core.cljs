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
    (and (satisfies? IBoardTile other)
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

(defn empty-cells
  "Returns a sequence of [x y] vectors of empty cells"
  [{:keys [width height tiles]}]
  (for [x (range width)
        y (range height)
        :let [position [x y]]
        :when (not (contains? tiles position))]
    position))

(defn rand-value [] (if (< (rand) 0.9) 2 4))

(defn set-tiles [board tiles] (assoc board :tiles tiles))

(defn clear-tiles [board] (set-tiles board {}))

(defn set-tile
  "Returns board after adding tile at specified [x y] postion"
  [board position tile]
  (assoc-in board [:tiles position] tile))

(defn add-rand-tile
  "Returns board after adding new tile in random open cell, if any, otherwise board."
  [board]
  (if-let [position (rand-nth (empty-cells board))]
    (set-tile board position (create-tile (rand-value)))
    board))

(defn prefill-board [board]
  (reduce
   (fn [board _] (add-rand-tile board))
   board
   (range (:prefill board 0))))

(defn reset-board
  [board]
  (-> board clear-tiles prefill-board))

(def cell-grid
  "A memoized function that returns a 2d vector of specified dimensions filled
  with nils"
  (memoize
   (fn [width height]
     (vec (repeat height (vec (repeat width nil)))))))

(defn expand-cells
  "Returns board cells in 2d vector format"
  [{:keys [width height tiles]}]
  (reduce-kv assoc-in (cell-grid width height) tiles))

(defn collapse-cells
  "Returns non-empty board cells in 1d map format"
  [cells]
  (reduce-kv
   (fn [tiles x row]
     (reduce-kv
      (fn [tiles y cell]
        (if cell
          (assoc tiles [x y] cell)
          tiles))
      tiles row))
   {} cells))

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

(defn mergefn
  "Returns a single-argument function that is passed a board and returns a board
   with tiles updated after applying merge function, f.
   The input and output of merge function is 2d representation of board's tiles."
  [f]
  (fn [board]
    (->> board
         expand-cells
         f
         collapse-cells
         (set-tiles board))))

(defn merge-cells-left [cells]
  (mapv (comp vec merge-row) cells))

(defn merge-cells-right [cells]
  (mapv (comp vec reverse merge-row reverse) cells))

(defn merge-cells-up [cells]
  (-> cells transposev merge-cells-left transposev))

(defn merge-cells-down [cells]
  (-> cells transposev merge-cells-right transposev))

(def merge-left (mergefn merge-cells-left))
(def merge-right (mergefn merge-cells-right))
(def merge-up (mergefn merge-cells-up))
(def merge-down (mergefn merge-cells-down))

(defn update-board!
  "Performs a board action given a board cursor and update-fn, a single argument function
  that takes current board and returns next board. Updates board cursor and adds
  any new tiles."
  [board update-fn]
  (let [curr-board @board
        next-board (update-fn curr-board)]
    (when (not= curr-board next-board)
      (om/update! board (add-rand-tile next-board)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UI Behavior

(defn which-key
  "Returns keycode for keyboard event, e"
  [e] (.-which e))

(def +keymap+
  {37 :left, 38 :up, 39 :right, 40 :down})

(def +keybinds+
  {:left merge-left, :up merge-up, :right merge-right, :down merge-down})

(defn keybind-task
  "Starts a go-loop that takes keyboard events from key-chan
   and maps to merge-fn in keybind and applies it to board cursor"
  [owner board key-chan]
  (go-loop []
    (when-let [key-event (<! key-chan)]
      (some->> key-event
               which-key
               +keymap+
               +keybinds+
               (update-board! board))
      (recur))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Components & HTML

(defn grid-html [{:keys [width height]}]
  (dom/div
   {:class "grid-container"}
   (for [_ (range height)]
     (dom/div
      {:class "grid-row"}
      (for [_ (range width)]
        (dom/div
         {:class "grid-cell"}))))))

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

(defn tiles-html [{:keys [tiles]}]
  (dom/div
   {:class "tile-container"}
   (->> tiles
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
      (keybind-task owner board keydown-chan)))
  (will-unmount [_]
    (let [keydown-chan (om/get-state owner :keydown-chan)]
      (async/untap keydown-mult keydown-chan)
      (async/close! keydown-chan)))
  (render [_]
    (let [{:keys [tiles]} board]
      (dom/div
       {:class "game-container"}
       (grid-html board)
       (tiles-html board)))))

(defcomponentk app
  [data owner]
  (init-state [_]
    {:show-board? true})
  (render-state [_ {:keys [show-board?]}]
    (dom/div
     (dom/button {:on-click #(om/set-state! owner :show-board? (not show-board?))}
                 (if show-board? "Unmount" "Remount"))
     (dom/button {:on-click #(do (.. % -target blur)
                                 (om/transact! data :board reset-board))}
                 "New Game")
     (when show-board?
       (om/build game-board data)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def +keydown-chan+ (async/chan))

(def +proto-board+ {:width 4
                    :height 4
                    :prefill 2
                    :tiles {}})

(dommy/listen! js/window :keydown
               #(when (= "BODY" (.. % -target -tagName))
                  (async/put! +keydown-chan+ %)))

(om/root app
         {:board (reset-board +proto-board+)}
         {:target (.getElementById js/document "app")
          :shared {:keydown-mult (async/mult +keydown-chan+)}})
