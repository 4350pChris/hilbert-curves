(ns hilbert-curves.controls
  (:require [quil.core :as q]))

(defn reset-state [state]
  (assoc state
         :mode nil
         :text-buffer ""
         :action nil))

(defn flush-buffer
  "Flushes the text buffer, returning new state."
  [{:keys [action mode text-buffer] :as state}]
  (when action
    (try
      ; set key saved in :mode to result of action
      (reset-state (assoc state mode (action text-buffer)))
      (catch Exception e
        (println (.getMessage e))))))

(defn make-action
  "Helper to make actions that take no params and do not return state not break the app."
  [f]
  (fn [state]
    (f) ; invoke f, which needs no params
    state)) ; return state as it was


(defn controls
  [state]
  [{:label "Loop (space): Pause / Resume"
    :action (make-action #(if (q/looping?) (q/no-loop) (q/start-loop)))
    :mode :none
    :key :space}
   {:label (str "Order (o): " (:order state))
    :action #(Integer/parseInt %)
    :mode :order
    :key :o}
   {:label (str "Increment (c) - 0 disables: " (:counter-increments state))
    :action #(Integer/parseInt %)
    :mode :counter-increments
    :key :c}
   {:label "Load Image (i)"
    :mode :image
    :action #(q/load-image %)
    :key :i}
   {:label (str (if (:show-grid state) "Hide" "Show") " Grid (g)")
    :mode :none
    :action #(assoc % :show-grid (not (:show-grid %)))
    :key :g}])

(defn is-key?
  "Helper to check if a key is pressed."
  [unicode raw-key]
  (let [key (->> raw-key
                 int
                 (format "%04x")
                 str)]
    (= key unicode)))

(def is-backspace? (partial is-key? "0008"))
(def is-enter? (partial is-key? "000a"))

(defn append-to-text-buffer [raw-key text-buffer]
  (if (is-backspace? raw-key)
    ; remove last char from text buffer
    (subs text-buffer 0 (Math/max 0 (dec (count text-buffer))))
    ; add input to text buffer
    (str text-buffer raw-key)))

(defn handle-input-mode-press
  [state
   {raw-key :raw-key}]
  (if (is-enter? raw-key)
    (flush-buffer state) ; if enter was pressed flush buffer and return new state
    (assoc state :text-buffer (append-to-text-buffer raw-key (:text-buffer state)))))

(defn framerate-mod-2?
  "Helper to make the cursor blink around every second."
  []
  (-> (q/frame-count)
      (/ (q/current-frame-rate))
      (int)
      (mod 2)
      (= 0)))

(defn control-offset-x
  "Helper to calculate X offset for controls."
  [idx]
  (-> idx
      (/ 3)
      int
      (* 300)
      (+ 10)))

(defn control-offset-y
  "Helper to calculate Y offset for controls."
  [idx]
  (-> idx
      (mod 3)
      (* 20)
      (+ 40)))

(defn build-label [buffer label]
  (str label ": " buffer (when (framerate-mod-2?) "|")))

(defn show-controls [state]
  (q/text-size 14)
  ; show input when active 
  (some->> (controls state)
           (filter #(= (:mode %) (:mode state)))
           first
           :label
           (build-label (:text-buffer state))
           vector
           (run! #(q/text % 10 20)))
  ; show controls
  (->> (controls state)
       (map-indexed vector)
       (run! (fn [[idx control]]
               (q/text (:label control) (control-offset-x idx) (control-offset-y idx))))))

(defn handle-nil-mode-press [state {key :key}]
  (when-let [{action :action mode :mode} (->> (controls state)
                                              (filter #(= (:key %) key))
                                              first)]
    (if (= mode :none)
        ; if the control has no mode, just invoke the action and return state
      (action state)
        ; if the control has a mode, enter input mode and return state
      (assoc state :mode mode :action action))))

(defn key-press
  [state event]
   ; make sure the UI is updated to show input
  (when (not (q/looping?))
    (q/redraw))
  (merge state
         (if (nil? (:mode state))
           ; if we're not in input mode see if the key maps to a control
           (handle-nil-mode-press state event)
           ; if we're in input mode handle the key press
           (handle-input-mode-press state event))))
