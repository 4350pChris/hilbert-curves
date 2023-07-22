(ns hilbert-curves.controls
  (:require [quil.core :as q]))

; keeps track of current input mode, has to respond to a key in state
(def input-mode (atom :none))

; holds handler that is invoked when text buffer is flushed
(def current-handler (atom nil))

; holds input value when in input mode, will be flushed an written to state when :enter is pressed
(def text-buffer (atom ""))

(defn flush-buffer
  "Flushes the text buffer, returning new state."
  [state]
  (let [mode @input-mode
        handler @current-handler
        text @text-buffer]
    (reset! input-mode :none)
    (reset! current-handler nil)
    (reset! text-buffer "")
    (try
      (if (nil? handler)
        state
        (assoc state mode (handler text)))
      (catch Exception e
        (println (.getMessage e))
        state))))

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
    :mode nil
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
    :mode nil
    :action #(assoc % :show-grid (not (:show-grid %)))
    :key :g}])

(defn is-key?
  [unicode raw-key]
  (= (str (format "%04x" (int raw-key))) unicode))

(def is-backspace? (partial is-key? "0008"))
(def is-enter? (partial is-key? "000a"))

(defn handle-input-mode-press
  [state
   {raw-key :raw-key}]
  (if (is-enter? raw-key)
    (flush-buffer state) ; if enter was pressed flush buffer and return new state
    (do
      (swap! text-buffer #(if (is-backspace? raw-key)
                            ; remove last char from text buffer
                            (subs % 0 (Math/max (dec (count %)) 0))
                            (str % raw-key))) ; add input to text buffer
      state)))

(defn framerate-mod-2?
  "Helper to make the cursor blink around every second."
  []
  (= 0 (mod (int (/ (q/frame-count) (q/current-frame-rate))) 2)))

(defn control-offset-x
  "Helper to calculate X offset for controls."
  [idx]
  (+ 10 (* 300 (int (/ idx 3)))))

(defn control-offset-y
  "Helper to calculate Y offset for controls."
  [idx]
  (+ 40 (* 20 (mod idx 3))))

(defn show-controls [state]
  (q/text-size 14)
  ; show input when active
  (when (not (= @input-mode :none))
    (let [label (some->> (controls state)
                         (filter #(= (:mode %) @input-mode))
                         first
                         :label)]
      (q/text (str label ": " @text-buffer (if (framerate-mod-2?) "|" "")) 10 20)))
  ; show controls
  (->> (controls state)
       (map-indexed vector)
       (run! (fn [[idx control]]
               (q/text (:label control) (control-offset-x idx) (control-offset-y idx))))))

(defn handle-none-mode-press [state {key :key}]
  (if-let [control (first (filter #(= (:key %) key) (controls state)))]
    (if (nil? (:mode control))
        ; if the control has no mode, just invoke the action and return state
      ((:action control) state)
        ; if the control has a mode, enter input mode and return state
      (do (reset! input-mode (:mode control))
          (reset! current-handler (:action control))
          state))
    state)) ; just return state when no mapped key was pressed

(defn key-press
  [state event]
   ; make sure the UI is updated to show input
  (when (not (q/looping?))
    (q/redraw))
    ; if we're not in input mode see if the key maps to a control
  (condp = @input-mode
    :none (handle-none-mode-press state event)
    (handle-input-mode-press state event)))
