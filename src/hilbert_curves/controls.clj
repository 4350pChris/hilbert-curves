(ns hilbert-curves.controls
  (:require [quil.core :as q]))

(def input-mode (atom :none))

; holds input value when in input mode, will be flushed an written to state when :enter is pressed
(def text-buffer (atom ""))

(defn flush-buffer
  "Flushes the text buffer, returning new state."
  [state]
  (let [mode @input-mode]
    (reset! input-mode :none)
    (condp = mode
      :counter (do
                 (println "counter set to" @text-buffer)
                 (let [text @text-buffer]
                   (reset! text-buffer "")
                   (try
                     (assoc state :counter-increments (Integer/parseInt text))
                     (catch Exception e
                       (println "Could not parse int from " text " - " (.getMessage e))
                       state))))
      state)))

(defn make-action
  "Helper to make actions that take no params and do not return state not break the app."
  [f]
  (fn [state]
    (f) ; invoke f, which needs no params
    state)) ; return state as it was


(def controls (atom [{:label "Space: Pause / Resume"
                      :action (make-action #(if (q/looping?) (q/no-loop) (q/start-loop)))
                      :key :space}
                     {:label "C: Set counter"
                      ; set input-mode to :counter
                      :action (make-action #(reset! input-mode :counter))
                      :key :c}]))

(defn is-enter?
  "I just couldn't get the key code for enter to work, so I'm using this workaround."
  [raw-key]
  (= (str (format "%04x" (int raw-key))) "000a"))

(defn handle-input-mode-press [state event]
  (if (is-enter? (:raw-key event))
    (flush-buffer state) ; if enter was pressed flush buffer and return new state
    (do (swap! text-buffer #(str % (:raw-key event))) ; add input to text buffer
        state)))

(defn show-controls []
  (q/text-size 14)
  (when (not (= @input-mode :none))
    (q/text (str "Input (Enter to confirm): " @text-buffer) 10 20))
  (doseq [[idx item] (map-indexed (fn [idx item] [idx item]) @controls)]
    (q/text (:label item) (+ 10 (* 100 (int (/ idx 4)))) (+ 25 (* 20 (mod (inc idx) 4))))))

(defn key-press [state event]
  (let [key (:key event)]
    ; if we're not in input mode see if the key maps to a control
    (if (= @input-mode :none)
      (if-let [control (first (filter #(= (:key %) key) @controls))]
        ((:action control) state) ; invoke action
        state) ; just return state when no mapped key was pressed
      (handle-input-mode-press state event))))
