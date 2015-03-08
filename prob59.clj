;; Each character on a computer is assigned a unique code and the
;; preferred standard is ASCII (American Standard Code for Information
;; Interchange). For example, uppercase A = 65, asterisk (*) = 42, and
;; lowercase k = 107.

;; A modern encryption method is to take a text file, convert the bytes
;; to ASCII, then XOR each byte with a given value, taken from a secret
;; key. The advantage with the XOR function is that using the same
;; encryption key on the cipher text, restores the plain text; for
;; example, 65 XOR 42 = 107, then 107 XOR 42 = 65.

;; For unbreakable encryption, the key is the same length as the plain
;; text message, and the key is made up of random bytes. The user would
;; keep the encrypted message and the encryption key in different
;; locations, and without both "halves", it is impossible to decrypt the
;; message.

;; Unfortunately, this method is impractical for most users, so the
;; modified method is to use a password as a key. If the password is
;; shorter than the message, which is likely, the key is repeated
;; cyclically throughout the message. The balance for this method is
;; using a sufficiently long password key for security, but short enough
;; to be memorable.

;; Your task has been made easy, as the encryption key consists of three
;; lower case characters. Using cipher.txt (right click and 'Save
;; Link/Target As...'), a file containing the encrypted ASCII codes, and
;; the knowledge that the plain text must contain common English words,
;; decrypt the message and find the sum of the ASCII values in the
;; original text.

(require '[clojure.string :as string])

(def ciphers (map read-string
                  (string/split (slurp "p059_cipher.txt") #",")))

(def as (take-nth 3 ciphers))
(def bs (take-nth 3 (next ciphers)))
(def cs (take-nth 3 (nnext ciphers)))

(def as-freq (sort-by val > (frequencies as)))
(def bs-freq (sort-by val > (frequencies bs)))
(def cs-freq (sort-by val > (frequencies cs)))

(defn decrypt [cint key]
  (char (bit-xor cint key)))

(defn decrypt-text [a b c]
  (let [as-decrypted (map decrypt as (repeat a))
        bs-decrypted (map decrypt bs (repeat b))
        cs-decrypted (map decrypt cs (repeat c))
        decrypted (concat (interleave as-decrypted
                              bs-decrypted
                              cs-decrypted)
                          [(last as-decrypted)])]
    (apply str decrypted)))

(defn prob59 []
  (let [as-space (ffirst as-freq)
        bs-space (ffirst bs-freq)
        cs-space (ffirst cs-freq)
        akey (bit-xor (int \space) as-space)
        bkey (bit-xor (int \space) bs-space)
        ckey (bit-xor (int \space) cs-space)
        decrypted (decrypt-text akey bkey ckey)]
    (println decrypted)
    (apply + (map int decrypted))))
