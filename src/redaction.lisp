(in-package :agento11y-cl)

;;; Secret redaction engine for exported generation content.
;;;
;;; Patterns hand-curated from Gitleaks (https://github.com/gitleaks/gitleaks).
;;; The regex text below is the text of the shared table in agento11y's
;;; redaction/patterns.json, which the Go, Python, JavaScript and .NET tables
;;; are generated from. Two tiers:
;;;   - Tier 1: 22 definite secret formats, plus the email pattern the caller
;;;     can switch off. Applied by both redact-full and redact-light.
;;;   - Tier 2: 3 key/value heuristics, applied by redact-full only. They
;;;     replace the value after PASSWORD, SECRET, TOKEN, CREDENTIAL, KEY,
;;;     API_KEY, PRIVATE_KEY or ACCESS_KEY where "=" or ":" follows the word,
;;;     so a sentence using one of those words loses the word after it. That is
;;;     why prose stays on tier 1.
;;;
;;; Two things differ from the shared table by design. \b is rewritten to an
;;; ASCII lookaround (see +ascii-word-boundary+), and the tier-2 replacement
;;; templates are spelled out here rather than derived from the upstream
;;; keepGroups lists.

(defstruct (secret-redactor (:constructor %make-secret-redactor (include-emails)))
  (include-emails t :read-only t))

(defun make-secret-redactor (&key (include-emails t))
  "Create a secret redactor. INCLUDE-EMAILS toggles the email pattern."
  (%make-secret-redactor include-emails))

(defparameter +ascii-word-boundary+
  "(?:(?<=[A-Za-z0-9_])(?![A-Za-z0-9_])|(?<![A-Za-z0-9_])(?=[A-Za-z0-9_]))"
  "Zero-width stand-in for \\b, restricted to ASCII word characters.
cl-ppcre decides \\b with ALPHANUMERICP, which SBCL answers for every Unicode
letter, so a token pressed against a CJK character has no boundary in front of
it and stays in clear text. The other engines read \\b as ASCII: Go and
JavaScript by default, Python under re.ASCII, .NET under RegexOptions.ECMAScript.
Both branches are zero-width, and neither adds a capturing group: the tier-1
scanner reads a register index back as a pattern id, and a group here would
shift that mapping.")

(defparameter +ascii-word-start+ "(?<![A-Za-z0-9_])"
  "Cheaper form of +ascii-word-boundary+ for a \\b whose right side is an ASCII
word character. The boundary then holds exactly when the left side is not one,
which is a single lookbehind instead of an alternation of two.")

(defun %ascii-word-char-p (char)
  "True for the ASCII characters \\b counts as word characters."
  (or (char<= #\a char #\z)
      (char<= #\A char #\Z)
      (char<= #\0 char #\9)
      (char= char #\_)))

(defun %ascii-boundaries (regex)
  "Rewrite every \\b in REGEX to a zero-width ASCII lookaround.
Applied when a pattern is compiled, so the tables below keep the upstream regex
text and stay diffable against patterns.json.

A \\b followed by a literal word character takes +ascii-word-start+; every other
one takes the two-branch +ascii-word-boundary+, because what precedes it is not
readable off the regex text. An escaped character is copied whole, so the b in
\\\\b is a literal. The rewrite does not look inside character classes, where \\b
means backspace; no pattern in the tables puts one there, and one added would
fail to compile at load rather than match the wrong thing."
  (let ((out (make-string-output-stream))
        (index 0)
        (end (length regex)))
    (loop while (< index end)
          do (let ((char (char regex index))
                   (next (when (< (1+ index) end) (char regex (1+ index)))))
               (cond
                 ((and (char= char #\\) (eql next #\b))
                  (let ((after (when (< (+ index 2) end) (char regex (+ index 2)))))
                    (write-string (if (and after (%ascii-word-char-p after))
                                      +ascii-word-start+
                                      +ascii-word-boundary+)
                                  out))
                  (incf index 2))
                 ((and (char= char #\\) next)
                  (write-char char out)
                  (write-char next out)
                  (incf index 2))
                 (t (write-char char out)
                    (incf index)))))
    (get-output-stream-string out)))

;;; --- Tier 1: high-confidence patterns (case-sensitive) ---

(defun %redaction-marker (id)
  "Replacement text for a match of the pattern named ID."
  (format nil "[REDACTED:~a]" id))

(defstruct (tier1 (:constructor %make-tier1 (patterns markers scanner)))
  "The tier-1 pattern alist and the two values derived from it.
One value rather than three defparameters, because the marker vector and the
scanner are only meaningful against the list they were built from: a table
edited on its own would keep the old scanner, and a match would then be labelled
with another pattern's id."
  (patterns nil :read-only t)
  (markers nil :read-only t)
  (scanner nil :read-only t))

(defun %compile-tier1 (patterns)
  "Build the tier-1 table from PATTERNS, an alist of (pattern-id . regex source).
The regexes are alternated into one scanner, each wrapped in a register, and
the markers are indexed by the same position. One scan per input is what keeps
this output equal to the reference SDKs': with a pass per pattern, an earlier
pattern rewrites text a later one would have matched, and \"Bearer glc_...\"
comes out labelled grafana-cloud-token instead of bearer-token. No entry may
contain a capturing group, because the index of the register that participated
is read back as a pattern id."
  (%make-tier1
   patterns
   (map 'vector (lambda (entry) (%redaction-marker (car entry))) patterns)
   (cl-ppcre:create-scanner
    (format nil "~{(~a)~^|~}"
            (mapcar (lambda (entry) (%ascii-boundaries (cdr entry))) patterns)))))

(defparameter +tier1+
  (%compile-tier1
   '(("grafana-cloud-token"           . "\\bglc_[A-Za-z0-9_-]{20,}")
     ("grafana-service-account-token" . "\\bglsa_[A-Za-z0-9_-]{20,}")
     ("aws-access-token"              . "\\b(?:A3T[A-Z0-9]|AKIA|ASIA|ABIA|ACCA)[A-Z2-7]{16}\\b")
     ("github-pat"                    . "\\bghp_[A-Za-z0-9_]{36,}")
     ("github-oauth"                  . "\\bgho_[A-Za-z0-9_]{36,}")
     ("github-app-token"              . "\\bghs_[A-Za-z0-9_]{36,}")
     ("github-fine-grained-pat"       . "\\bgithub_pat_[A-Za-z0-9_]{82}")
     ("anthropic-api-key"             . "\\bsk-ant-api03-[a-zA-Z0-9_-]{93}AA")
     ("anthropic-admin-key"           . "\\bsk-ant-admin01-[a-zA-Z0-9_-]{93}AA")
     ("openai-api-key"                . "\\bsk-[a-zA-Z0-9]{20}T3BlbkFJ[a-zA-Z0-9]{20}")
     ("openai-project-key"            . "\\bsk-proj-[a-zA-Z0-9_-]{40,}")
     ("openai-svcacct-key"            . "\\bsk-svcacct-[a-zA-Z0-9_-]{40,}")
     ("gcp-api-key"                   . "\\bAIza[A-Za-z0-9_-]{35}")
     ("private-key"                   . "-----BEGIN[A-Z ]*PRIVATE KEY-----[\\s\\S]*?-----END[A-Z ]*PRIVATE KEY-----")
     ("connection-string"             . "(?:postgres|mysql|mongodb|redis|amqp)://[^ \\t\\n\\f\\r\\xa0'\"]+@[^ \\t\\n\\f\\r\\xa0'\"]+")
     ("bearer-token"                  . "[Bb]earer[ \\t\\n\\f\\r\\xa0]+[A-Za-z0-9_.\\-~+/]{20,}={0,3}")
     ("slack-token"                   . "\\bxox[bporas]-[A-Za-z0-9-]{10,}")
     ("stripe-key"                    . "\\b[sr]k_(?:live|test)_[A-Za-z0-9]{20,}")
     ("sendgrid-api-key"              . "\\bSG\\.[A-Za-z0-9_-]{22}\\.[A-Za-z0-9_-]{43}")
     ("twilio-api-key"                . "\\bSK[a-f0-9]{32}")
     ("npm-token"                     . "\\bnpm_[A-Za-z0-9]{36}")
     ("pypi-token"                    . "\\bpypi-[A-Za-z0-9_-]{50,}")))
  "The 22 definite secret formats, in the order the shared table lists them.
Position decides the label when two patterns match the same text, so a pattern
added upstream goes in at the same index here.")

(defparameter +email-pattern+
  (let ((id "email"))
    (list id
          (cl-ppcre:create-scanner
           (%ascii-boundaries "\\b[A-Za-z0-9._%+\\-]+@[A-Za-z0-9.\\-]+\\.[A-Za-z]{2,}\\b"))
          (%redaction-marker id)))
  "(pattern-id scanner replacement) for email addresses, in the shape of a
tier-2 entry. Applied by both modes, gated by the caller's include-emails
option.")

;;; --- Tier 2: heuristic key/value patterns ---

(defparameter +tier2-patterns+
  (mapcar (lambda (entry)
            (destructuring-bind (id source replacement) entry
              (list id
                    (cl-ppcre:create-scanner (%ascii-boundaries source)
                                             :case-insensitive-mode t)
                    replacement)))
          '(("json-secret-field"
             "(\"(?:password|secret|token|credential|api_?key|private_?key|access_?key|client_?secret|auth_?token|secret_?key)\"[ \\t\\n\\f\\r\\xa0]*:[ \\t\\n\\f\\r\\xa0]*\")([^\"]+)(\")"
             "\\1[REDACTED:json-secret-field]\\3")
            ("env-secret-quoted-value"
             "(^|[^A-Za-z0-9])((?:PASSWORD|SECRET|TOKEN|KEY|CREDENTIAL|API_KEY|PRIVATE_KEY|ACCESS_KEY)[ \\t\\n\\f\\r\\xa0]*[=:][ \\t\\n\\f\\r\\xa0]*\")([^\"]+)(\")"
             "\\1\\2[REDACTED:env-secret-quoted-value]\\4")
            ("env-secret-value"
             "(^|[^A-Za-z0-9])((?:PASSWORD|SECRET|TOKEN|KEY|CREDENTIAL|API_KEY|PRIVATE_KEY|ACCESS_KEY)[ \\t\\n\\f\\r\\xa0]*[=:][ \\t\\n\\f\\r\\xa0]*)([^ \\t\\n\\f\\r\\xa0\"{}\\[\\],]+)"
             "\\1\\2[REDACTED:env-secret-value]")))
  "List of (pattern-id scanner replacement) applied in the order the shared
table lists them, which decides the label when two of them match the same text.
Each replacement reprints the groups upstream keepGroups preserves: the key, the
separator, and for a quoted value the closing quote, so the surrounding JSON
still parses. The left boundary group keeps MONKEY: from matching as KEY:.")

(defun %tier1-marker (match &rest registers)
  "Marker for a combined tier-1 match. Exactly one register participates, and
its index is the index of the pattern that fired. A match with no participating
register would mean a pattern carrying a capture group of its own, which the
index mapping cannot survive; POSITION-IF then returns NIL and AREF signals,
which apply-secret-redaction turns into a plain \"[REDACTED]\"."
  (declare (ignore match))
  (aref (tier1-markers +tier1+) (position-if #'identity registers)))

(defun %apply-tier1 (redactor text)
  "Apply the combined tier-1 scanner, plus the email pattern when enabled."
  (let ((s (cl-ppcre:regex-replace-all (tier1-scanner +tier1+) text #'%tier1-marker
                                       :simple-calls t)))
    (when (secret-redactor-include-emails redactor)
      (setf s (cl-ppcre:regex-replace-all (second +email-pattern+) s
                                          (third +email-pattern+))))
    s))

(defun redact-light (redactor text)
  "Light redaction: tier-1 patterns (and optional email) only.
Used for assistant text, reasoning, and conversation titles."
  (%apply-tier1 redactor text))

(defun redact-full (redactor text)
  "Full redaction: tier-1 (and optional email) plus the tier-2 key/value
heuristics. Used for user text, tool-call input, tool results, and system
prompts."
  (let ((s (%apply-tier1 redactor text)))
    (dolist (pattern +tier2-patterns+ s)
      (setf s (cl-ppcre:regex-replace-all (second pattern) s (third pattern))))))

(defun apply-secret-redaction (redactor mode text)
  "Redact TEXT according to MODE (:none, :light, or :full).
Returns TEXT unchanged when REDACTOR is nil, MODE is :none, or TEXT is empty.
Fails closed: a non-string TEXT and any error raised during redaction both
produce a \"[REDACTED]\" marker rather than leaking unredacted content."
  (if (or (null redactor)
          (eq mode :none)
          (null text)
          (and (stringp text) (zerop (length text))))
      text
      (handler-case
          (let ((s (if (stringp text) text "[REDACTED]")))
            (ecase mode
              (:light (redact-light redactor s))
              (:full  (redact-full redactor s))))
        (error () "[REDACTED]"))))
