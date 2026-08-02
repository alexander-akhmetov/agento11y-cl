(in-package :agento11y-cl)

;;; Secret redaction engine for exported generation content.
;;;
;;; ~22 high-confidence patterns hand-curated from Gitleaks
;;; (https://github.com/gitleaks/gitleaks). Kept in sync with
;;; python/agento11y/redaction.py. Two tiers:
;;;   - Tier 1: definite secret formats plus optional email addresses,
;;;     used by both redact-full and redact-light.
;;;   - Tier 2: heuristic env-value patterns, used only by redact-full.

(defstruct (secret-redactor (:constructor %make-secret-redactor (include-emails)))
  (include-emails t :read-only t))

(defun make-secret-redactor (&key (include-emails t))
  "Create a secret redactor. INCLUDE-EMAILS toggles the email pattern."
  (%make-secret-redactor include-emails))

;;; --- Tier 1: high-confidence patterns (case-sensitive) ---

(defparameter +tier1-patterns+
  (list
   (cons "grafana-cloud-token"           (cl-ppcre:create-scanner "\\bglc_[A-Za-z0-9_-]{20,}"))
   (cons "grafana-service-account-token" (cl-ppcre:create-scanner "\\bglsa_[A-Za-z0-9_-]{20,}"))
   (cons "aws-access-token"              (cl-ppcre:create-scanner "\\b(?:A3T[A-Z0-9]|AKIA|ASIA|ABIA|ACCA)[A-Z2-7]{16}\\b"))
   (cons "github-pat"                    (cl-ppcre:create-scanner "\\bghp_[A-Za-z0-9_]{36,}"))
   (cons "github-oauth"                  (cl-ppcre:create-scanner "\\bgho_[A-Za-z0-9_]{36,}"))
   (cons "github-app-token"              (cl-ppcre:create-scanner "\\bghs_[A-Za-z0-9_]{36,}"))
   (cons "github-fine-grained-pat"       (cl-ppcre:create-scanner "\\bgithub_pat_[A-Za-z0-9_]{82}"))
   (cons "anthropic-api-key"             (cl-ppcre:create-scanner "\\bsk-ant-api03-[a-zA-Z0-9_-]{93}AA"))
   (cons "anthropic-admin-key"           (cl-ppcre:create-scanner "\\bsk-ant-admin01-[a-zA-Z0-9_-]{93}AA"))
   (cons "openai-api-key"                (cl-ppcre:create-scanner "\\bsk-[a-zA-Z0-9]{20}T3BlbkFJ[a-zA-Z0-9]{20}"))
   (cons "openai-project-key"            (cl-ppcre:create-scanner "\\bsk-proj-[a-zA-Z0-9_-]{40,}"))
   (cons "openai-svcacct-key"            (cl-ppcre:create-scanner "\\bsk-svcacct-[a-zA-Z0-9_-]{40,}"))
   (cons "gcp-api-key"                   (cl-ppcre:create-scanner "\\bAIza[A-Za-z0-9_-]{35}"))
   (cons "private-key"                   (cl-ppcre:create-scanner "-----BEGIN[A-Z ]*PRIVATE KEY-----[\\s\\S]*?-----END[A-Z ]*PRIVATE KEY-----"))
   (cons "connection-string"             (cl-ppcre:create-scanner "(?:postgres|mysql|mongodb|redis|amqp)://[^\\s'\"]+@[^\\s'\"]+"))
   (cons "bearer-token"                  (cl-ppcre:create-scanner "[Bb]earer\\s+[A-Za-z0-9_.\\-~+/]{20,}={0,3}"))
   (cons "slack-token"                   (cl-ppcre:create-scanner "\\bxox[bporas]-[A-Za-z0-9-]{10,}"))
   (cons "stripe-key"                    (cl-ppcre:create-scanner "\\b[sr]k_(?:live|test)_[A-Za-z0-9]{20,}"))
   (cons "sendgrid-api-key"              (cl-ppcre:create-scanner "\\bSG\\.[A-Za-z0-9_-]{22}\\.[A-Za-z0-9_-]{43}"))
   (cons "twilio-api-key"                (cl-ppcre:create-scanner "\\bSK[a-f0-9]{32}"))
   (cons "npm-token"                     (cl-ppcre:create-scanner "\\bnpm_[A-Za-z0-9]{36}"))
   (cons "pypi-token"                    (cl-ppcre:create-scanner "\\bpypi-[A-Za-z0-9_-]{50,}")))
  "Alist of (pattern-id . scanner) for definite secret formats.")

(defparameter +email-pattern+
  (cons "email"
        (cl-ppcre:create-scanner "\\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\b"
                                 :case-insensitive-mode t))
  "(pattern-id . scanner) for email addresses.")

;;; --- Tier 2: heuristic env-value patterns ---

(defparameter +tier2-pattern+
  (cons "env-secret-value"
        (cl-ppcre:create-scanner
         "((?:PASSWORD|SECRET|TOKEN|KEY|CREDENTIAL|API_KEY|PRIVATE_KEY|ACCESS_KEY)\\s*[=:]\\s*)([^\\s\"{}\\[\\],]+)"
         :case-insensitive-mode t))
  "(pattern-id . scanner) for KEY=value style env assignments. The value is
replaced while the assignment prefix (register 1) is preserved.")

(defun %apply-pattern (scanner id text)
  "Replace every SCANNER match in TEXT with a [REDACTED:ID] marker."
  (cl-ppcre:regex-replace-all scanner text (format nil "[REDACTED:~a]" id)))

(defun %apply-tier1 (redactor text)
  "Apply all tier-1 patterns, plus the email pattern when enabled."
  (let ((s text))
    (dolist (p +tier1-patterns+)
      (setf s (%apply-pattern (cdr p) (car p) s)))
    (when (secret-redactor-include-emails redactor)
      (setf s (%apply-pattern (cdr +email-pattern+) (car +email-pattern+) s)))
    s))

(defun redact-light (redactor text)
  "Light redaction: tier-1 patterns (and optional email) only.
Used for assistant text and reasoning."
  (%apply-tier1 redactor text))

(defun redact-full (redactor text)
  "Full redaction: tier-1 (and optional email) plus the tier-2 env-value
heuristic. Used for tool-call input, tool results, and system prompts."
  (let ((s (%apply-tier1 redactor text)))
    (cl-ppcre:regex-replace-all (cdr +tier2-pattern+) s
                                (format nil "\\1[REDACTED:~a]" (car +tier2-pattern+)))))

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
