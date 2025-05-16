;; Payment Processing Contract
;; Handles automated billing for utility usage

(define-data-var contract-owner principal tx-sender)

;; Constants
(define-constant ERR-NOT-AUTHORIZED u1)
(define-constant ERR-INSUFFICIENT-FUNDS u2)
(define-constant ERR-INVOICE-NOT-FOUND u3)
(define-constant ERR-ALREADY-PAID u4)

;; Data structures
(define-map utility-rates
  { utility-type: (string-utf8 20) }
  { rate-per-unit: uint }
)

(define-map invoices
  { invoice-id: uint }
  {
    resident: principal,
    utility-type: (string-utf8 20),
    period: uint,
    amount: uint,
    rate: uint,
    total-due: uint,
    paid: bool,
    due-date: uint
  }
)

(define-data-var next-invoice-id uint u1)

;; Public functions
(define-public (set-utility-rate (utility-type (string-utf8 20)) (rate-per-unit uint))
  (begin
    (asserts! (is-contract-owner tx-sender) (err ERR-NOT-AUTHORIZED))
    (map-set utility-rates
      { utility-type: utility-type }
      { rate-per-unit: rate-per-unit })
    (ok true)))

(define-public (generate-invoice
                (resident principal)
                (utility-type (string-utf8 20))
                (period uint)
                (amount uint)
                (due-date uint))
  (let ((invoice-id (var-get next-invoice-id))
        (rate-data (unwrap! (map-get? utility-rates { utility-type: utility-type }) (err u5)))
        (rate (get rate-per-unit rate-data))
        (total-due (* amount rate)))
    (asserts! (is-contract-owner tx-sender) (err ERR-NOT-AUTHORIZED))
    (map-set invoices
      { invoice-id: invoice-id }
      {
        resident: resident,
        utility-type: utility-type,
        period: period,
        amount: amount,
        rate: rate,
        total-due: total-due,
        paid: false,
        due-date: due-date
      })
    (var-set next-invoice-id (+ invoice-id u1))
    (ok invoice-id)))

(define-public (pay-invoice (invoice-id uint))
  (let ((caller tx-sender)
        (invoice (unwrap! (map-get? invoices { invoice-id: invoice-id }) (err ERR-INVOICE-NOT-FOUND))))
    (asserts! (is-eq caller (get resident invoice)) (err ERR-NOT-AUTHORIZED))
    (asserts! (not (get paid invoice)) (err ERR-ALREADY-PAID))
    ;; In a real implementation, this would transfer tokens from the resident
    ;; to the utility provider's account
    (map-set invoices
      { invoice-id: invoice-id }
      {
        resident: (get resident invoice),
        utility-type: (get utility-type invoice),
        period: (get period invoice),
        amount: (get amount invoice),
        rate: (get rate invoice),
        total-due: (get total-due invoice),
        paid: true,
        due-date: (get due-date invoice)
      })
    (ok true)))

;; Read-only functions
(define-read-only (get-utility-rate (utility-type (string-utf8 20)))
  (map-get? utility-rates { utility-type: utility-type }))

(define-read-only (get-invoice (invoice-id uint))
  (map-get? invoices { invoice-id: invoice-id }))

(define-read-only (get-resident-invoices (resident principal))
  ;; In a real implementation, this would return all invoices for a resident
  ;; Here we just return a placeholder since Clarity doesn't support returning arrays directly
  (ok true))

(define-read-only (is-contract-owner (caller principal))
  (is-eq caller (var-get contract-owner)))

;; Contract initialization
(define-private (initialize-contract)
  (var-set contract-owner tx-sender))

(initialize-contract)
