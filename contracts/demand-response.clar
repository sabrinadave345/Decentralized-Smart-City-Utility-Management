;; Demand Response Contract
;; Manages peak usage reduction incentives

(define-data-var contract-owner principal tx-sender)

;; Constants
(define-constant ERR-NOT-AUTHORIZED u1)
(define-constant ERR-EVENT-NOT-FOUND u2)
(define-constant ERR-ALREADY-PARTICIPATING u3)
(define-constant ERR-NOT-PARTICIPATING u4)
(define-constant ERR-EVENT-ENDED u5)

;; Data structures
(define-map demand-events
  { event-id: uint }
  {
    start-time: uint,
    end-time: uint,
    utility-type: (string-utf8 20),
    reduction-target: uint,
    reward-per-unit: uint,
    active: bool
  }
)

(define-map event-participants
  { event-id: uint, participant: principal }
  {
    committed-reduction: uint,
    actual-reduction: uint,
    rewarded: bool
  }
)

(define-data-var next-event-id uint u1)

;; Public functions
(define-public (create-demand-event
                (start-time uint)
                (end-time uint)
                (utility-type (string-utf8 20))
                (reduction-target uint)
                (reward-per-unit uint))
  (let ((event-id (var-get next-event-id)))
    (asserts! (is-contract-owner tx-sender) (err ERR-NOT-AUTHORIZED))
    (map-set demand-events
      { event-id: event-id }
      {
        start-time: start-time,
        end-time: end-time,
        utility-type: utility-type,
        reduction-target: reduction-target,
        reward-per-unit: reward-per-unit,
        active: true
      })
    (var-set next-event-id (+ event-id u1))
    (ok event-id)))

(define-public (join-demand-event (event-id uint) (committed-reduction uint))
  (let ((participant tx-sender)
        (event (unwrap! (map-get? demand-events { event-id: event-id }) (err ERR-EVENT-NOT-FOUND))))
    (asserts! (get active event) (err ERR-EVENT-ENDED))
    (asserts! (is-none (map-get? event-participants { event-id: event-id, participant: participant })) (err ERR-ALREADY-PARTICIPATING))
    (map-set event-participants
      { event-id: event-id, participant: participant }
      {
        committed-reduction: committed-reduction,
        actual-reduction: u0,
        rewarded: false
      })
    (ok true)))

(define-public (record-actual-reduction (event-id uint) (participant principal) (actual-reduction uint))
  (let ((caller tx-sender)
        (event (unwrap! (map-get? demand-events { event-id: event-id }) (err ERR-EVENT-NOT-FOUND)))
        (participation (unwrap! (map-get? event-participants { event-id: event-id, participant: participant }) (err ERR-NOT-PARTICIPATING))))
    (asserts! (is-contract-owner caller) (err ERR-NOT-AUTHORIZED))
    (map-set event-participants
      { event-id: event-id, participant: participant }
      {
        committed-reduction: (get committed-reduction participation),
        actual-reduction: actual-reduction,
        rewarded: false
      })
    (ok true)))

(define-public (distribute-rewards (event-id uint) (participant principal))
  (let ((caller tx-sender)
        (event (unwrap! (map-get? demand-events { event-id: event-id }) (err ERR-EVENT-NOT-FOUND)))
        (participation (unwrap! (map-get? event-participants { event-id: event-id, participant: participant }) (err ERR-NOT-PARTICIPATING))))
    (asserts! (is-contract-owner caller) (err ERR-NOT-AUTHORIZED))
    (asserts! (not (get rewarded participation)) (err u6)) ;; Already rewarded
    (map-set event-participants
      { event-id: event-id, participant: participant }
      {
        committed-reduction: (get committed-reduction participation),
        actual-reduction: (get actual-reduction participation),
        rewarded: true
      })
    ;; In a real implementation, this would transfer tokens to the participant
    ;; based on their actual reduction and the reward rate
    (ok (* (get actual-reduction participation) (get reward-per-unit event)))))

(define-public (end-demand-event (event-id uint))
  (let ((caller tx-sender)
        (event (unwrap! (map-get? demand-events { event-id: event-id }) (err ERR-EVENT-NOT-FOUND))))
    (asserts! (is-contract-owner caller) (err ERR-NOT-AUTHORIZED))
    (map-set demand-events
      { event-id: event-id }
      {
        start-time: (get start-time event),
        end-time: (get end-time event),
        utility-type: (get utility-type event),
        reduction-target: (get reduction-target event),
        reward-per-unit: (get reward-per-unit event),
        active: false
      })
    (ok true)))

;; Read-only functions
(define-read-only (get-demand-event (event-id uint))
  (map-get? demand-events { event-id: event-id }))

(define-read-only (get-participant-data (event-id uint) (participant principal))
  (map-get? event-participants { event-id: event-id, participant: participant }))

(define-read-only (is-contract-owner (caller principal))
  (is-eq caller (var-get contract-owner)))

;; Contract initialization
(define-private (initialize-contract)
  (var-set contract-owner tx-sender))

(initialize-contract)
