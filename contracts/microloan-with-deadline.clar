(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_LOAN_NOT_FOUND (err u101))
(define-constant ERR_LOAN_ALREADY_EXISTS (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_LOAN_NOT_ACTIVE (err u104))
(define-constant ERR_DEADLINE_NOT_REACHED (err u105))
(define-constant ERR_LOAN_ALREADY_REPAID (err u106))
(define-constant ERR_INVALID_AMOUNT (err u107))
(define-constant ERR_INVALID_DEADLINE (err u108))

(define-constant LOAN_STATE_ACTIVE u1)
(define-constant LOAN_STATE_REPAID u2)
(define-constant LOAN_STATE_DEFAULTED u3)

(define-map loans
  { loan-id: uint }
  {
    lender: principal,
    borrower: principal,
    amount: uint,
    deadline: uint,
    state: uint,
    created-at: uint,
    interest-rate: uint
  }
)

(define-data-var next-loan-id uint u1)

(define-public (create-loan (borrower principal) (amount uint) (deadline uint) (interest-rate uint))
  (let
    (
      (loan-id (var-get next-loan-id))
      (current-block stacks-block-height)
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> deadline current-block) ERR_INVALID_DEADLINE)
    (asserts! (<= interest-rate u10000) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? loans { loan-id: loan-id })) ERR_LOAN_ALREADY_EXISTS)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set loans
      { loan-id: loan-id }
      {
        lender: tx-sender,
        borrower: borrower,
        amount: amount,
        deadline: deadline,
        state: LOAN_STATE_ACTIVE,
        created-at: current-block,
        interest-rate: interest-rate
      }
    )
    
    (var-set next-loan-id (+ loan-id u1))
    (ok loan-id)
  )
)

(define-public (fund-loan (loan-id uint))
  (let
    (
      (loan-data (unwrap! (map-get? loans { loan-id: loan-id }) ERR_LOAN_NOT_FOUND))
    )
    (asserts! (is-eq (get lender loan-data) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get state loan-data) LOAN_STATE_ACTIVE) ERR_LOAN_NOT_ACTIVE)
    
    (try! (as-contract (stx-transfer? (get amount loan-data) tx-sender (get borrower loan-data))))
    (ok true)
  )
)

(define-public (repay-loan (loan-id uint))
  (let
    (
      (loan-data (unwrap! (map-get? loans { loan-id: loan-id }) ERR_LOAN_NOT_FOUND))
    )
    (asserts! (is-eq (get borrower loan-data) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get state loan-data) LOAN_STATE_ACTIVE) ERR_LOAN_NOT_ACTIVE)
    (asserts! (<= stacks-block-height (get deadline loan-data)) ERR_DEADLINE_NOT_REACHED)
    
    (let
      (
        (principal-amount (get amount loan-data))
        (rate (get interest-rate loan-data))
        (blocks-elapsed (- stacks-block-height (get created-at loan-data)))
        (interest-amount (/ (* principal-amount rate blocks-elapsed) u1000000))
        (total-amount (+ principal-amount interest-amount))
      )
      (try! (stx-transfer? total-amount tx-sender (get lender loan-data)))
    )
    
    (map-set loans
      { loan-id: loan-id }
      (merge loan-data { state: LOAN_STATE_REPAID })
    )
    (ok true)
  )
)

(define-public (claim-default (loan-id uint))
  (let
    (
      (loan-data (unwrap! (map-get? loans { loan-id: loan-id }) ERR_LOAN_NOT_FOUND))
    )
    (asserts! (is-eq (get lender loan-data) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get state loan-data) LOAN_STATE_ACTIVE) ERR_LOAN_NOT_ACTIVE)
    (asserts! (> stacks-block-height (get deadline loan-data)) ERR_DEADLINE_NOT_REACHED)
    
    (map-set loans
      { loan-id: loan-id }
      (merge loan-data { state: LOAN_STATE_DEFAULTED })
    )
    (ok true)
  )
)

(define-public (extend-deadline (loan-id uint) (new-deadline uint))
  (let
    (
      (loan-data (unwrap! (map-get? loans { loan-id: loan-id }) ERR_LOAN_NOT_FOUND))
    )
    (asserts! (is-eq (get lender loan-data) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get state loan-data) LOAN_STATE_ACTIVE) ERR_LOAN_NOT_ACTIVE)
    (asserts! (> new-deadline (get deadline loan-data)) ERR_INVALID_DEADLINE)
    
    (map-set loans
      { loan-id: loan-id }
      (merge loan-data { deadline: new-deadline })
    )
    (ok true)
  )
)

(define-read-only (get-loan (loan-id uint))
  (map-get? loans { loan-id: loan-id })
)

(define-read-only (get-loan-state (loan-id uint))
  (match (map-get? loans { loan-id: loan-id })
    loan-data (ok (get state loan-data))
    ERR_LOAN_NOT_FOUND
  )
)

(define-read-only (is-loan-overdue (loan-id uint))
  (match (map-get? loans { loan-id: loan-id })
    loan-data 
      (if (and 
            (is-eq (get state loan-data) LOAN_STATE_ACTIVE)
            (> stacks-block-height (get deadline loan-data)))
        (ok true)
        (ok false))
    ERR_LOAN_NOT_FOUND
  )
)

(define-read-only (get-next-loan-id)
  (var-get next-loan-id)
)

(define-read-only (can-repay (loan-id uint))
  (match (map-get? loans { loan-id: loan-id })
    loan-data
      (ok (and
        (is-eq (get state loan-data) LOAN_STATE_ACTIVE)
        (<= stacks-block-height (get deadline loan-data))))
    ERR_LOAN_NOT_FOUND
  )
)

(define-read-only (can-claim-default (loan-id uint))
  (match (map-get? loans { loan-id: loan-id })
    loan-data
      (ok (and
        (is-eq (get state loan-data) LOAN_STATE_ACTIVE)
        (> stacks-block-height (get deadline loan-data))))
    ERR_LOAN_NOT_FOUND
  )
)

(define-read-only (get-loan-status (loan-id uint))
  (match (map-get? loans { loan-id: loan-id })
    loan-data
      (ok {
        loan-id: loan-id,
        lender: (get lender loan-data),
        borrower: (get borrower loan-data),
        amount: (get amount loan-data),
        deadline: (get deadline loan-data),
        state: (get state loan-data),
        created-at: (get created-at loan-data),
        interest-rate: (get interest-rate loan-data),
        blocks-remaining: (if (> (get deadline loan-data) stacks-block-height)
                           (- (get deadline loan-data) stacks-block-height)
                           u0),
        is-overdue: (> stacks-block-height (get deadline loan-data)),
        current-interest: (let
          (
            (principal-amount (get amount loan-data))
            (rate (get interest-rate loan-data))
            (blocks-elapsed (- stacks-block-height (get created-at loan-data)))
          )
          (/ (* principal-amount rate blocks-elapsed) u1000000)),
        total-owed: (let
          (
            (principal-amount (get amount loan-data))
            (rate (get interest-rate loan-data))
            (blocks-elapsed (- stacks-block-height (get created-at loan-data)))
            (interest-amount (/ (* principal-amount rate blocks-elapsed) u1000000))
          )
          (+ principal-amount interest-amount))
      })
    ERR_LOAN_NOT_FOUND
  )
)

(define-read-only (calculate-interest (loan-id uint))
  (match (map-get? loans { loan-id: loan-id })
    loan-data
      (let
        (
          (principal-amount (get amount loan-data))
          (rate (get interest-rate loan-data))
          (blocks-elapsed (- stacks-block-height (get created-at loan-data)))
          (interest-amount (/ (* principal-amount rate blocks-elapsed) u1000000))
        )
        (ok {
          principal: principal-amount,
          interest-rate: rate,
          blocks-elapsed: blocks-elapsed,
          interest-amount: interest-amount,
          total-amount: (+ principal-amount interest-amount)
        }))
    ERR_LOAN_NOT_FOUND
  )
)