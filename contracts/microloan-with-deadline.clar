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
    interest-rate: uint,
    amount-paid: uint
  }
)

(define-map payment-history
  { loan-id: uint, payment-id: uint }
  {
    amount: uint,
    timestamp: uint,
    remaining-balance: uint
  }
)

(define-map loan-payment-count
  { loan-id: uint }
  { count: uint }
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
        interest-rate: interest-rate,
        amount-paid: u0
      }
    )
    
    (map-set loan-payment-count { loan-id: loan-id } { count: u0 })
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
      (merge loan-data { state: LOAN_STATE_REPAID, amount-paid: (get amount loan-data) })
    )
    (ok true)
  )
)

(define-public (make-partial-payment (loan-id uint) (payment-amount uint))
  (let
    (
      (loan-data (unwrap! (map-get? loans { loan-id: loan-id }) ERR_LOAN_NOT_FOUND))
      (payment-count-data (unwrap! (map-get? loan-payment-count { loan-id: loan-id }) ERR_LOAN_NOT_FOUND))
      (current-payment-id (get count payment-count-data))
    )
    (asserts! (is-eq (get borrower loan-data) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get state loan-data) LOAN_STATE_ACTIVE) ERR_LOAN_NOT_ACTIVE)
    (asserts! (<= stacks-block-height (get deadline loan-data)) ERR_DEADLINE_NOT_REACHED)
    (asserts! (> payment-amount u0) ERR_INVALID_AMOUNT)
    
    (let
      (
        (principal-amount (get amount loan-data))
        (rate (get interest-rate loan-data))
        (blocks-elapsed (- stacks-block-height (get created-at loan-data)))
        (interest-amount (/ (* principal-amount rate blocks-elapsed) u1000000))
        (total-owed (+ principal-amount interest-amount))
        (amount-already-paid (get amount-paid loan-data))
        (remaining-balance (- total-owed amount-already-paid))
        (new-amount-paid (+ amount-already-paid payment-amount))
        (new-remaining-balance (if (>= new-amount-paid total-owed) u0 (- total-owed new-amount-paid)))
        (actual-payment (if (> payment-amount remaining-balance) remaining-balance payment-amount))
      )
      (asserts! (> remaining-balance u0) ERR_LOAN_ALREADY_REPAID)
      
      (try! (stx-transfer? actual-payment tx-sender (get lender loan-data)))
      
      (map-set payment-history
        { loan-id: loan-id, payment-id: current-payment-id }
        {
          amount: actual-payment,
          timestamp: stacks-block-height,
          remaining-balance: new-remaining-balance
        }
      )
      
      (map-set loan-payment-count
        { loan-id: loan-id }
        { count: (+ current-payment-id u1) }
      )
      
      (if (>= new-amount-paid total-owed)
        (map-set loans
          { loan-id: loan-id }
          (merge loan-data { state: LOAN_STATE_REPAID, amount-paid: total-owed })
        )
        (map-set loans
          { loan-id: loan-id }
          (merge loan-data { amount-paid: new-amount-paid })
        )
      )
      
      (ok {
        payment-amount: actual-payment,
        remaining-balance: new-remaining-balance,
        loan-fully-paid: (>= new-amount-paid total-owed)
      })
    )
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
        amount-paid: (get amount-paid loan-data),
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
          (+ principal-amount interest-amount)),
        remaining-balance: (let
          (
            (principal-amount (get amount loan-data))
            (rate (get interest-rate loan-data))
            (blocks-elapsed (- stacks-block-height (get created-at loan-data)))
            (interest-amount (/ (* principal-amount rate blocks-elapsed) u1000000))
            (total-owed (+ principal-amount interest-amount))
            (amount-paid (get amount-paid loan-data))
          )
          (if (>= amount-paid total-owed) u0 (- total-owed amount-paid)))
      })
    ERR_LOAN_NOT_FOUND
  )
)

(define-read-only (get-payment-history (loan-id uint) (payment-id uint))
  (map-get? payment-history { loan-id: loan-id, payment-id: payment-id })
)

(define-read-only (get-payment-count (loan-id uint))
  (match (map-get? loan-payment-count { loan-id: loan-id })
    count-data (ok (get count count-data))
    ERR_LOAN_NOT_FOUND
  )
)

(define-read-only (get-remaining-balance (loan-id uint))
  (match (map-get? loans { loan-id: loan-id })
    loan-data
      (let
        (
          (principal-amount (get amount loan-data))
          (rate (get interest-rate loan-data))
          (blocks-elapsed (- stacks-block-height (get created-at loan-data)))
          (interest-amount (/ (* principal-amount rate blocks-elapsed) u1000000))
          (total-owed (+ principal-amount interest-amount))
          (amount-paid (get amount-paid loan-data))
        )
        (ok (if (>= amount-paid total-owed) u0 (- total-owed amount-paid))))
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