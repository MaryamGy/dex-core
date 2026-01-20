;; -------------------------------------------------------------
;; Contract: dex-core.clar
;; Description: Simple constant-product AMM (x * y = k) for two
;; SIP-010 fungible tokens.
;; -------------------------------------------------------------

;; Define SIP-010 trait
(define-trait sip-010-trait
  (
    (transfer? (uint principal principal) (response bool uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-token-uri () (response (optional (string-utf8 256)) uint))
  )
)

(define-constant ERR-NOT-OWNER u100)
(define-constant ERR-ALREADY-INITIALIZED u101)
(define-constant ERR-INVALID-AMOUNT u102)
(define-constant ERR-INSUFFICIENT-INPUT u103)
(define-constant ERR-INSUFFICIENT-LIQUIDITY u104)
(define-constant ERR-SWAP-FAILED u105)
(define-constant ERR-TRANSFER-FAILED u106)
(define-constant ERR-NO-POOL u107)
(define-constant ERR-INSUFFICIENT-SHARES u108)

;; -------------------------
;; Config / Storage
;; -------------------------
(define-data-var owner (optional principal) none)

;; Token contract principals (SIP-010 contracts)
(define-data-var token-a principal 'SP000000000000000000002Q6VF78) ;; placeholder
(define-data-var token-b principal 'SP000000000000000000002Q6VF78) ;; placeholder

;; Fee in permille (e.g., u3 -> 0.3% fee)
(define-data-var fee-permille uint u3)

;; Reserves (token amounts held by pool)
(define-data-var reserve-a uint u0)
(define-data-var reserve-b uint u0)

;; LP shares accounting
(define-data-var total-shares uint u0)
(define-map lp-balances
  {provider: principal}
  {shares: uint})

;; -------------------------
;; Helpers
;; -------------------------

;;  Generic call to SIP-010 transfer? function
;; token-contract: principal of token contract
;; amount: uint
;; from-principal: principal (usually tx-sender)
;; to-principal: principal (usually (as-contract tx-sender))
;; Note: In production, use static contract references instead of dynamic principals
(define-private (ft-transfer (token-contract principal) (amount uint) (from-principal principal) (to-principal principal))
  ;; Placeholder - in actual use, would call specific token contract
  (ok true))

(define-private (validate-transfer (result (response bool uint)))
  (match result
    ok-val (ok ok-val)
    err-val (err ERR-TRANSFER-FAILED)))

;; get caller principal
(define-read-only (caller) (ok tx-sender))

;; safe min helper
(define-private (min (a uint) (b uint))
  (if (< a b) a b))

;; -------------------------
;; Initialization
;; -------------------------
;; Initialize owner and pair token contracts (call once).
(define-public (initialize (token-a-principal principal) (token-b-principal principal))
  (match (var-get owner)
    existing-owner (err ERR-ALREADY-INITIALIZED)
    (begin
      (asserts! (not (is-eq token-a-principal tx-sender)) (err u1))
      (asserts! (not (is-eq token-b-principal tx-sender)) (err u1))
      (var-set owner (some tx-sender))
      (var-set token-a token-a-principal)
      (var-set token-b token-b-principal)
      (ok true)
    )
  )
)

(define-read-only (get-owner) (ok (var-get owner)))
(define-read-only (get-tokens) (ok { token-a: (var-get token-a), token-b: (var-get token-b) }))
(define-read-only (get-fee) (ok (var-get fee-permille)))
(define-read-only (get-reserves) (ok { a: (var-get reserve-a), b: (var-get reserve-b) }))
(define-read-only (get-total-shares) (ok (var-get total-shares)))
(define-read-only (get-lp (p principal)) (ok (default-to { shares: u0 } (map-get? lp-balances { provider: p })) ))

;; -------------------------
;; Add Liquidity
;; Users must call this as a single transaction - the contract will call
;; each token's transfer? to pull tokens from the user into the pool.
;; - amount-a and amount-b are amounts the user provides.
;; On first provide, we mint shares = sqrt(amount-a * amount-b) approximation:
;; For simplicity we mint shares = amount-a (could use better logic).
;; -------------------------
(define-public (add-liquidity (amount-a uint) (amount-b uint))
  (let ((sender tx-sender))
    (if (or (<= amount-a u0) (<= amount-b u0))
        (err ERR-INVALID-AMOUNT)
        (begin
          ;; transfer token-a from user -> contract
          (unwrap-panic (ft-transfer (var-get token-a) amount-a sender (as-contract tx-sender)))
          ;; transfer token-b from user -> contract
          (unwrap-panic (ft-transfer (var-get token-b) amount-b sender (as-contract tx-sender)))

          ;; compute shares to mint
          (let ((total (var-get total-shares))
                (resA (var-get reserve-a))
                (resB (var-get reserve-b)))
            (if (or (<= total u0) (and (is-eq resA u0) (is-eq resB u0)))
                ;; first liquidity provider - seed pool
                (let ((mint-shares amount-a)) ;; simple seeding strategy
                  (var-set reserve-a (+ resA amount-a))
                  (var-set reserve-b (+ resB amount-b))
                  (var-set total-shares (+ total mint-shares))
                  (map-set lp-balances { provider: sender } { shares: (+ (get shares (default-to { shares: u0 } (map-get? lp-balances { provider: sender }))) mint-shares) })
                  (ok { minted: mint-shares })
                )
                ;; subsequent providers - keep ratio
                (let ((share-a (/ (* amount-a total) resA))
                      (share-b (/ (* amount-b total) resB))
                      (mint-shares (min share-a share-b)))
                  (if (<= mint-shares u0)
                      (err ERR-INSUFFICIENT-INPUT)
                      (begin
                        (var-set reserve-a (+ resA amount-a))
                        (var-set reserve-b (+ resB amount-b))
                        (var-set total-shares (+ total mint-shares))
                        (map-set lp-balances { provider: sender } { shares: (+ (get shares (default-to { shares: u0 } (map-get? lp-balances { provider: sender }))) mint-shares) })
                        (ok { minted: mint-shares })
                      )
                  )
                )
            )
          )
        )
    )
  )
)

;; -------------------------
;; Remove Liquidity
;; Provider redeems `shares` for underlying tokens.
;; -------------------------
(define-public (remove-liquidity (shares uint))
  (let ((sender tx-sender))
    (if (<= shares u0)
        (err ERR-INVALID-AMOUNT)
        (match (map-get? lp-balances { provider: sender })
          some-rec
            (let ((owned (get shares some-rec))
                  (total (var-get total-shares)))
              (if (< owned shares)
                  (err ERR-INSUFFICIENT-SHARES)
                  (let ((resA (var-get reserve-a))
                        (resB (var-get reserve-b))
                        (amount-a (/ (* resA shares) total))
                        (amount-b (/ (* resB shares) total)))
                    ;; update state
                    (var-set reserve-a (- resA amount-a))
                    (var-set reserve-b (- resB amount-b))
                    (var-set total-shares (- total shares))
                    (map-set lp-balances { provider: sender } { shares: (- owned shares) })
                    ;; transfer tokens back to provider
                    (unwrap-panic (ft-transfer (var-get token-a) amount-a (as-contract tx-sender) sender))
                    (unwrap-panic (ft-transfer (var-get token-b) amount-b (as-contract tx-sender) sender))
                    (ok { returned-a: amount-a, returned-b: amount-b })
                  )
              )
            )
          (err ERR-INSUFFICIENT-LIQUIDITY)
        )
    )
  )
)

;; -------------------------
;; Swap: token-a -> token-b
;; amount-in provided by caller; min-out is slippage protection
;; Uses constant product: (reserveA + in_after_fee) * (reserveB - out) >= reserveA * reserveB
;; Formula for out:
;;   in_after_fee = amount_in * (1000 - fee) / 1000
;;   out = floor(reserve_b * in_after_fee / (reserve_a + in_after_fee))
;; -------------------------
(define-public (swap-a-for-b (amount-in uint) (min-out uint))
  (let ((sender tx-sender))
    (if (<= amount-in u0) (err ERR-INVALID-AMOUNT)
        (begin
          ;; pull token-a into pool
          (unwrap-panic (ft-transfer (var-get token-a) amount-in sender (as-contract tx-sender)))

          (let ((fee (var-get fee-permille)))
            (let ((in_after_fee (/ (* amount-in (- u1000 fee)) u1000))
                  (resA (var-get reserve-a))
                  (resB (var-get reserve-b)))
              (if (<= in_after_fee u0) (err ERR-INSUFFICIENT-INPUT)
                  (let ((numerator (* resB in_after_fee))
                        (denom (+ resA in_after_fee))
                        (out (/ numerator denom)))
                    (if (< out min-out) (err ERR-SWAP-FAILED)
                        (if (> out resB) (err ERR-INSUFFICIENT-LIQUIDITY)
                            (begin
                              ;; update reserves
                              (var-set reserve-a (+ resA amount-in))
                              (var-set reserve-b (- resB out))
                              ;; send token-b to sender
                              (unwrap-panic (ft-transfer (var-get token-b) out (as-contract tx-sender) sender))
                              (ok { out: out })
                            )))))))
        )
    )
  )
)

;; -------------------------
;; Swap: token-b -> token-a (mirror)
;; -------------------------
(define-public (swap-b-for-a (amount-in uint) (min-out uint))
  (let ((sender tx-sender))
    (if (<= amount-in u0) (err ERR-INVALID-AMOUNT)
        (begin
          ;; pull token-b into pool
          (unwrap-panic (ft-transfer (var-get token-b) amount-in sender (as-contract tx-sender)))

          (let ((fee (var-get fee-permille)))
            (let ((in_after_fee (/ (* amount-in (- u1000 fee)) u1000))
                  (resA (var-get reserve-a))
                  (resB (var-get reserve-b)))
              (if (<= in_after_fee u0) (err ERR-INSUFFICIENT-INPUT)
                  (let ((numerator (* resA in_after_fee))
                        (denom (+ resB in_after_fee))
                        (out (/ numerator denom)))
                    (if (< out min-out) (err ERR-SWAP-FAILED)
                        (if (> out resA) (err ERR-INSUFFICIENT-LIQUIDITY)
                            (begin
                              ;; update reserves
                              (var-set reserve-b (+ resB amount-in))
                              (var-set reserve-a (- resA out))
                              ;; send token-a to sender
                              (unwrap-panic (ft-transfer (var-get token-a) out (as-contract tx-sender) sender))
                              (ok { out: out })
                            )))))))
        )
    )
  )
)

;; -------------------------
;; Owner: set fee permille
;; -------------------------
(define-public (set-fee (new-fee uint))
  (match (var-get owner)
    o
      (if (is-eq o tx-sender)
          (begin
            (asserts! (< new-fee u1001) (err u1))
            (var-set fee-permille new-fee)
            (ok new-fee)
          )
          (err ERR-NOT-OWNER)
      )
    (err ERR-NOT-OWNER)
  )
)
