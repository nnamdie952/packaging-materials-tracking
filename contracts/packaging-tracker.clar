;; Packaging Materials Tracking Contract
;; Sustainable packaging system with material sourcing and environmental impact monitoring

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_MATERIAL_NOT_FOUND (err u101))
(define-constant ERR_SUPPLIER_NOT_FOUND (err u102))
(define-constant ERR_BATCH_NOT_FOUND (err u103))
(define-constant ERR_INSUFFICIENT_QUANTITY (err u104))
(define-constant ERR_INVALID_IMPACT_SCORE (err u105))

;; Data Variables
(define-data-var material-counter uint u0)
(define-data-var supplier-counter uint u0)
(define-data-var batch-counter uint u0)

;; Data Maps
(define-map materials
  { material-id: uint }
  {
    name: (string-ascii 50),
    category: (string-ascii 30),
    recyclable: bool,
    biodegradable: bool,
    carbon-footprint: uint,
    cost-per-unit: uint,
    unit-type: (string-ascii 20),
    created-at: uint
  }
)

(define-map suppliers
  { supplier-id: uint }
  {
    name: (string-ascii 50),
    location: (string-ascii 100),
    sustainability-rating: uint,
    certification-level: (string-ascii 20),
    contact-info: (string-ascii 100),
    registered-by: principal,
    status: (string-ascii 20),
    created-at: uint
  }
)

(define-map material-batches
  { batch-id: uint }
  {
    material-id: uint,
    supplier-id: uint,
    quantity: uint,
    unit-cost: uint,
    production-date: uint,
    expiry-date: uint,
    quality-grade: (string-ascii 10),
    environmental-impact-score: uint,
    recycling-instructions: (string-ascii 200),
    batch-status: (string-ascii 20)
  }
)

(define-map recycling-records
  { batch-id: uint }
  {
    recycled-quantity: uint,
    recycling-facility: (string-ascii 50),
    recycling-date: uint,
    recycling-method: (string-ascii 50),
    recovery-rate: uint,
    recorded-by: principal
  }
)

;; Public Functions

;; Register a new packaging material
(define-public (register-material
  (name (string-ascii 50))
  (category (string-ascii 30))
  (recyclable bool)
  (biodegradable bool)
  (carbon-footprint uint)
  (cost-per-unit uint)
  (unit-type (string-ascii 20))
)
  (let
    (
      (material-id (+ (var-get material-counter) u1))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= carbon-footprint u1000) ERR_INVALID_IMPACT_SCORE)
    
    (map-set materials
      { material-id: material-id }
      {
        name: name,
        category: category,
        recyclable: recyclable,
        biodegradable: biodegradable,
        carbon-footprint: carbon-footprint,
        cost-per-unit: cost-per-unit,
        unit-type: unit-type,
        created-at: stacks-block-height
      }
    )
    
    (var-set material-counter material-id)
    (ok material-id)
  )
)

;; Register a new supplier
(define-public (register-supplier
  (name (string-ascii 50))
  (location (string-ascii 100))
  (sustainability-rating uint)
  (certification-level (string-ascii 20))
  (contact-info (string-ascii 100))
)
  (let
    (
      (supplier-id (+ (var-get supplier-counter) u1))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= sustainability-rating u100) ERR_INVALID_IMPACT_SCORE)
    
    (map-set suppliers
      { supplier-id: supplier-id }
      {
        name: name,
        location: location,
        sustainability-rating: sustainability-rating,
        certification-level: certification-level,
        contact-info: contact-info,
        registered-by: tx-sender,
        status: "active",
        created-at: stacks-block-height
      }
    )
    
    (var-set supplier-counter supplier-id)
    (ok supplier-id)
  )
)

;; Create a new material batch
(define-public (create-batch
  (material-id uint)
  (supplier-id uint)
  (quantity uint)
  (unit-cost uint)
  (expiry-date uint)
  (quality-grade (string-ascii 10))
  (environmental-impact-score uint)
  (recycling-instructions (string-ascii 200))
)
  (let
    (
      (batch-id (+ (var-get batch-counter) u1))
      (material (unwrap! (map-get? materials { material-id: material-id }) ERR_MATERIAL_NOT_FOUND))
      (supplier (unwrap! (map-get? suppliers { supplier-id: supplier-id }) ERR_SUPPLIER_NOT_FOUND))
    )
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender (get registered-by supplier))) ERR_UNAUTHORIZED)
    (asserts! (<= environmental-impact-score u100) ERR_INVALID_IMPACT_SCORE)
    
    (map-set material-batches
      { batch-id: batch-id }
      {
        material-id: material-id,
        supplier-id: supplier-id,
        quantity: quantity,
        unit-cost: unit-cost,
        production-date: stacks-block-height,
        expiry-date: expiry-date,
        quality-grade: quality-grade,
        environmental-impact-score: environmental-impact-score,
        recycling-instructions: recycling-instructions,
        batch-status: "available"
      }
    )
    
    (var-set batch-counter batch-id)
    (ok batch-id)
  )
)

;; Use materials from a batch
(define-public (use-materials
  (batch-id uint)
  (quantity-used uint)
)
  (let
    (
      (batch (unwrap! (map-get? material-batches { batch-id: batch-id }) ERR_BATCH_NOT_FOUND))
      (remaining-quantity (- (get quantity batch) quantity-used))
    )
    (asserts! (>= (get quantity batch) quantity-used) ERR_INSUFFICIENT_QUANTITY)
    
    ;; Update batch quantity or mark as depleted
    (if (> remaining-quantity u0)
      (map-set material-batches
        { batch-id: batch-id }
        (merge batch { quantity: remaining-quantity })
      )
      (map-set material-batches
        { batch-id: batch-id }
        (merge batch { quantity: u0, batch-status: "depleted" })
      )
    )
    
    (ok remaining-quantity)
  )
)

;; Record recycling activity
(define-public (record-recycling
  (batch-id uint)
  (recycled-quantity uint)
  (recycling-facility (string-ascii 50))
  (recycling-method (string-ascii 50))
  (recovery-rate uint)
)
  (let
    (
      (batch (unwrap! (map-get? material-batches { batch-id: batch-id }) ERR_BATCH_NOT_FOUND))
    )
    (asserts! (<= recovery-rate u100) ERR_INVALID_IMPACT_SCORE)
    
    (map-set recycling-records
      { batch-id: batch-id }
      {
        recycled-quantity: recycled-quantity,
        recycling-facility: recycling-facility,
        recycling-date: stacks-block-height,
        recycling-method: recycling-method,
        recovery-rate: recovery-rate,
        recorded-by: tx-sender
      }
    )
    
    (ok true)
  )
)

;; Update supplier sustainability rating
(define-public (update-supplier-rating
  (supplier-id uint)
  (new-rating uint)
)
  (let
    (
      (supplier (unwrap! (map-get? suppliers { supplier-id: supplier-id }) ERR_SUPPLIER_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-rating u100) ERR_INVALID_IMPACT_SCORE)
    
    (map-set suppliers
      { supplier-id: supplier-id }
      (merge supplier { sustainability-rating: new-rating })
    )
    
    (ok true)
  )
)

;; Update batch status
(define-public (update-batch-status
  (batch-id uint)
  (new-status (string-ascii 20))
)
  (let
    (
      (batch (unwrap! (map-get? material-batches { batch-id: batch-id }) ERR_BATCH_NOT_FOUND))
    )
    
    (map-set material-batches
      { batch-id: batch-id }
      (merge batch { batch-status: new-status })
    )
    
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-material (material-id uint))
  (map-get? materials { material-id: material-id })
)

(define-read-only (get-supplier (supplier-id uint))
  (map-get? suppliers { supplier-id: supplier-id })
)

(define-read-only (get-batch (batch-id uint))
  (map-get? material-batches { batch-id: batch-id })
)

(define-read-only (get-recycling-record (batch-id uint))
  (map-get? recycling-records { batch-id: batch-id })
)

(define-read-only (calculate-total-cost (batch-id uint))
  (match (map-get? material-batches { batch-id: batch-id })
    batch (ok (* (get quantity batch) (get unit-cost batch)))
    ERR_BATCH_NOT_FOUND
  )
)

(define-read-only (get-environmental-impact (batch-id uint))
  (match (map-get? material-batches { batch-id: batch-id })
    batch
    (match (map-get? materials { material-id: (get material-id batch) })
      material
      (ok {
        batch-impact: (get environmental-impact-score batch),
        carbon-footprint: (get carbon-footprint material),
        recyclable: (get recyclable material),
        biodegradable: (get biodegradable material)
      })
      ERR_MATERIAL_NOT_FOUND
    )
    ERR_BATCH_NOT_FOUND
  )
)

(define-read-only (get-material-count)
  (var-get material-counter)
)

(define-read-only (get-supplier-count)
  (var-get supplier-counter)
)

(define-read-only (get-batch-count)
  (var-get batch-counter)
)

;; title: packaging-tracker
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

