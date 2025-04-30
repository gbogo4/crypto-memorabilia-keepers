;; Crypto Memorabilia Keepers Contract
;; This smart contract provides management capabilities for a collection of unique memorabilia items. The contract handles inventory tracking, ownership verification, and controlled access to items
;; with validation mechanisms for item metadata such as nomenclature, dimensions, and categories.

;; -----------------------------
;; Constants Definition
;; -----------------------------
;; Designates the contract administrator (initial deployer)
(define-constant PLATFORM-ADMINISTRATOR tx-sender)

;; Error notification system
(define-constant ERROR-ITEM-MISSING (err u301))                ;; Item requested cannot be located in records
(define-constant ERROR-ITEM-ALREADY-EXISTS (err u302))         ;; Attempted to register an item with duplicate identifier
(define-constant ERROR-ADMIN-PRIVILEGE-REQUIRED (err u307))    ;; Operation restricted to administrator account
(define-constant ERROR-ACCESS-RIGHTS-MISSING (err u308))       ;; User lacks access rights for the specified item
(define-constant ERROR-INVALID-NOMENCLATURE (err u303))        ;; Item name does not meet required format standards
(define-constant ERROR-INVALID-DIMENSIONS (err u304))          ;; Item dimensions provided are outside acceptable range
(define-constant ERROR-AUTHENTICATION-FAILED (err u305))       ;; User lacks proper authentication for requested operation
(define-constant ERROR-RECIPIENT-INVALID (err u306))           ;; Target recipient specification is invalid

;; -----------------------------
;; Data Storage Structures
;; -----------------------------
;; Master inventory counter
(define-data-var inventory-count uint u0)

;; Primary inventory database
(define-map inventory-records
  { item-identifier: uint }  ;; Unique identifier for each memorabilia item
  {
    nomenclature: (string-ascii 64),        ;; Official designation of the item
    proprietor: principal,                  ;; Current rightful owner
    dimensions: uint,                       ;; Physical or virtual dimensions specification
    registration-block: uint,               ;; Blockchain timestamp of item registration
    item-synopsis: (string-ascii 128),      ;; Detailed description of the memorabilia item
    categories: (list 10 (string-ascii 32)) ;; Classification categories for search and organization
  }
)

;; Permissions management system
(define-map permissions-registry
  { item-identifier: uint, stakeholder: principal }  ;; Item and user association
  { access-granted: bool }                           ;; Permission status indicator
)

;; -----------------------------
;; Utility Functions
;; -----------------------------
;; Verify existence of item in inventory
(define-private (item-exists-in-records? (item-identifier uint))
  (is-some (map-get? inventory-records { item-identifier: item-identifier }))
)

;; Verify ownership credentials
(define-private (verify-proprietorship? (item-identifier uint) (potential-owner principal))
  (match (map-get? inventory-records { item-identifier: item-identifier })
    record-data (is-eq (get proprietor record-data) potential-owner)
    false
  )
)

;; Validate complete category collection
(define-private (validate-category-collection? (category-set (list 10 (string-ascii 32))))
  (and
    (> (len category-set) u0)                 ;; At least one category required
    (<= (len category-set) u10)               ;; Maximum categories threshold
    (is-eq (len (filter validate-category-format? category-set)) (len category-set))  ;; All categories must pass validation
  )
)

;; Text string bounds validation
(define-private (validate-text-boundaries (content (string-ascii 64)) (minimum-length uint) (maximum-length uint))
  (and 
    (>= (len content) minimum-length)
    (<= (len content) maximum-length)
  )
)

;; Update inventory counter
(define-private (update-inventory-counter)
  (let ((current-total (var-get inventory-count)))
    (var-set inventory-count (+ current-total u1))
    (ok current-total) ;; Return pre-increment value
  )
)

;; Retrieve dimensional specifications
(define-private (extract-item-dimensions (item-identifier uint))
  (default-to u0 
    (get dimensions 
      (map-get? inventory-records { item-identifier: item-identifier })
    )
  )
)

;; Validate category tag format
(define-private (validate-category-format? (category-tag (string-ascii 32)))
  (and 
    (> (len category-tag) u0)     ;; Cannot be empty
    (< (len category-tag) u33)    ;; Must respect maximum length
  )
)



;; -----------------------------
;; Public Interface Functions
;; -----------------------------
;; Register new memorabilia item
(define-public (register-memorabilia (nomenclature (string-ascii 64)) (dimensions uint) (item-synopsis (string-ascii 128)) (categories (list 10 (string-ascii 32))))
  (let
    (
      (new-identifier (+ (var-get inventory-count) u1))  ;; Generate unique identifier
    )
    ;; Input validation procedures
    (asserts! (and (> (len nomenclature) u0) (< (len nomenclature) u65)) ERROR-INVALID-NOMENCLATURE)  ;; Name validation
    (asserts! (and (> dimensions u0) (< dimensions u1000000000)) ERROR-INVALID-DIMENSIONS)            ;; Size validation
    (asserts! (and (> (len item-synopsis) u0) (< (len item-synopsis) u129)) ERROR-INVALID-NOMENCLATURE)  ;; Description validation
    (asserts! (validate-category-collection? categories) ERROR-INVALID-NOMENCLATURE)  ;; Categories validation

    ;; Record creation in inventory
    (map-insert inventory-records
      { item-identifier: new-identifier }
      {
        nomenclature: nomenclature,
        proprietor: tx-sender,
        dimensions: dimensions,
        registration-block: block-height,
        item-synopsis: item-synopsis,
        categories: categories
      }
    )

    ;; Initialize access permissions for creator
    (map-insert permissions-registry
      { item-identifier: new-identifier, stakeholder: tx-sender }
      { access-granted: true }
    )

    ;; Update inventory counter
    (var-set inventory-count new-identifier)
    (ok new-identifier)  ;; Return assigned identifier
  )
)

;; Retrieve item description information
(define-public (retrieve-item-synopsis (item-identifier uint))
  (let
    (
      (record-data (unwrap! (map-get? inventory-records { item-identifier: item-identifier }) ERROR-ITEM-MISSING))
    )
    (ok (get item-synopsis record-data))
  )
)

;; Verify stakeholder access permissions
(define-public (verify-stakeholder-access (item-identifier uint) (stakeholder principal))
  (let
    (
      (permission-data (map-get? permissions-registry { item-identifier: item-identifier, stakeholder: stakeholder }))
    )
    (ok (is-some permission-data))
  )
)

;; Count categories for cataloging purposes
(define-public (count-item-categories (item-identifier uint))
  (let
    (
      (record-data (unwrap! (map-get? inventory-records { item-identifier: item-identifier }) ERROR-ITEM-MISSING))
    )
    (ok (len (get categories record-data)))
  )
)

;; Validate nomenclature format compliance
(define-public (validate-nomenclature-format (nomenclature (string-ascii 64)))
  ;; Confirms nomenclature meets length requirements
  (ok (and (> (len nomenclature) u0) (<= (len nomenclature) u64)))
)

;; Transfer proprietorship between stakeholders
(define-public (transfer-proprietorship (item-identifier uint) (new-proprietor principal))
  (let
    (
      (record-data (unwrap! (map-get? inventory-records { item-identifier: item-identifier }) ERROR-ITEM-MISSING))  ;; Retrieve current record
    )
    (asserts! (item-exists-in-records? item-identifier) ERROR-ITEM-MISSING)  ;; Confirm item exists
    (asserts! (is-eq (get proprietor record-data) tx-sender) ERROR-AUTHENTICATION-FAILED)  ;; Verify current owner

    ;; Update ownership records
    (map-set inventory-records
      { item-identifier: item-identifier }
      (merge record-data { proprietor: new-proprietor })  ;; Update proprietor field
    )
    (ok true)  ;; Operation successful
  )
)

;; Update memorabilia specifications
(define-public (update-memorabilia-details (item-identifier uint) (revised-nomenclature (string-ascii 64)) (revised-dimensions uint) (revised-synopsis (string-ascii 128)) (revised-categories (list 10 (string-ascii 32))))
  (let
    (
      (record-data (unwrap! (map-get? inventory-records { item-identifier: item-identifier }) ERROR-ITEM-MISSING))  ;; Retrieve current record
    )
    ;; Validation suite
    (asserts! (item-exists-in-records? item-identifier) ERROR-ITEM-MISSING)  ;; Confirm item exists
    (asserts! (is-eq (get proprietor record-data) tx-sender) ERROR-AUTHENTICATION-FAILED)  ;; Verify current owner
    (asserts! (and (> (len revised-nomenclature) u0) (< (len revised-nomenclature) u65)) ERROR-INVALID-NOMENCLATURE)  ;; Validate new name
    (asserts! (and (> revised-dimensions u0) (< revised-dimensions u1000000000)) ERROR-INVALID-DIMENSIONS)  ;; Validate new dimensions
    (asserts! (and (> (len revised-synopsis) u0) (< (len revised-synopsis) u129)) ERROR-INVALID-NOMENCLATURE)  ;; Validate new synopsis
    (asserts! (validate-category-collection? revised-categories) ERROR-INVALID-NOMENCLATURE)  ;; Validate new categories

    ;; Update record with new specifications
    (map-set inventory-records
      { item-identifier: item-identifier }
      (merge record-data { 
        nomenclature: revised-nomenclature, 
        dimensions: revised-dimensions, 
        item-synopsis: revised-synopsis, 
        categories: revised-categories 
      })
    )
    (ok true)  ;; Operation successful
  )
)

;; Remove memorabilia from inventory
(define-public (decommission-memorabilia (item-identifier uint))
  (let
    (
      (record-data (unwrap! (map-get? inventory-records { item-identifier: item-identifier }) ERROR-ITEM-MISSING))  ;; Retrieve current record
    )
    (asserts! (item-exists-in-records? item-identifier) ERROR-ITEM-MISSING)  ;; Confirm item exists
    (asserts! (is-eq (get proprietor record-data) tx-sender) ERROR-AUTHENTICATION-FAILED)  ;; Verify current owner

    ;; Remove from active inventory
    (map-delete inventory-records { item-identifier: item-identifier })
    (ok true)  ;; Operation successful
  )
)

