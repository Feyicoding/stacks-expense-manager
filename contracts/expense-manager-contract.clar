
;; expense-manager-contract
;; A smart contract for managing expenses on the Stacks blockchain
;; Allows users to create, track, and manage expenses with categories and approval workflows

;; constants
;;
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-EXPENSE-NOT-FOUND (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-INVALID-CATEGORY (err u103))
(define-constant ERR-ALREADY-APPROVED (err u104))
(define-constant ERR-ALREADY-REJECTED (err u105))
(define-constant ERR-CATEGORY-EXISTS (err u106))
(define-constant ERR-CATEGORY-NOT-FOUND (err u107))
(define-constant ERR-BUDGET-EXCEEDED (err u108))
(define-constant ERR-INVALID-DATE (err u109))

;; data maps and vars
;;

;; Expense status: 0=pending, 1=approved, 2=rejected
(define-data-var next-expense-id uint u1)
(define-data-var next-category-id uint u1)
(define-data-var admin principal tx-sender)

;; Expense data structure
(define-map expenses uint {
    creator: principal,
    amount: uint,
    description: (string-ascii 100),
    category-id: uint,
    date: uint,
    status: uint,
    approver: (optional principal),
    notes: (optional (string-ascii 100))
})

;; Category data structure
(define-map categories uint {
    name: (string-ascii 50),
    budget: uint,
    description: (string-ascii 100),
    created-by: principal
})

;; Track total spent per category
(define-map category-spent uint uint)

;; Track expenses by user
(define-map user-expenses principal (list 100 uint))

;; private functions
;;

;; Get the next expense ID and increment the counter
(define-private (get-next-expense-id)
    (let ((current-id (var-get next-expense-id)))
        (var-set next-expense-id (+ current-id u1))
        current-id
    )
)

;; Get the next category ID and increment the counter
(define-private (get-next-category-id)
    (let ((current-id (var-get next-category-id)))
        (var-set next-category-id (+ current-id u1))
        current-id
    )
)

;; Add expense ID to user's expense list
(define-private (add-expense-to-user (expense-id uint) (user principal))
    (let ((current-expenses (default-to (list) (map-get? user-expenses user))))
        ;; Always set the list with the new expense ID
        ;; If list is full, we'll just overwrite it with a new list containing only the new ID
        (if (< (len current-expenses) u100)
            (map-set user-expenses user 
                (unwrap-panic (as-max-len? (append current-expenses expense-id) u100)))
            (map-set user-expenses user (list expense-id))
        )
    )
)

;; Update category spent amount
(define-private (update-category-spent (category-id uint) (amount uint))
    (let ((current-spent (default-to u0 (map-get? category-spent category-id))))
        (map-set category-spent category-id (+ current-spent amount))
    )
)

;; Check if category exists
(define-private (category-exists? (category-id uint))
    (is-some (map-get? categories category-id))
)

;; Check if expense exists
(define-private (expense-exists? (expense-id uint))
    (is-some (map-get? expenses expense-id))
)

;; Check if user is admin
(define-private (is-admin)
    (is-eq tx-sender (var-get admin))
)

;; Check if budget is exceeded
(define-private (budget-exceeded? (category-id uint) (amount uint))
    (let (
        (category-info (unwrap! (map-get? categories category-id) false))
        (current-spent (default-to u0 (map-get? category-spent category-id)))
    )
        (> (+ current-spent amount) (get budget category-info))
    )
)

;; public functions
;;

;; Create a new expense
(define-public (create-expense (amount uint) (description (string-ascii 100)) (category-id uint) (date uint))
    (begin
        ;; Validate inputs
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (category-exists? category-id) ERR-CATEGORY-NOT-FOUND)
        (asserts! (> date u0) ERR-INVALID-DATE)
        
        (let ((expense-id (get-next-expense-id)))
            ;; Create the expense
            (map-set expenses expense-id {
                creator: tx-sender,
                amount: amount,
                description: description,
                category-id: category-id,
                date: date,
                status: u0, ;; pending
                approver: none,
                notes: none
            })
            
            ;; Add to user's expense list
            (add-expense-to-user expense-id tx-sender)
            
            (ok expense-id)
        )
    )
)

;; Approve an expense
(define-public (approve-expense (expense-id uint) (notes (optional (string-ascii 100))))
    (let ((expense (unwrap! (map-get? expenses expense-id) ERR-EXPENSE-NOT-FOUND)))
        ;; Validate
        (asserts! (or (is-admin) (is-eq tx-sender (get creator expense))) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status expense) u0) ERR-ALREADY-APPROVED)
        
        ;; Update expense status
        (map-set expenses expense-id (merge expense {
            status: u1, ;; approved
            approver: (some tx-sender),
            notes: notes
        }))
        
        ;; Update category spent
        (update-category-spent (get category-id expense) (get amount expense))
        
        (ok true)
    )
)

;; Reject an expense
(define-public (reject-expense (expense-id uint) (notes (optional (string-ascii 100))))
    (let ((expense (unwrap! (map-get? expenses expense-id) ERR-EXPENSE-NOT-FOUND)))
        ;; Validate
        (asserts! (or (is-admin) (is-eq tx-sender (get creator expense))) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status expense) u0) ERR-ALREADY-REJECTED)
        
        ;; Update expense status
        (map-set expenses expense-id (merge expense {
            status: u2, ;; rejected
            approver: (some tx-sender),
            notes: notes
        }))
        
        (ok true)
    )
)

;; Create a new category
(define-public (create-category (name (string-ascii 50)) (budget uint) (description (string-ascii 100)))
    (begin
        (let ((category-id (get-next-category-id)))
            ;; Create the category
            (map-set categories category-id {
                name: name,
                budget: budget,
                description: description,
                created-by: tx-sender
            })
            
            ;; Initialize spent amount
            (map-set category-spent category-id u0)
            
            (ok category-id)
        )
    )
)

;; Update category budget
(define-public (update-category-budget (category-id uint) (new-budget uint))
    (let ((category (unwrap! (map-get? categories category-id) ERR-CATEGORY-NOT-FOUND)))
        ;; Validate
        (asserts! (or (is-admin) (is-eq tx-sender (get created-by category))) ERR-NOT-AUTHORIZED)
        
        ;; Update category
        (map-set categories category-id (merge category {
            budget: new-budget
        }))
        
        (ok true)
    )
)

;; Get expense details
(define-read-only (get-expense (expense-id uint))
    (map-get? expenses expense-id)
)

;; Get category details
(define-read-only (get-category (category-id uint))
    (map-get? categories category-id)
)

;; Get category spent amount
(define-read-only (get-category-spent (category-id uint))
    (default-to u0 (map-get? category-spent category-id))
)

;; Get user's expenses
(define-read-only (get-user-expenses (user principal))
    (default-to (list) (map-get? user-expenses user))
)

;; Change admin
(define-public (set-admin (new-admin principal))
    (begin
        (asserts! (is-admin) ERR-NOT-AUTHORIZED)
        (var-set admin new-admin)
        (ok true)
    )
)
