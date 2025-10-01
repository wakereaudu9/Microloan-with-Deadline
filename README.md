# 💰 Microloan with Deadline Smart Contract

A decentralized microloan platform built on Stacks blockchain that enables peer-to-peer lending with automatic deadline enforcement and collateral reclaim functionality.

## 🚀 Features

- 📝 **Create Loans**: Lenders can create loan offers for specific borrowers
- 💸 **Fund Loans**: Automatic STX transfer to borrowers upon funding
- ⏰ **Deadline Management**: Built-in deadline enforcement with block-height precision
- 🔄 **Repayment System**: Secure loan repayment with automatic state updates
- ⚡ **Default Claims**: Lenders can claim defaults after deadline expiration
- 📊 **Loan Tracking**: Comprehensive loan status and user activity tracking
- 🛡️ **State Machine**: Robust loan lifecycle management (Created → Funded → Repaid/Defaulted)

## 🎯 Contract States

| State | Value | Description |
|-------|-------|-------------|
| `CREATED` | 1 | Loan created but not yet funded |
| `FUNDED` | 2 | Loan funded and active |
| `REPAID` | 3 | Loan successfully repaid |
| `DEFAULTED` | 4 | Loan defaulted after deadline |

## 🔧 Core Functions

### 📋 Public Functions

#### `create-loan`
```clarity
(create-loan (borrower principal) (amount uint) (deadline uint))
```
Creates a new loan offer with specified borrower, amount, and deadline block height.

#### `fund-loan`
```clarity
(fund-loan (loan-id uint))
```
Funds an existing loan by transferring STX to the borrower.

#### `repay-loan`
```clarity
(repay-loan (loan-id uint))
```
Allows borrowers to repay loans before the deadline.

#### `claim-default`
```clarity
(claim-default (loan-id uint))
```
Enables lenders to claim loan defaults after deadline expiration.

#### `extend-deadline`
```clarity
(extend-deadline (loan-id uint) (new-deadline uint))
```
Allows lenders to extend loan deadlines for active loans.

### 📖 Read-Only Functions

#### `get-loan-status`
Returns comprehensive loan information including timing and status details.

#### `get-user-loans`
Retrieves all loan IDs associated with a specific user.

#### `get-contract-stats`
Provides platform-wide statistics including total loans and amounts.

## 🛠️ Usage Examples

### Creating a Loan
```bash
clarinet console
```

```clarity
(contract-call? .microloan-with-deadline create-loan 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG u1000000 u1000)
```

### Funding a Loan
```clarity
(contract-call? .microloan-with-deadline fund-loan u1)
```

### Repaying a Loan
```clarity
(contract-call? .microloan-with-deadline repay-loan u1)
```

### Checking Loan Status
````clarity
(contract-call? .microloan-with-deadline get-

