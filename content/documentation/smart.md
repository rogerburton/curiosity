---
title: Curiosity
---

# Business specifications

## Big picture

The Smart application

- collects data ;
- records contractual documents ;
- communicates these documents and collects agreements and signatures ;
- processes these documents in the accounts ;
- and produces views, possibly recalculated, for various legal or management
  purposes.Users are commercial parties (sellers, buyers, in the generic sense
  of the term), data entry operators, advisors and supervisors, but also
  recipients of legally required reports.

As standard, Smart operates through its business units - which will always be
referenced in an agreement and in accounting.

These units (several thousand) are autonomous, and are driven by users who
often have no knowledge of commercial or accounting management.

Business processes are determined by the actors' place in the Smart eco-system,
from which their rights are specified, and by multiple deadlines and dates
usually set by law.

## A standardized framework

The framework used is the [UBL standard (v
2.3)](https://docs.oasis-open.org/ubl/UBL-2.3.html). Invoice, CreditNote and
DebitNote messages must be interoperable with the [Peppol
standard](https://docs.peppol.eu/poacc/billing/3.0/bis/).

List of most used messages in business process : commitment flux (steps 1-4)
and fulfillment flux (5-10).

| Step | Seller              | Buyer               |
| ---- | ------------------- | ------------------- |
| 1    | Quotation           |                     |
| 2    |                     | ApplicationResponse |
| 2    |                     | Order               |
| 3    | OrderResponseSimple |                     |
| 3    | OrderResponse       |                     |
| 4    |                     | OrderChange         |
| 4    |                     | OrderCancellation   |
| 5    | DespatchAdvice      |                     |
| 6    |                     | ReceiptAdvice       |
| 6    |                     | ApplicationResponse |
| 7    | Invoice             |                     |
| 7    | CreditNote          |                     |
| 7    | DebitNote           |                     |
| 8    | RemittanceAdvice    | RemittanceAdvice    |
| 9    | Statement           |                     |
| 10   | Reminder            |                     |

And : DocumentStatusRequest, DocumentStatus, Enquiry, EnquiryResponse,
ExceptionCriteria, ExceptionNotification.

All actors are referenced in the **Party** object, considered in the Smart
application as a UBL document. This object provides the schema of their data
sheet and the relationships they have with each other.

Each document recorded at time T reflects **<u>the complete state</u>** at that
time of a transaction. The Smart application is data-driven.

The documents are immutable. They can only be created and viewed, but never
updated or deleted. Before the creation of a document, the data are only "*form
data*".

A document is always issued: first created, then sent to one or more recipents
. The issuer of a document is always presumed to have accepted its terms.

The business rules concern (almost) only the creation of a document. And the
life cycle of the business process in which this document takes place. The
concept of deadline is essential in a business process.

## The Happy Path of a business transaction (sale)

- Seller : Smart entity
- Agent of Seller : Business unit
- Buyer : any third party

<u>**START business process (life cycle)**</u>

Step 1 : **INITIALIZATION**

The seller registers and sends an quotation to the buyer.

> The application checks before saving the document whether the data in the
> form is well formed, complete and complies with business requirements.

Step 2 : **ORDERING** (commitment)

The buyer accepts the quotation (ApplicationResponse), creates a purchase order
and sends it. The seller accepts the order form and informs the buyer
(OrderResponseSimple).

Step 3 : **DELIVERY** (fulfillment)

The order is delivered (DispatchAdvice). The buyer confirms receipt of the
order (ReceiptAdvice).

Step 4 : **INVOICING** (fulfillment)

The seller registers an invoice and sends it to the buyer.

Smart entity registers an internal credit note (shared costs) and sends it to
the business unit.

Smart entity registers an internal  credit note (VAT management) and sends it
to the business unit.

>  The accounting interpreter processes the invoice and the two credit notes.

Step 5 : **PAYMENT** (fulfillment)

The buyer pays the invoice and the seller's bank issues a bank statement
(RemittanceAdvice).

The Smart entity credits the business unit's bank account with the amount paid
by the buyer: the Smart entity's internal bank issues a debit bank statement
and the business unit's internal bank issues a credit bank statement.

The business unit pays the Smart entity the two credit notes received. Its
internal bank issues two debit bank statements, and the Smart entity's internal
bank issues two credit bank statements.

>  The accounting interpreter processes the seven bank statements
>  (RemittanceAdvice).

> At the end of the month, a VAT report is issued to the tax authorities (this
> report can be seen as a SelfBilledCreditNote or a DebitNote), and the VAT due
> to the State is paid by the Smart entity, which receives a debit bank
> statement from its bank. The accounting interpreter processes the VAT report
> and the payment to the State.

<u>**END business process (life cycle)**</u>
