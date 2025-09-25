{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module TDF.Models where

import           Data.Aeson (ToJSON, FromJSON)
import           GHC.Generics (Generic)
import           Data.Text (Text)
import           Data.Time (UTCTime, Day)
import           Database.Persist
import           Database.Persist.TH
import           Database.Persist.Sql (SqlBackend)

-- Enums
data ServiceKind = Recording | Mixing | Mastering | Rehearsal | Classes | EventProduction
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "ServiceKind"

data PricingModel = Hourly | PerSong | Package | Quote | Retainer
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "PricingModel"

data RoleEnum = Admin | Manager | Engineer | Teacher | Reception | Accounting | Artist | Student | Vendor | ReadOnly | Customer
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "RoleEnum"

data ResourceType = Room | Person | Equipment
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "ResourceType"

data BookingStatus = Tentative | Confirmed | InProgress | Completed | Cancelled | NoShow
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "BookingStatus"

data AttendanceStatus = Enrolled | Attended | Absent | MakeUpNeeded | CompletedA | CancelledA
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "AttendanceStatus"

data UnitsKind = Hours | Lessons | Credits | Sessions
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "UnitsKind"

data RefundPolicy = CreditOnly | Cash | None
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "RefundPolicy"

data PaymentMethod = CashM | BankTransferM | CardPOSM | PayPalM | StripeM | WompiM | PayPhoneM | CryptoM | OtherM
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "PaymentMethod"

data InvoiceStatus = Draft | Sent | PartiallyPaid | Paid | CancelledI
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "InvoiceStatus"

data TicketStatus = OpenT | InProgressT | WaitingParts | DoneT | ClosedT
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "TicketStatus"

data StockTxnReason = Purchase | Consumption | Adjustment | Return | Transfer
  deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "StockTxnReason"

-- Provide JSON instances for enums (via Generic)
instance ToJSON ServiceKind
instance FromJSON ServiceKind
instance ToJSON PricingModel
instance FromJSON PricingModel
instance ToJSON RoleEnum
instance FromJSON RoleEnum
instance ToJSON ResourceType
instance FromJSON ResourceType
instance ToJSON BookingStatus
instance FromJSON BookingStatus
instance ToJSON AttendanceStatus
instance FromJSON AttendanceStatus
instance ToJSON UnitsKind
instance FromJSON UnitsKind
instance ToJSON RefundPolicy
instance FromJSON RefundPolicy
instance ToJSON PaymentMethod
instance FromJSON PaymentMethod
instance ToJSON InvoiceStatus
instance FromJSON InvoiceStatus
instance ToJSON TicketStatus
instance FromJSON TicketStatus
instance ToJSON StockTxnReason
instance FromJSON StockTxnReason

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Party
    legalName        Text Maybe
    displayName      Text
    isOrg            Bool
    taxId            Text Maybe
    primaryEmail     Text Maybe
    primaryPhone     Text Maybe
    whatsapp         Text Maybe
    instagram        Text Maybe
    emergencyContact Text Maybe
    notes            Text Maybe
    createdAt        UTCTime
    deriving Show Generic
PartyRole
    partyId          PartyId
    role             RoleEnum
    active           Bool
    UniquePartyRole  partyId role
    deriving Show Generic
Band
    name             Text
    labelArtist      Bool
    notes            Text Maybe
    deriving Show Generic
BandMember
    bandId           BandId
    partyId          PartyId
    roleInBand       Text Maybe
    startDate        Day Maybe
    endDate          Day Maybe
    UniqueBandMember bandId partyId
    deriving Show Generic
ServiceCatalog
    name             Text
    kind             ServiceKind
    pricingModel     PricingModel
    defaultRateCents Int Maybe
    taxBps           Int Maybe
    active           Bool
    deriving Show Generic
ServiceOrder
    customerId       PartyId
    artistId         PartyId Maybe
    catalogId        ServiceCatalogId
    serviceKind      ServiceKind
    title            Text Maybe
    description      Text Maybe
    status           Text
    priceQuotedCents Int Maybe
    quoteSentAt      UTCTime Maybe
    scheduledStart   UTCTime Maybe
    scheduledEnd     UTCTime Maybe
    createdAt        UTCTime
    deriving Show Generic
ServiceStatusChange
    serviceOrderId   ServiceOrderId
    status           Text
    notes            Text Maybe
    changedBy        PartyId Maybe
    createdAt        UTCTime
    deriving Show Generic
Resource
    name             Text
    slug             Text
    resourceType     ResourceType
    capacity         Int Maybe
    active           Bool
    UniqueResourceSlug slug
    deriving Show Generic
Booking
    title            Text
    serviceOrderId   ServiceOrderId Maybe
    startsAt         UTCTime
    endsAt           UTCTime
    status           BookingStatus
    createdBy        PartyId Maybe
    notes            Text Maybe
    createdAt        UTCTime
    deriving Show Generic
BookingResource
    bookingId        BookingId
    resourceId       ResourceId
    role             Text
    UniqueBookingRes bookingId resourceId role
    deriving Show Generic
Attendance
    bookingId        BookingId
    partyId          PartyId
    status           AttendanceStatus
    notes            Text Maybe
    UniqueAttendance bookingId partyId
    deriving Show Generic
PackageProduct
    name             Text
    serviceKind      ServiceKind
    unitsKind        UnitsKind
    unitsQty         Int
    priceCents       Int
    expiresDays      Int Maybe
    transferable     Bool
    refundPolicy     RefundPolicy
    active           Bool
    deriving Show Generic
PackagePurchase
    buyerId          PartyId
    productId        PackageProductId
    purchasedAt      UTCTime
    priceCents       Int
    expiresAt        UTCTime Maybe
    remainingUnits   Int
    status           Text
    deriving Show Generic
PackageLedger
    purchaseId       PackagePurchaseId
    bookingId        BookingId Maybe
    deltaUnits       Int
    notes            Text Maybe
    createdAt        UTCTime
    deriving Show Generic
Invoice
    customerId       PartyId
    issueDate        Day
    dueDate          Day
    number           Text Maybe
    status           InvoiceStatus
    currency         Text
    subtotalCents    Int
    taxCents         Int
    totalCents       Int
    sriDocumentId    Text Maybe
    notes            Text Maybe
    createdAt        UTCTime
    UniqueInvoiceNumber number !force
    deriving Show Generic
InvoiceLine
    invoiceId        InvoiceId
    serviceOrderId   ServiceOrderId Maybe
    packagePurchaseId PackagePurchaseId Maybe
    description      Text
    quantity         Int
    unitCents        Int
    taxBps           Int
    totalCents       Int
    deriving Show Generic
Payment
    invoiceId        InvoiceId
    method           PaymentMethod
    amountCents      Int
    receivedAt       UTCTime
    reference        Text Maybe
    createdBy        PartyId Maybe
    deriving Show Generic
PaymentSplit
    paymentId        PaymentId
    payerId          PartyId
    amountCents      Int
    deriving Show Generic
Asset
    sku              Text Maybe
    name             Text
    category         Text
    serialNumber     Text Maybe
    purchaseDate     Day Maybe
    purchaseVendor   Text Maybe
    purchaseCostCents Int Maybe
    location         Text Maybe
    condition        Text Maybe
    insured          Bool
    insurancePolicy  Text Maybe
    active           Bool
    UniqueSerial     serialNumber !force
    deriving Show Generic
AssetCheckout
    assetId          AssetId
    bookingId        BookingId Maybe
    partyId          PartyId Maybe
    outAt            UTCTime
    dueAt            UTCTime Maybe
    inAt             UTCTime Maybe
    notes            Text Maybe
    deriving Show Generic
MaintenanceTicket
    assetId          AssetId
    openedAt         UTCTime
    status           TicketStatus
    description      Text
    costCents        Int Maybe
    nextServiceAt    Day Maybe
    deriving Show Generic
StockItem
    sku              Text
    name             Text
    unit             Text
    minLevel         Int Maybe
    reorderPoint     Int Maybe
    vendor           Text Maybe
    active           Bool
    UniqueStockSku   sku
    deriving Show Generic
StockTxn
    stockItemId      StockItemId
    qty              Int
    reason           StockTxnReason
    costCents        Int Maybe
    bookingId        BookingId Maybe
    createdAt        UTCTime
    deriving Show Generic
ExternalCalendarMapping
    resourceId       ResourceId
    googleCalendarId Text
    direction        Text
    UniqueCalMap     resourceId
    deriving Show Generic
AuditLog
    actorId          PartyId Maybe
    entity           Text
    entityId         Text
    action           Text
    diff             Text Maybe
    createdAt        UTCTime
    deriving Show Generic
|]
