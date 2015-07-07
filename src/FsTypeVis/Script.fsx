#load "Simple_type.fs"
#load "Graphics.fs"
#load "Visualisation.fs"
open FsTypeVis

module Example1 =
    type Customer = 
        { name : string
          location : string }

    type NormalOrder = 
        { date : System.DateTime
          number : string
          customer : Customer }

    type SpecialOrder = 
        { date : System.DateTime
          number : string
          customer : Customer }

    type Order = 
        | Normal of NormalOrder
        | Special of SpecialOrder

    type Confirm = 
        | Confirm of (Order -> Order)

    type Close = 
        | Close of (Order -> Order)

    type Dispatch = 
        | Dispatch of ((Order -> Order) -> Order)

    type Receive = 
        | Receive of (SpecialOrder -> Set<SpecialOrder>)
    type Dummy = Dispatch*Receive*Confirm*Close
module Example2 =
    type Date = System.DateTime

    // == Customer related ==

    type Customer = {
        name:string
        address:string
        }

    // == Item related ==

    type [<Measure>] grams

    type Item = {
        shippingWeight: int<grams>
        description: string
        }

    type Qty = int
    type Price = decimal


    // == Payment related ==

    type PaymentMethod = 
        | Cash
        | Credit of number:string * cardType:string * expDate:Date
        | Check of name:string * bankID: string

    type Payment = {
        amount: decimal
        paymentMethod : PaymentMethod 
        }

    // == Order related ==

    type TaxStatus = Taxable | NonTaxable
    type Tax = decimal

    type OrderDetail = {
        item: Item
        qty: int
        taxStatus : TaxStatus
        }
    
    type OrderStatus = Open | Completed

    type Order = {
        date: System.DateTime; 
        customer: Customer
        status: OrderStatus
        lines: OrderDetail list
        payments: Payment list
        }

    // ==================================
    // activities / use-cases / scenarios
    // ==================================
    type GetPriceForQuantity = GetPriceForQuantity of (Item -> Qty -> Price)

    type CalcTax = CalcTax of (Order -> Tax)
    type CalcTotal = CalcTotal of (Order -> Price)
    type CalcTotalWeight = CalcTotalWeight of (Order -> int<grams>)

    type Dummy = Date * Customer * Item * Qty * Price  * PaymentMethod * Payment * TaxStatus * Tax * OrderDetail * OrderStatus * Order * GetPriceForQuantity * CalcTax * CalcTotal * CalcTotalWeight


let ex1() =
    let myTypeMap = Simple_type.Make.typeMap [typeof<Example1.Dummy>]
    let myVisMap = Visualisation.visTypeMap myTypeMap Visualisation.Palettes.bassCss
    Graphics.Rendering.Html.renderToFile @"C:\Users\Sergey\Documents\test2.html" (myVisMap |> Map.toSeq |> Seq.map snd)

let ex2() =
    let myTypeMap = Simple_type.Make.typeMap [typeof<Example2.Dummy>]
    let myVisMap = Visualisation.visTypeMap myTypeMap Visualisation.Palettes.bassCss
    Graphics.Rendering.Html.renderToFile @"C:\Users\Sergey\Documents\test3.html" (myVisMap |> Map.toSeq |> Seq.map snd)

ex1 ();;
ex2 ();;