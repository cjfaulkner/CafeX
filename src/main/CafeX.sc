type PriceType = BigDecimal
type TaxRateType = BigDecimal
type BillType = List[String]
type MenuItem = (String, PriceType, TaxRateType)

val menu : List[MenuItem] =
List(
    ("Coke", 0.5, 0),
    ("Coffee", 1, 0),
      ("Cheese Sandwich", 2, 0.1),
        ("Steak Sandwich", 4.5, 0.2),
  )

def identifyItem(item : String, iterMenu : List[MenuItem]) : MenuItem = {
  if (iterMenu.isEmpty) throw new Error("Item Not Found '" + item + "'")
  if(item == iterMenu.head._1) iterMenu.head
  else identifyItem(item, iterMenu.tail)
}

def findItemPrice (item : String): PriceType =
{
   identifyItem(item, menu)._2
}

def findItemTaxRate (item : String): TaxRateType =
{
  identifyItem(item, menu)._3
}

def addBill (bill : BillType) : PriceType = {
  if (bill.isEmpty) 0
  else findItemPrice(bill.head) + addBill(bill.tail)
}

def getMaxTaxRate(bill : BillType) : TaxRateType =
{
  if (bill.tail.isEmpty) findItemTaxRate(bill.head)
  else
    {
      val thisItemTax : TaxRateType = findItemTaxRate(bill.head)
      val maxOtherTax : TaxRateType = getMaxTaxRate(bill.tail)
      if (thisItemTax > maxOtherTax) thisItemTax
      else maxOtherTax
    }
}
//val order : List[String]  = List("Coke", "apple")
//val order : List[String]  = List("Coke", "Coffee", "Cheese Sandwich")

//val order : List[String]  = List("Coke", "Coffee", "Steak Sandwich", "Cheese Sandwich", "Coke")
//val order : List[String]  = List("Coke", "Steak Sandwich", "Coffee", "Steak Sandwich", "Cheese Sandwich", "Coke")


val order : BillType  = List("Steak Sandwich", "Coffee",
  "Steak Sandwich", "Coffee",
  "Steak Sandwich", "Coffee",
  "Steak Sandwich", "Coffee",
"Steak Sandwich", "Coffee",
"Steak Sandwich", "Coffee",
"Steak Sandwich", "Coffee",
"Steak Sandwich", "Coffee",
"Steak Sandwich", "Coffee",
"Steak Sandwich", "Coffee",
"Steak Sandwich", "Coffee",
"Steak Sandwich", "Coffee",
"Steak Sandwich", "Coffee",
"Steak Sandwich", "Coffee",
"Steak Sandwich", "Coffee",
"Steak Sandwich", "Coffee",
"Steak Sandwich", "Coffee",
"Steak Sandwich", "Coffee",
"Steak Sandwich", "Coffee",
"Steak Sandwich", "Coffee"
)



val netCost : PriceType = addBill(order)
val maxTaxRate : TaxRateType = getMaxTaxRate(order)
var totalTax = netCost * maxTaxRate
if (totalTax > 20) totalTax = 20
else totalTax = totalTax.setScale(2, BigDecimal.RoundingMode.HALF_UP)

println("Net Cost : " + netCost)
println("Total Tax : " + totalTax)
println("Total : " + (netCost + totalTax))
