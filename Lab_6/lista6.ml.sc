// Rafał Kruszyna

//Zadanie 1

def whileLoop(condition: =>Boolean)(expression: =>Unit):Unit={
  if condition then {
    expression
    whileLoop(condition)(expression)
  }
}
var count = 0
while count < 3 do
  println(count)
  count += 1