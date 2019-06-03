object ZebraPuzzle {

  case class House(
  	colour: Colour,
  	resident: Resident, 
  	pet: Pet, 
  	brand: Brand, 
  	drink: Drink
  )

  sealed trait Colour
  case object Red extends Colour
  case object Green extends Colour
  case object Yellow extends Colour
  case object Blue extends Colour
  case object Ivory extends Colour

  sealed trait Resident
  case object Englishman extends Resident
  case object Spaniard extends Resident
  case object Ukrainian extends Resident
  case object Norwegian extends Resident
  case object Japanese extends Resident

  sealed trait Pet
  case object Dog extends Pet
  case object Snail extends Pet
  case object Zebra extends Pet
  case object Fox extends Pet
  case object Horse extends Pet

  sealed trait Brand 
  case object OldGold extends Brand
  case object Kools extends Brand
  case object Chesterfield extends Brand
  case object LuckyStrike extends Brand
  case object Parliament extends Brand

  sealed trait Drink
  case object Coffee extends Drink
  case object Tea extends Drink
  case object Milk extends Drink
  case object OrangeJuice extends Drink
  case object Water extends Drink

  case class Solution(waterDrinker: Resident, zebraOwner: Resident)

  lazy val solve: Solution = {

  	val colours = Seq(Red, Green, Yellow, Blue, Ivory)
    val residents = Seq(Englishman, Spaniard, Ukrainian, Norwegian, Japanese)
	val pets = Seq(Dog, Snail, Zebra, Fox, Horse)
	val brands = Seq(OldGold, Kools, Chesterfield, LuckyStrike, Parliament)
	val drinks = Seq(Coffee, Tea, Milk, OrangeJuice, Water)

	def colourRules: Seq[Colour] => Boolean = {
  		l => 
  		val index = l.indexWhere(_ == Ivory)
  		(index != 4 && l(index + 1) == Green)
  	}

  	def residentRules: (Seq[Colour], Seq[Resident]) => Boolean = {
  		(c, r) => r(0) == Norwegian && (c.indexWhere(_ == Red) == r.indexWhere(_ == Englishman))
  				  (r.indexWhere(_ == Norwegian) == c.indexWhere(_ == Blue)+1 ||
  				   r.indexWhere(_ == Norwegian) == c.indexWhere(_ == Blue)-1)
  	}

  	def petRules: (Seq[Resident], Seq[Pet]) => Boolean = {
  		(r, p) => (r.indexWhere(_ == Spaniard) == p.indexWhere(_ == Dog))
  	}

  	def brandRules: (Seq[Colour], Seq[Resident], Seq[Pet], Seq[Brand]) => Boolean = {
  		(c, r, p, b) => (p.indexWhere(_ == Snail) == b.indexWhere(_ == OldGold)) &&
  						(c.indexWhere(_ == Yellow) == b.indexWhere(_ == Kools)) &&
  						(r.indexWhere(_ == Japanese) == b.indexWhere(_ == Parliament)) &&
  						(p.indexWhere(_ == Fox) == b.indexWhere(_ == Chesterfield)+1 ||
  						 p.indexWhere(_ == Fox) == b.indexWhere(_ == Chesterfield)-1) &&
  						(p.indexWhere(_ == Horse) == b.indexWhere(_ == Kools)+1 ||
  						 p.indexWhere(_ == Horse) == b.indexWhere(_ == Kools)-1)
  	}

  	def drinkRules: (Seq[Colour], Seq[Resident], Seq[Brand], Seq[Drink]) => Boolean = {
  		(c, r, b, d) => (d(2) == Milk) &&
  						(d.indexWhere(_ == Coffee) == c.indexWhere(_ == Green)) &&
  						(d.indexWhere(_ == Tea) == r.indexWhere(_ == Ukrainian)) &&
  						(d.indexWhere(_ == OrangeJuice) == b.indexWhere(_ == LuckyStrike))
  	}

  	def getWaterDrinker(houses: Seq[House]): Resident = {
  		houses.filter(h => h.drink == Water).head.resident 
  	}
  	def getZebraOwner(houses: Seq[House]): Resident = {
  		houses.filter(h => h.pet == Zebra).head.resident
  	}

  	lazy val combinations: Seq[Seq[House]] = {
  		for { 
  			c <- colours.permutations.toSeq if colourRules(c)
  			r <- residents.permutations if residentRules(c, r)
  			p <- pets.permutations if petRules(r, p)
  			b <- brands.permutations if brandRules(c, r, p, b)
  			d <- drinks.permutations if drinkRules(c, r, b, d)
  		} yield getHouses(c,r,p,b,d)
  	}

  	def getHouses(newColours: Seq[Colour], newResidents: Seq[Resident], newPets: Seq[Pet], newBrands: Seq[Brand], newDrinks: Seq[Drink]): Seq[House] = {
  		for (i <- 0 to 4) yield House(newColours(i), newResidents(i), newPets(i), newBrands(i), newDrinks(i))
  	}

  	Solution(getWaterDrinker(combinations.head), getZebraOwner(combinations.head))

  }

}

