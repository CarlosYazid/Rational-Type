class Rational(num : Int, denom : Int) {
  var numerator : Int = num
  var denominator : Int = denom
  val reduced : Boolean = reduce()
  val sign = (numerator > 0 && denominator > 0) || (numerator < 0 && denominator < 0)
  
  def negative(): Rational = if (denom < 0) new Rational(numerator, -denominator) else new Rational(-numerator, denominator)  
  def toDouble(): Double = numerator.toDouble/denominator.toDouble
  def toFloat(): Float = numerator.toFloat/denominator.toFloat
  def toInt(): Int = numerator/denominator
  def toLong(): Long = numerator.toLong/denominator.toLong

  override def toString(): String = {
    if (denominator == 1) then s"$numerator"
    else if (denominator == -1) then s"${-numerator}"
    else if (sign) s"$numerator/$denominator" 
    else {
      if numerator < 0 then s"-(${-numerator}/$denominator)" else s"-($numerator/${-denominator})"
    }
  }
  
  override def equals(that: Any): Boolean = this.hashCode() == that.hashCode()
  
  private def reduce(): Boolean = {
    var num_ : BigInt = numerator
    var denom_ : BigInt = denominator
    var gcd_ = num_.gcd(denom_)

    if (gcd_ != 1) {
    
      while (gcd_ != 1) {
      
        num_ = num_ / gcd_
        denom_ = denom_ / gcd_
        gcd_ = num_.gcd(denom_)
        
      }
      
      this.numerator = num_.toInt
      this.denominator = denom_.toInt
      false
      
    } else {
    
      true
      
    }    
  }  

  def +(other : Rational): Rational = new Rational((numerator * other.denominator + denominator * other.numerator),(denominator * other.denominator))
  def -(other : Rational): Rational = new Rational((numerator * other.denominator - denominator * other.numerator),(denominator * other.denominator))
  def *(other : Rational): Rational = new Rational((numerator * other.numerator),(denominator * other.denominator))
  def /(other : Rational): Rational = new Rational((numerator * other.denominator),(denominator * other.numerator))

  def +(num : Int): Rational = new Rational((numerator + denominator * num),denominator)
  def -(num : Int): Rational = new Rational((numerator - denominator * num),denominator)
  def *(num : Int): Rational = new Rational((numerator * num),denominator)
  def /(num : Int): Rational = new Rational(numerator,(denominator * num))

  def +(num : BigInt): Rational = new Rational((numerator + denominator * num.toInt),denominator)
  def -(num : BigInt): Rational = new Rational((numerator - denominator * num.toInt),denominator)
  def *(num : BigInt): Rational = new Rational((numerator * num.toInt),denominator)
  def /(num : BigInt): Rational = new Rational(numerator,(denominator * num.toInt))

  def +(num : Long): Rational = new Rational((numerator + denominator * num.toInt),denominator)
  def -(num : Long): Rational = new Rational((numerator - denominator * num.toInt),denominator)
  def *(num : Long): Rational = new Rational((numerator * num.toInt),denominator)
  def /(num : Long): Rational = new Rational(numerator,(denominator * num.toInt))

  def +(num : Short): Rational = new Rational((numerator + denominator * num.toInt),denominator)
  def -(num : Short): Rational = new Rational((numerator - denominator * num.toInt),denominator)
  def *(num : Short): Rational = new Rational((numerator * num.toInt),denominator)
  def /(num : Short): Rational = new Rational(numerator,(denominator * num.toInt))

  def +(num : Byte): Rational = new Rational((numerator + denominator * num.toInt),denominator)
  def -(num : Byte): Rational = new Rational((numerator - denominator * num.toInt),denominator)
  def *(num : Byte): Rational = new Rational((numerator * num.toInt),denominator)
  def /(num : Byte): Rational = new Rational(numerator,(denominator * num.toInt))
  
  def ==(that: Rational): Boolean = (numerator * that.denominator == denominator * that.numerator)
  def <=(that: Rational): Boolean = (numerator * that.denominator <= denominator * that.numerator)
  def <(that: Rational): Boolean = (numerator * that.denominator < denominator * that.numerator)
  def >(that: Rational): Boolean = (numerator * that.denominator > denominator * that.numerator)
  def >=(that: Rational): Boolean = (numerator * that.denominator >= denominator * that.numerator)
  
  def ==(num: Int): Boolean = (numerator == denominator * num)
  def <=(num: Int): Boolean = (numerator <= denominator * num)  
  def <(num: Int): Boolean = (numerator < denominator * num)
  def >(num: Int): Boolean = (numerator > denominator * num)  
  def >=(num: Int): Boolean = (numerator >= denominator * num)

  def ==(num: BigInt): Boolean = (numerator == denominator * num.toInt)
  def <=(num: BigInt): Boolean = (numerator <= denominator * num.toInt)  
  def <(num: BigInt): Boolean = (numerator < denominator * num.toInt)
  def >(num: BigInt): Boolean = (numerator > denominator * num.toInt)  
  def >=(num: BigInt): Boolean = (numerator >= denominator * num.toInt)

  def ==(num: Long): Boolean = (numerator == denominator * num.toInt)
  def <=(num: Long): Boolean = (numerator <= denominator * num.toInt)  
  def <(num: Long): Boolean = (numerator < denominator * num.toInt)
  def >(num: Long): Boolean = (numerator > denominator * num.toInt)  
  def >=(num: Long): Boolean = (numerator >= denominator * num.toInt)

  def ==(num: Byte): Boolean = (numerator == denominator * num.toInt)
  def <=(num: Byte): Boolean = (numerator <= denominator * num.toInt)  
  def <(num: Byte): Boolean = (numerator < denominator * num.toInt)
  def >(num: Byte): Boolean = (numerator > denominator * num.toInt)  
  def >=(num: Byte): Boolean = (numerator >= denominator * num.toInt)

  def ==(num: Short): Boolean = (numerator == denominator * num.toInt)
  def <=(num: Short): Boolean = (numerator <= denominator * num.toInt)  
  def <(num: Short): Boolean = (numerator < denominator * num.toInt)
  def >(num: Short): Boolean = (numerator > denominator * num.toInt)  
  def >=(num: Short): Boolean = (numerator >= denominator * num.toInt) 
}