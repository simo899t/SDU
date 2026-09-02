package Exercise4;

public class Fraction {
    private int numerator;
    // Denominator cannot be 0
    private int denominator;

    public static void main(String[] args) {
        Fraction fraction1 = new Fraction();
        Fraction fraction2 = new Fraction(3);
        Fraction fraction3 = new Fraction(3, 4);

        fraction3 = fraction3.add(fraction3);
        System.out.println(fraction3.toString());
        fraction3.simplify();
        System.out.println(fraction3.toString());
    }

    /**
     * Default constructor for 1/1
     */
    public  Fraction() {
        this.numerator = 1;
        this.denominator = 1;
    }

    /**
     * Constructor for n/1 (a whole number)
     * @param n
     */
    public  Fraction(int n) {
        
    }

    /**
     * Constructor for n/m
     * @param n
     * @param m
     */
    public  Fraction(int n, int m) {
        this.numerator = n;
        this.denominator = m;
    }

    /**
     * Get the numerator of the fraction
     * @return
     */
    public int getNumerator() {
        return this.numerator;
    }

    /**
     * Get the denominator of the fraction
     * @return
     */
    public int getDenominator() {
        return this.denominator;
    }

    /**
     * Set the numerator of the fraction as n
     * @param n
     */
    public void setNumerator(int n) {
        this.numerator = n;
    }

    /**
     * Set the denominator of the fraction as m
     * @param m
     */
    public void setDenominator(int m) {
        this.denominator = m;
    }

    /**
     * Return the fraction as a string for printing
     * @return
     */
    public String toString() {
        return this.numerator + "/" + this.denominator;
    }

    /**
     * Adds two fractions (fraction1.add(fraction2))
     * @param f
     * @return
     */
    public Fraction add(Fraction f) {
        Fraction result = new Fraction();
        result.numerator = getNumerator()*f.getDenominator() + f.getNumerator()*getDenominator();
        result.denominator = getDenominator()*f.getDenominator();
        return result;
    }

    /**
     * Subtracts two fractions (fraction1.subtract(fraction2))
     * @param f
     * @return
     */
    public Fraction subtract(Fraction f) {
        Fraction result = new Fraction();
        result.numerator = getNumerator()*f.getDenominator() - f.getNumerator()*getDenominator();
        result.denominator = getDenominator()*f.getDenominator();
        return result;
    }

    /**
     * Multiplies two fractions (fraction1.multiply(fraction2))
     * @param f
     * @return
     */
    public Fraction multiply(Fraction f) {
        Fraction result = new Fraction();
        result.numerator = getNumerator()*f.getNumerator();
        result.denominator = getDenominator()*f.getDenominator();
        return result;
    }

    /**
     * Divides two fractions (fraction1.divide(fraction2))
     * @param f
     * @return
     */
    public Fraction divide(Fraction f) {
        Fraction result = new Fraction();
        result.numerator = getNumerator()*f.getDenominator();
        result.denominator = getDenominator()*f.getNumerator();
        return result;
    }
    
    /**
     * Find the greatest common divisor of two numbers
     * @param a
     * @param b
     * @return
     */
    public int gcd(int a, int b) {
        return a == 0 ? b : gcd(b % a, a);
    }

    /**
     * Simplify the fraction
     */
    public void simplify() {
        int gcd = gcd(getNumerator(), getDenominator());
        while (gcd != 1) {
            this.numerator /= gcd;
            this.denominator /= gcd;
            gcd = gcd(getNumerator(), getDenominator());
        }
    }

    /**
     * Return the value of the fraction as a double
     * @return
     */
    public double value() {
        return ((double) getNumerator()) / getDenominator();
    }

    /**
     * Return the integer part of the fraction
     * @return
     */
    public int intergerPart() {
        return getNumerator() / getDenominator();
    }

    /**
     * Return the deciaml part of the fraction as a fraction
     * @return
     */
    public Fraction properPart() {
        Fraction result = new Fraction();
        result.numerator = getNumerator() % getDenominator();
        return result;
    }

    /**
     * Clones the fraction
     * @return
     */
    public Fraction clone() {
        Fraction result = new Fraction(getNumerator(),getDenominator());
        return result;
    }
}
