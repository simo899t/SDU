package Exercise4;

public class Point2D {
    
    private double x;
    private double y;

    public static void main(String[] args) {
        
    }

    /**
     * Constructor for a point in 2D space.
     * @param x
     * @param y
     */
    public Point2D(double x, double y) {
        this.x = x;
        this.y = y;
    }

    /**
     * Get the x coordinate of the point.
     * @return
     */
    public double getx() {
        return this.x;
    }

    /**
     * Get the y coordinate of the point.
     * @return
     */
    public double gety() {
        return this.y;
    }

    /**
     * Set the x coordinate of the point.
     * @param x
     */
    public void setx(double x) {
        this.x = x;
    }

    /**
     * Set the y coordinate of the point.
     * @param y
     */
    public void sety(double y) {
        this.y = y;
    }

    /**
     * Checks if the point is the origin (0,0).
     * @return
     */
    public Boolean isOrigin() {
        return getx() == 0 && gety() == 0;
    }

    /**
     * Moves the point by the vector <deltaX, deltaY>.
     * @param deltaX
     * @param deltaY
     */
    public void move(double deltaX, double deltaY) {
        setx(getx()+deltaX);
        sety(gety()+deltaY);
    }

    /**
     * Calculates the distance between this.point to the origin (0,0).
     * @param p
     * @return
     */
    public double distanceToOrigin() {
        return Math.sqrt(Math.pow(getx(), 2) + Math.pow(gety(), 2));
    }

    /**
     * Calculates the distance between this.point to another point p.
     * @param p
     * @return
     */
    public double distanceTo(Point2D p) {
        return Math.sqrt(Math.pow(getx()-p.getx(), 2) + Math.pow(gety()-p.gety(), 2));
    }


    /**
     * Returns a new point with the same coordinates as this.point.
     * @return
     */
    public Point2D clone() {
        Point2D result = new Point2D(getx(), gety());
        return result;
    }

    /**
     * Returns the point as a string.
     */
    public String toString() {
        return "(" + getx() + "," + gety() + ")";
    }
}

