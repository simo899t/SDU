package Exercise4;
import Exercise3.*;
import static Exercise3.imageUtils.stretchHorizontal;
import static Exercise3.imageUtils.stretchVertical;


public class Polygon {

    private Point2D[] vertices;
    public static void main(String[] args) {
        
        Point2D[] points = {new Point2D(5,20), new Point2D(20,60), new Point2D(50,70), new Point2D(90,10)};
        Polygon square1 = new Polygon(points);
        //Polygon square2 = square1.clone();
        //square1.move(4, 4);
        System.out.println(square1.PolyToString());
        
        //System.out.println(square1.id()=square2.id());
        //System.out.println(square1.perimeter());
        //System.out.println(square1.nearest());
        //System.out.println(square1.longetsSide());
        //System.out.println(square1.isTriange());
        //System.out.println(square1.isRectangle());
        Image img = new Image(100, 100, Color.WHITE);
        drawVertices(img, square1, new Color(255, 0, 0));
        drawSides(img, square1, new Color(153,50,204));
        enhanceVertices(img, square1, new Color(255, 0, 0));
        for (int i = 0; i < 3; i++) {
            img = stretchHorizontal(img);
            img = stretchVertical(img);
        }
        img.display();
    }

    /**
     * Constructor for a polygon with a given set of vertices.
     * @param vertices
     */
    public Polygon(Point2D[] vertices) {
        this.vertices = vertices;
    }

    /**
     * Get the number of vertices in the polygon.
     * @return
     */
    public int getNumVertices() {
        return this.vertices.length;
    }

    /**
     * Set the vertice at the specified index.
     * @param index
     * @param point
     */
    public void setVertice(int index, Point2D point) {
        this.vertices[index] = point;
    }

    /**
     * Get the vertice at the specified index.
     * @param index
     * @return
     */
    public Point2D getVertice(int index) {
        return this.vertices[index];
    }

    /**
     * Calculates the distance between point p to another point q.
     * @param p
     * @return
     */
    public double distanceTo(Point2D p, Point2D q) {
        return Math.sqrt(Math.pow(q.getx()-p.getx(), 2) + Math.pow(q.gety()-p.gety(), 2));
    }

    /**
     * Calculates the perimeter of the polygon.
     * @return
     */
    public double perimeter() {
        double perimeter = 0;
        for (int i = 0; i < this.vertices.length; i++) {
            if (i == this.vertices.length-1) {
                perimeter += distanceTo(this.vertices[i], this.vertices[0]);
            } else {
                perimeter += distanceTo(this.vertices[i], this.vertices[i+1]);
            }
        }
        return perimeter;
    }

    /**
     * Draws the vertices of the polygon on the image.
     * @param image
     * @param polygon
     * @param color
     */
    public static void drawVertices(Image image, Polygon polygon, Color color) {
        for (int i = 0; i < polygon.getNumVertices(); i++) {
            image.setPixel((int)polygon.getVertice(i).getx(), (int)polygon.getVertice(i).gety(), color);
        }
    }

    /**
     * Returns the vertice that is nearest to the origin.
     * @return
     */
    public Point2D nearest() {
        double minDistance = vertices[0].distanceToOrigin();
        Point2D nearest = vertices[0];
        for (int i = 1; i < vertices.length; i++) {
            if (vertices[i].distanceToOrigin() < minDistance) {
                minDistance = vertices[i].distanceToOrigin();
                nearest = vertices[i];
            }
        }
        return nearest;
    }

    /**
     * Returns the longest side of the polygon.
     * @return
     */
    public double longetsSide() {
        double longestSide = distanceTo(vertices[0], vertices[1]);
        for (int i = 1; i < vertices.length-1; i++) {
            if (vertices[i].distanceToOrigin() > longestSide) {
                longestSide = distanceTo(vertices[i], vertices[i+1]);
            }
        }
        return longestSide;
    }

    /**
     * Moves all points in the polygon by the vector <deltaX, deltaY>.
     * @param deltaX
     * @param deltaY
     */
    public void move(double deltaX, double deltaY) {
        for (int i = 0; i < vertices.length; i++) {
            vertices[i].move(deltaX, deltaY);
        }
    }

    /**
     * Check if the polygon is a triangle.
     * @return
     */
    public boolean isTriange(){
        return vertices.length == 3;
    }

    /**
     * Check if the polygon is a rectangle.
     * @return
     */
    public boolean isRectangle(){
        return vertices.length == 4;
    }

    /**
     * Returns the id of the polygon.
     * @return
     */
    public int id() {
        return this.hashCode();
    }

    /**
     * Returns a clone of the polygon.
     * @return
     */
    public Polygon clone() {
        Point2D[] result = new Point2D[vertices.length];
        for (int i = 0; i < vertices.length; i++) {
            result[i] = vertices[i].clone();
        }
        return new Polygon(result);
    }

    /**
     * Returns a string representation of the polygon.
     * @return
     */
    public String PolyToString() {
        String result = "";
        for (int i = 0; i < vertices.length; i++) {
            if (i<vertices.length-1) {
                result += vertices[i].toString() + ", ";
            }
            else {
                result += vertices[i].toString();
            }
        }
        return result;
    }

    public static void drawSides(Image image, Polygon polygon, Color color) {
        // ligning for linje, y=a*x+b, gennem to kendte punkter
        // hvor a = y2-y1/x2-x1
        int y;double a;double b;
        double x1;double x2;
        double y1;double y2;
        int width = image.width();
        int height = image.height();
        for (int v= 0; v < polygon.vertices.length; v++) {
            x1 = polygon.vertices[v].getx();
            x2 = polygon.vertices[(v + 1) % polygon.vertices.length].getx();
            y1 = polygon.vertices[v].gety();
            y2 = polygon.vertices[(v + 1) % polygon.vertices.length].gety();
            if (x2 != x1) {
                a = (y2 - y1) / (x2 - x1);
                b = y1 - a * x1;
                //draw line
                for (int x = (int) Math.min(x1, x2); x < Math.max(x1, x2); x++) {
                    y = (int) (a * x + b);
                    if (x >= 0 && x < width && y >= 0 && y < height) {
                        image.setPixel(x, y, color);
                    }
                }
            } else {
                for (int i = (int) Math.min(y1, y2); i < Math.max(y1, y2); i++) {
                    if (x1 >= 0 && x1 < width && i >= 0 && i < height) {
                        image.setPixel((int) x1, i, color);
                    }
                }
            }
        }
    }

    public void drawPolygon(Image image, Color color) {
        drawSides(image, this, color);
        drawVertices(image, this, color);
    }

    public static void enhanceVertices(Image image, Polygon p, Color color) {
        for (int v = 0; v < p.vertices.length; v++) {
            int xStart = (int) p.vertices[v].getx() - 1;
            int yStart = (int) p.vertices[v].gety() - 1;
            for (int x = xStart; x <= xStart + 2; x++) {
                for (int y = yStart; y <= yStart + 2; y++) {
                    if (x == p.vertices[v].getx() && y == p.vertices[v].gety()) {
                        continue;
                    }
                    if (x >= 0 && x < image.width() && y >= 0 && y < image.height()) {
                        image.setPixel(x, y, color);
                    }
                }
            }
        }
    }
}


