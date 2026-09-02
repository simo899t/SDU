// Source code is decompiled from a .class file using FernFlower decompiler.
package Exercise3;
public class Color {
    private final int rgb;
    public static final Color BLACK = new Color(0, 0, 0);
    public static final Color WHITE = new Color(255, 255, 255);
    public static final Color GRAY = new Color(128, 128, 128);
    public static final Color RED = new Color(255, 0, 0);
    public static final Color MAROON = new Color(128, 0, 0);
    public static final Color LIME = new Color(0, 255, 0);
    public static final Color GREEN = new Color(0, 128, 0);
    public static final Color BLUE = new Color(0, 0, 255);
    public static final Color NAVY = new Color(0, 0, 128);
    public static final Color YELLOW = new Color(255, 255, 0);
    public static final Color MAGENTA = new Color(255, 0, 255);
    public static final Color CYAN = new Color(0, 255, 255);
    public static final Color PINK = new Color(255, 192, 203);
    public static final Color ORANGE = new Color(255, 165, 0);
 
    public Color(int var1) {
       assert 0 <= var1 && var1 <= 16777215;
 
       this.rgb = var1;
    }
 
    public Color(int var1, int var2, int var3) {
       assert 0 <= var1 && var1 <= 255;
 
       assert 0 <= var2 && var2 <= 255;
 
       assert 0 <= var3 && var3 <= 255;
 
       this.rgb = var1 << 16 | var2 << 8 | var3;
    }
 
    public int red() {
       return this.rgb >> 16 & 255;
    }
 
    public int green() {
       return this.rgb >> 8 & 255;
    }
 
    public int blue() {
       return this.rgb & 255;
    }
 
    public int rgb() {
       return this.rgb;
    }
 
    public String toString() {
       return String.format("RGB(%3d,%3d,%3d)", this.red(), this.green(), this.blue());
    }
 
    public int hashCode() {
       return this.rgb;
    }
 
    public boolean equals(Object var1) {
       if (var1 != null && var1 instanceof Color) {
          return this.rgb == ((Color)var1).rgb;
       } else {
          return false;
       }
    }
 }
 