// Source code is decompiled from a .class file using FernFlower decompiler.
package Exercise3;
import java.awt.Component;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JOptionPane;

public class Image {
   private final Color[][] pixels;
   private final int width;
   private final int height;

   public Image(int var1, int var2) {
      this(var1, var2, Color.BLACK);
   }

   public Image(int var1, int var2, Color var3) {
      assert var1 >= 0;

      assert var2 >= 0;

      this.width = var1;
      this.height = var2;
      this.pixels = new Color[var1][var2];

      for(int var4 = 0; var4 < var1; ++var4) {
         for(int var5 = 0; var5 < var2; ++var5) {
            this.pixels[var4][var5] = var3;
         }
      }

   }

   public static Image fromFile(String var0) {
      try {
         BufferedImage var1 = ImageIO.read(new File(var0));
         Image var2 = new Image(var1.getWidth(), var1.getHeight());

         for(int var3 = 0; var3 < var2.width; ++var3) {
            for(int var4 = 0; var4 < var2.height; ++var4) {
               int var5 = var1.getRGB(var3, var4) & 16777215;
               var2.pixels[var3][var4] = new Color(var5);
            }
         }

         return var2;
      } catch (IOException var6) {
         return null;
      }
   }

   public int width() {
      return this.width;
   }

   public int height() {
      return this.height;
   }

   public Color pixel(int var1, int var2) {
      return this.pixels[var1][var2];
   }

   public void setPixel(int var1, int var2, Color var3) {
      this.pixels[var1][var2] = var3;
   }

   public void display() {
      BufferedImage var1 = new BufferedImage(this.width, this.height, 1);

      for(int var2 = 0; var2 < this.width; ++var2) {
         for(int var3 = 0; var3 < this.height; ++var3) {
            var1.setRGB(var2, var3, this.pixels[var2][var3].rgb());
         }
      }

      JOptionPane.showMessageDialog((Component)null, new ImageIcon(var1));
   }
}
