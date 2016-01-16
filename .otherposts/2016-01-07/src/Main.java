import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.List;

public class Main {
    private static final int x = 900, y = 1200, w = 400, h = 400;
    public static void main(String[] args) {
        new File("gen").mkdirs();
        BufferedImage[] cropped = Base
                .applyToAll(bi -> bi.getSubimage(x, y, w, h));
        StarIdentification a = new StarIdentification(Base.IMAGES[0], .2f);
        BufferedImage clustersWOriginal = Base.average(Base.IMAGES[0],
                a.coloredStars);
        StarIdentification b = new StarIdentification(Base.IMAGES[1], .2f);
        Base.write(clustersWOriginal.getSubimage(x, y, w, h),
                "gen/clusters.png");
        Base.write(
                Base.combine(
                        Base.layerMonochrome(Color.black, Color.red, Color.blue,
                                Color.white),
                        a.coloredStars.getSubimage(x, y, w, h),
                        b.coloredStars.getSubimage(x, y, w, h)),
                "gen/clusters-two.png");
        visualize(Base.IMAGES[0], Base.IMAGES[1], a.associate(b, 20, .7));
        Base.write(cropped[0], "gen/cluster-original.png");
        Base.write(Base.average(cropped), "gen/cluster-overlay.png");
    }
    public static void visualize(BufferedImage a, BufferedImage b,
            List<Coordinate<Star>> stars) {
        System.out.println(stars);
        BufferedImage comp = Base.average(a, b);
        Graphics g = comp.getGraphics();
        g.setColor(Color.red);
        for (Coordinate<Star> cs : stars) {
            g.drawLine((int) cs.x.x, (int) cs.x.y, (int) cs.y.x, (int) cs.y.y);
        }
        Base.write(comp.getSubimage(x, y, w, h), "gen/matchedStars.png");
    }
}
